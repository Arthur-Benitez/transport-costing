
# Para conocer el número de dígitos de un número
digits <- function(x){
  floor(log10(abs(x))) + 1
} 

# Para obtener la moda de un vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

str_clean <- function(string) {
  unwanted <- '(SA P I|SAPI|SRLCV|SACV|SA|DE|CV|S|RL|INC|Y|CIA|NC)'
  string %>% 
    str_to_upper() %>% 
    str_squish() %>% 
    str_remove_all(sprintf('(?<=\\s)%s(?=\\s)', unwanted)) %>% 
    str_remove_all(sprintf('%s$', unwanted)) %>% 
    str_squish() %>% 
    str_replace_all('\\s', '_')
}

min_greater <- function(x, array){
  array <- sort(array)
  res <- rep_len(0, length(x))
  for (i in 1:length(x)){
    res[i] <- detect(array, ~.x >= x[i], .default = array[length(array)])
  }
  res
}

# Función para crear las variables estándar de interés en las bases resumidas de pxv y ded.
special_summarise <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    summarise(
      month = first(month),
      moda_transporte = getmode(equipment_type),
      n_entregas = n(),
      n_viajes = sum(n_viajes),
      n_remolques = sum(n_remolques),
      total_gasto = sum(total_cost_teorico),
      avg_km_entrega = mean(km_ida_viaje),
      avg_km_viaje = sum(km_ida_viaje) / n_viajes,
      total_km_entrega = sum(km_ida_viaje),
      .groups = 'drop'
    ) %>% 
    mutate(
      costo_por_viaje = total_gasto / n_viajes,
      costo_por_entrega = total_gasto / n_entregas,
      costo_por_km = total_gasto / total_km_entrega
    )
}

create_model <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    nest() %>% 
    mutate(
      model = map(data, function(x){
        safely(MASS::rlm)(
          costo_por_km ~ I(1/avg_km_entrega),
          data = x,
          psi = MASS::psi.huber
        )$result
      }),
      data = map2(data, model, ~{
        if (!is.null(.y)) {
          .x$pred <- predict(.y, .x)
          # .x$intercept <- .y$coefficients[1]
          # .x$slope <- .y$coefficients[2]
          # .x$residual_weight <- .y$weights
        }
        .x
      })
    )
}

compare_prediction <- function(data) {
  data %>% 
    select(-model) %>% 
    unnest(data) %>% 
    ungroup() %>% 
    mutate(
      diff_vs_pred = costo_por_km - pred,
      diffp_vs_pred = diff_vs_pred / pred,
      gasto_pred = total_km_entrega * pred,
      impacto_gasto = total_km_entrega * diff_vs_pred,
      rank_impacto_gasto = row_number(desc(impacto_gasto))
    )
}

# Función genérica de la gráfica que queremos tener para los análisis
good_plot <- function(x) {
  gp <- x %>% 
    mutate(
      dot_color = ifelse(rank_impacto_gasto <= 15 & !is.na(rank_impacto_gasto), 'red', 'black'),
      # Valor dummy solo para que se pueda graficar
      impacto_gasto = ifelse(is.na(impacto_gasto), 1000, impacto_gasto)
    ) %>% 
    ggplot(aes(avg_km_entrega)) +
    geom_point(aes(y = costo_por_km, size = abs(impacto_gasto), color = dot_color), alpha = 0.4) +
    geom_line(aes(y = pred), color = 'blue') + 
    ylim(0, NA) + 
    labs(
      title = 'Tarifas vs la tendencia por CD-tipo de transporte',
      x = 'Kilómetros promedio por viaje',
      y = 'Costo por kilómetro'
    ) + 
    scale_color_identity() + 
    scale_size_continuous(name = "Impacto en Pesos\n(absoluto)", labels = scales::comma) + 
    theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 15)
    )
  if('grupo_remolque' %in% names(x)){
    gp + 
      facet_wrap(vars(grupo_comparable), scales = 'free')
  } else {
    gp + 
      facet_wrap(vars(grupo_tractor), scales = 'free')
  }
}

create_common_groups <- function(data, target_data, all_groups = TRUE) {
  data <- data %>% 
    rename(c(
      'grupo_tractor_anterior' = 'grupo_tractor',
      'equipment_type_anterior' = 'equipment_type'
    )) %>% 
    mutate(
      equipment_type = case_when(
        grupo_tractor_anterior %in% (
          target_data %>% 
            pull(grupo_tractor) %>% 
            unique()
        ) ~ equipment_type_anterior,
        str_detect(equipment_type_anterior, 'ca|CA') ~ 'CAMIONETA',
        str_detect(equipment_type_anterior, 'full|FULL|F') ~ 'FULL',
        str_detect(equipment_type_anterior, 'th|TH') ~ 'THORTON',
        TRUE ~ 'SENCILLO'
      ),
      grupo_tractor = paste(
        whse_nbr,
        str_replace_all(equipment_type, ' ', '_'),
        sep = '-'
      )
    )
  if(all_groups) {
    data %>% 
      mutate(
        grupo_comparable = case_when(
          equipment_type %in% c('SENCILLO', 'FULL', 'THORTON', 'CAMIONETA') ~ paste(
            grupo_tractor,
            tipo_remolque,
            capacidad_transporte,
            sep = '-'
          ),
          TRUE ~ grupo_tractor
        ),
        new_key = paste(
          whse_nbr,
          store_nbr,
          transportista,
          str_replace_all(equipment_type, ' ', '_'),
          tipo_remolque,
          capacidad_transporte,
          sep = '-'
        )
      )
  } else {
    data
  }
}

create_costs_base <- function(data, grouping_vars, add_region = TRUE){
  if (add_region) {
    grouping_vars <- append(grouping_vars, sym('region'), after = 0)
  }
  res <- data %>% 
    filter(!is.na(region), !str_detect(equipment_type, 'AR|RA'), !ruteo) %>% 
    special_summarise(!!!grouping_vars, capacidad_transporte) %>% 
    mutate(
      original_equipment_type = equipment_type,
      tipo_remolque = str_extract(equipment_type, '(?<=\\s).+(?=\\s)'),
      capacidad_tarimas = case_when(
        capacidad_transporte == 28 ~ 14,
        capacidad_transporte == 80 ~ 44,
        str_detect(equipment_type, '40F$') ~ 44,
        TRUE ~ 2 * floor(capacidad_transporte * 0.3048 / 1.03) 
      ),
      equipment_type = str_replace_all(equipment_type, ' ', '_'),
      key = paste(!!!grouping_vars, sep = '-')
    ) %>% 
    select(-equipment_type) %>% 
    rename(c('equipment_type' = 'original_equipment_type')) %>% 
    select(key, as.character(grouping_vars), capacidad_transporte, tipo_remolque, capacidad_tarimas, everything())
  
  if ('whse_nbr' %in% grouping_vars) {
    res %>% 
      left_join(det_dict, by = c('whse_nbr' = 'transportes')) %>% 
      relocate(whse_nbr2, .after = whse_nbr)
  } else {
    res
  }
}

