# Este código es para unir las bases de datos finales de pago por viaje y flota dedicada y así poder hacer una comparativa entre los dos esquemas.

library(tidyverse)
source('functions.R')

pre_pxv <- readRDS(sprintf('data/%s-pre_pxv', Sys.Date()))
pre_ded <- readRDS(sprintf('data/%s-pre_ded', Sys.Date()))
ded_pred <- readRDS(sprintf('data/%s-ded_pred', Sys.Date()))

# Comparativa de gráficas a nivel llave -----------------------------------

comparable_pxv_pred <- pre_pxv %>% 
  create_common_groups(ded) %>% 
  special_summarise(region, whse_nbr, store_nbr, transportista, equipment_type, tipo_remolque, capacidad_transporte, new_key, grupo_tractor, grupo_remolque, grupo_comparable) %>% 
  # El modelo debe ser al mismo nivel que el otro para que la gráfica por grupo comparable esté correcta.
  create_model(grupo_comparable) %>% 
  compare_prediction() %>% 
  mutate(esquema = 'pxv')

comparison <- comparable_pxv_pred %>% 
  bind_rows(mutate(ded_pred, esquema = 'ded'))

comparison %>% 
  filter(grupo_comparable %in% c('9805-TR_S_53')) %>% 
  ggplot(aes(avg_km)) + 
  geom_point(aes(y = costo_por_km, size = abs(impacto_gasto), color = esquema), alpha = 0.4) + 
  facet_wrap(vars(grupo_comparable), scales = 'free', nrow = 2) + 
  geom_line(aes(y = pred, color = esquema)) + 
  ylim(0, NA) + 
  theme_bw()


# Comparativa por tabla quitando transportista ----------------------------

joinable_ded_pred <- pre_ded %>% 
  special_summarise(region, whse_nbr, store_nbr, equipment_type, tipo_remolque, capacidad_transporte, grupo_tractor, grupo_remolque, grupo_comparable) %>% 
  mutate(joinable_key = paste(
    whse_nbr,
    store_nbr,
    str_replace_all(equipment_type, ' ', '_'),
    tipo_remolque,
    capacidad_transporte,
    sep = '-'
  )) %>% 
  create_model(grupo_comparable) %>% 
  compare_prediction()
  
joinable_pxv_pred <- pre_pxv %>% 
  # No hace falta que tengan los mismos grupos porque no van a graficarse, mientras estén agrupados al mismo nivel (joinable_key). Sin embargo, si los equipment types no son los mismos, sólo se unen la mitad de registros que cuando sí son iguales, por lo que prefiero que sean iguales.
  create_common_groups(joinable_ded_pred) %>%
  special_summarise(region, whse_nbr, store_nbr, equipment_type, tipo_remolque, capacidad_transporte, grupo_tractor, grupo_remolque, grupo_comparable) %>% 
  mutate(joinable_key = paste(
    whse_nbr,
    store_nbr,
    str_replace_all(equipment_type, ' ', '_'),
    tipo_remolque,
    capacidad_transporte,
    sep = '-'
  )) %>% 
  # No importa a qué grupo se haya hecho el modelo, porque las bases estarán al mismo nivel, en todo caso, si hacemos este por grupo de tractor original, representa que este modelo es más preciso que si se hiciera por grupo comparable, pero como estos grupos de tractores ya no son los originales sino los comunes, sí es necesario hacerlo por grupo comparable para no mezclar capacidades.
  create_model(grupo_comparable) %>% 
  compare_prediction()

joined_base <- joinable_ded_pred %>% 
  inner_join(
    joinable_pxv_pred,
    # Aunque la joinable key es suficiente para hacer el join, vamos a unir con todas las demás para evitar columnas duplicadas en el resultado.
    by = c('joinable_key', 'grupo_comparable', 'region', 'whse_nbr', 'store_nbr', 'equipment_type', 'tipo_remolque', 'capacidad_transporte', 'grupo_tractor', 'grupo_remolque'),
    suffix = c('_ded', '_pxv')
  ) %>% 
  select(
    joinable_key,
    region,
    whse_nbr,
    store_nbr,
    equipment_type,
    tipo_remolque,
    capacidad_transporte,
    starts_with('grupo'),
    starts_with('n_entregas'),
    starts_with('total_gasto'),
    starts_with('total_km'),
    starts_with('avg_km'),
    starts_with('costo_por_viaje'),
    starts_with('costo_por_km'),
    starts_with('pred'),
    # starts_with('diff_vs_pred'),
    # starts_with('impacto_gasto')
  ) %>% 
  mutate(
    total_km = total_km_pxv + total_km_ded,
    total_entregas = n_entregas_pxv + n_entregas_ded,
    costo_real = total_gasto_pxv + total_gasto_ded,
    costo_pxv = total_km * costo_por_km_pxv,
    costo_ded = total_km * costo_por_km_ded
  )
  # mutate(
  #   diff_costos_km = abs(costo_por_km_ded - costo_por_km_pxv),
  #   esquema_barato = ifelse(costo_por_km_ded > costo_por_km_pxv, 'pxv', 'ded'),
  #   diff_preds = pred_ded - pred_pxv,
  #   # Para que siempre se multiplique por la cantidad correcta de km para obtener la diferencia en el impacto.
  #   km_para_impacto = ifelse(esquema_barato == 'pxv', total_km_ded, total_km_pxv),
  #   impacto_gasto = diff_costos_km * km_para_impacto,
  #   # impacto_gasto_total = impacto_gasto_ded + impacto_gasto_pxv,
  #   rank_impacto_gasto = row_number(desc(impacto_gasto))
  # )


# Resúmenes y gráficas ----------------------------------------------------

# Resumen por grupo comparable
resumen_grupo <- joined_base %>% 
  group_by(grupo_comparable) %>% 
  summarise(
    across(any_of(c('total_km', 'total_entregas', 'costo_real', 'costo_pxv', 'costo_ded')), sum)
  ) %>% 
  mutate(
    across(
      starts_with('costo'), 
      list(
        'por_km' = ~.x / total_km,
        'por_entrega' = ~.x / total_entregas
      ),
      names = '{col}_{fn}'
    )
  )
  # summarise(
  #   n_tiendas = n(),
  #   avg_diff_costo = mean(diff_costos_km),
  #   avg_diff_pred = mean(diff_preds),
  #   # Cuidado, aquí se promedian km de diferentes tiendas, es sólo de referencia
  #   avg_km = mean(km_para_impacto),
  #   total_km = sum(km_para_impacto),
  #   impacto = sum(impacto_gasto)
  # )

# Gráfica comparativa de costos por km 
resumen_grupo %>% 
  pivot_longer(
    ends_with('por_km'), 
    names_to = 'tipo_costo',
    names_prefix = 'costo_',
    values_to = 'costo_por_km'
  ) %>% 
  mutate(tipo_costo = str_extract(tipo_costo, '.*(?=_por)')) %>% 
  ggplot(aes(x = grupo_comparable)) + 
  geom_point(aes(y = costo_por_km, color = tipo_costo)) + 
  ggthemes::scale_color_colorblind() + 
  coord_flip()

# Gráfica para ver cantidad de tiendas en las que es más barato cada esquema
resumen_grupo %>% 
  ggplot(aes(
    x = esquema_barato, 
    y = n_tiendas, 
    # y = impacto, 
    group = grupo_comparable
  )) + 
  geom_col(
    aes(fill = impacto),
    # aes(fill = n_tiendas),
    position = 'dodge'
  ) + 
  ylim(
    -max(resumen_grupo$n_tiendas) * 0.25, 
    max(resumen_grupo$n_tiendas) * 1.15
    # -max(resumen_grupo$impacto) * 0.25, 
    # max(resumen_grupo$impacto) * 1.10
  ) + 
  geom_text(
    aes(y = 0, label = grupo_comparable, angle = 90),
    position = position_dodge(0.9),
    hjust = 1.1
  ) + 
  geom_text(
    aes(
      label = sprintf('%skm, $%d', scales::comma(round(total_km), accuracy = 1), round(avg_diff_costo)),
      angle = 90
    ),
    position = position_dodge(0.9),
    # vjust = -0.5
    hjust = -0.1
  ) + 
  scale_fill_viridis_c(name = "Impacto en Pesos", labels = scales::comma) + 
  labs(
    title = 'Cantidad de tiendas por esquema más barato',
    x = 'Esquema más barato',
    y = 'Número de tiendas'
  ) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 15)
  )

joined_base %>% 
  arrange(rank_impacto_gasto) %>% 
  write_csv('output/20200714-joined_base.csv')


# Costo promedio por CD-tienda --------------------------------------------

pretty_names <- c(
  'CD transportes' = 'whse_nbr',
  'Tienda' = 'store_nbr',
  'Determinante' = 'whse_nbr2',
  # 'Camión' = 'equipment_type',
  # 'Transportista' = 'transportista',
  # 'Tipo remolque' = 'tipo_remolque',
  # 'Capacidad' = 'capacidad_transporte',
  'Mes' = 'month',
  'Esquema incluido' = 'esquema_incluido',
  'Transporte mas comun' = 'moda_transporte',
  'Entregas dedicada' = 'n_entregas_ded',
  # 'Gasto SPT dedicada' = 'gasto_spt',
  # 'Gasto tractor dedicada' = 'gasto_tractor',
  # 'Gasto remolque dedicada' = 'gasto_remolques',
  # 'Gasto casetas dedicada' = 'gasto_casetas',
  'Gasto total dedicada' = 'total_gasto_ded',
  'Km prom dedicada' = 'avg_km_ded',
  'Total km dedicada' = 'total_km_ded',
  'Costo por viaje dedicada' = 'costo_por_viaje_ded',
  'Costo por km dedicada' = 'costo_por_km_ded',
  'Entregas pago por viaje' = 'n_entregas_pxv',
  'Gasto pago por viaje' = 'total_gasto_pxv',
  'Km prom pago por viaje' = 'avg_km_pxv',
  'Total km pago por viaje' = 'total_km_pxv',
  'Costo por viaje pago por viaje' = 'costo_por_viaje_pxv',
  'Costo por km pago por viaje' = 'costo_por_km_pxv',
  'Total entregas' = 'n_entregas',
  'Gasto total' = 'total_gasto',
  'Total km' = 'total_km',
  'Costo por viaje total' = 'costo_por_viaje_calc',
  'Costo por km total' = 'costo_por_km_calc'
)

det_dict <- read_csv('data/20200724-dc-dictionary.csv') %>% 
  select(transportes, whse_nbr2) %>% 
  mutate(whse_nbr2 = ifelse(whse_nbr2 == 0, NA, whse_nbr2))

# keys <- syms(c('whse_nbr', 'store_nbr', 'equipment_type', 'transportista',  'tipo_remolque', 'capacidad_transporte'))
# keys <- syms(c('whse_nbr', 'store_nbr', 'equipment_type', 'transportista'))
keys <- syms(c('whse_nbr', 'store_nbr'))

process1 <- function(data, keys){
  data %>% 
    # filter(
    #   region == 'VILLAHERMOSA', 
    #   tipo_remolque != 'R' | is.na(tipo_remolque), 
    #   !str_detect(equipment_type, ' R ')
    # ) %>% 
    special_summarise(!!!keys)
}
process2 <- function(data, keys){
  data %>% 
    group_by(!!!keys) %>% 
    summarise(
      month = first(month),
      n_entregas = n(),
      gasto_spt = sum(costo_total_spt),
      gasto_tractor = sum(trip_tractor_cost),
      gasto_remolques = sum(trip_remolque_cost),
      gasto_casetas = sum(total_casetas),
      total_gasto = sum(total_cost),
      avg_km = mean(km_ida_viaje),
      total_km = sum(km_ida_viaje),
      .groups = 'drop'
    ) %>% 
    mutate(
      costo_por_viaje = total_gasto / n_entregas,
      costo_por_km = total_gasto / total_km
    )
}

complete_base <- pre_ded %>% 
  # rename(
  #   'equipment_type_nuevo' = 'equipment_type',
  #   'equipment_type' = 'equipment_type_anterior'
  # ) %>%
  process1(keys) %>% 
  full_join(
    y = process1(pre_pxv, keys),
    by = as.character(keys),
    suffix = c('_ded', '_pxv')
  ) %>% 
  mutate(
    month = coalesce(month_ded, month_pxv),
    esquema_incluido = case_when(
      is.na(total_gasto_ded) ~ 'pxv',
      is.na(total_gasto_pxv) ~ 'ded',
      TRUE ~ 'ambos'
    ),
    across(
      # .cols = !tipo_remolque, 
      .fns = ~ifelse(is.na(.x), 0, .x)
    ),
    n_entregas  = n_entregas_ded + n_entregas_pxv,
    total_gasto = total_gasto_ded + total_gasto_pxv,
    total_km = total_km_ded + total_km_pxv,
    costo_por_viaje_calc = total_gasto / n_entregas,
    costo_por_km_calc = total_gasto / total_km
  ) %>% 
  select(-starts_with('n_viajes'), -starts_with('month_')) %>% 
  left_join(det_dict, by = c('whse_nbr' = 'transportes'))

complete_base %>% 
  select(pretty_names) %>% 
  write_excel_csv(sprintf('output/20200805-cd_tienda-%s.csv', unique(complete_base$month)), na = '')

consolidado <- list.files('output/consolidado/', full.names = TRUE, pattern = '-base-') %>% 
  map(read_csv) %>% 
  bind_rows() %>% 
  set_names(names(complete_base)) %>% 
  mutate(
    month_order = case_when(
      month == 'feb' ~ 2,
      month == 'mar' ~ 3,
      month == 'apr' ~ 4,
      month == 'may' ~ 5,
      month == 'jun' ~ 6
    )
    # tipo_remolque = ifelse(tipo_remolque == 0, NA, tipo_remolque)
  )

consolidado %>% 
  arrange(!!!keys, month_order) %>% 
  select(!!!keys, month, esquema_incluido, everything(), -month_order) %>% 
  rename(pretty_names) %>% 
  write_excel_csv('output/consolidado/20200804-consolidado-feb_jun.csv', na = 'NA', append = FALSE)
