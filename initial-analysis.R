
library(tidyverse)
library(readxl)


d <- read_excel('data/20200505-base-datos-erick.xlsx') %>% 
  mutate(
    store_nbr = substr(key, 5, 8),
    key = paste(
      as.numeric(substr(key, 1, 4)),
      as.numeric(substr(key, 5, 8)),
      nomenclatura,
      str_replace_all(clave_gts, ' ', '_'),
      sep = '-'
    )
  )

mod <- read_excel('data/20200505-analisis-modificado.xlsx') %>% 
  mutate(
    store_nbr = substr(key, 5, 8)
  )


d %>% 
  filter(costo_por_km < 100) %>% 
  filter(whse_nbr == 9805) %>% 
  ggplot(aes(avg_km, costo_por_viaje)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ I(exp(-x))) +
  facet_grid(vars(whse_nbr), vars(clave_gts))



y ~ b2 * 1/(x-10) + b0

lm(costo_por_km ~ I(1/(avg_km - 10)), data = x)
lm(costo_por_km ~ I(exp(-0.01*avg_km - 10)), data = x)

ms <- d %>% 
  group_by(whse_nbr, clave_gts) %>% 
  nest() %>% 
  mutate(
    model = map(data, function(x){
      safely(nls)(
        costo_por_km ~ b0 + b1 * 1/(avg_km - b2),
        # costo_por_km ~ b0 + b1 * exp(-b3*avg_km - b2),
        x,
        start = list(b0 = 15, b1 = 1000, b2 = min(x$avg_km) - 1),
        # start = list(b0 = 15, b1 = 1000, b2 = 10, b3 = 0.0001),
        # lower = list(b0 = 0, b1 = 0, b2 = 0),
        # upper = list(b2 = min(x$avg_km))
        # algorithm = 'port'
      )$result
    }),
    data = map2(data, model, ~{
      if (!is.null(.y)) {
        .x$pred <- predict(.y, .x)
      }
      .x
    })
  )

pred <- ms %>% 
  select(-model) %>% 
  unnest(data) %>% 
  ungroup()

##### Resultados finales 
#Requiere la libreria FG
result <- pred %>% 
  mutate(
    grupo = paste(
      whse_nbr,
      str_replace_all(clave_gts, ' ', '_'),
      sep = '-'
    ),
    diff_vs_pred = costo_por_km - pred,
    diffp_vs_pred = diff_vs_pred / pred,
    gasto_pred = n_viajes * avg_km * pred,
    impacto_gasto = n_viajes * avg_km * diff_vs_pred,
    impacto_gasto_porc = impacto_gasto / gasto_pred,
    rank_impacto_gasto = row_number(desc(impacto_gasto))
  )

good_plot <- function(x) {
  x %>% 
    mutate(
      dot_color = ifelse(rank_impacto_gasto <= 15 & !is.na(rank_impacto_gasto), 'red', 'black'),
      # Valor dummy solo para que se pueda graficar
      impacto_gasto = ifelse(is.na(impacto_gasto), 1000, impacto_gasto)
    ) %>% 
    ggplot(aes(avg_km)) +
    geom_point(aes(y = costo_por_km, size = abs(impacto_gasto), color = dot_color), alpha = 0.4) +
    geom_line(aes(y = pred), color = 'blue') +
    facet_wrap(vars(whse_nbr, clave_gts), scales = 'free') + 
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
}

# Gráfica de top 15
interest_groups <- result %>% 
  filter(
    rank_impacto_gasto <= 15 & 
    !is.na(rank_impacto_gasto)
  ) %>% 
  pull(grupo) %>% 
  unique()

result %>% 
  filter(grupo %in% interest_groups) %>% 
  good_plot()
  # group_by(grupo) %>% 
  # summarise(
    # puntos_top = sum(ifelse(rank_impacto_gasto <= 15 & !is.na(rank_impacto_gasto), 1, 0))
  # ) %>% 
  # arrange(desc(puntos_top))

result %>% 
  filter(grupo %in% interest_groups) %>% 
  mutate(dot_color = ifelse(rank_impacto_gasto <= 15 & !is.na(rank_impacto_gasto), 'red', 'black')) %>% 
  ggplot(aes(avg_km)) + 
  geom_point(aes(y = costo_por_km, color = dot_color), alpha = 0.4) +
  facet_wrap(vars(whse_nbr, clave_gts), scales = 'free') + 
  scale_color_identity()

####### Graficas
result %>% 
  filter(!is.na(pred)) %>% 
  filter(whse_nbr %in% c(8033, 8034, 9804, 9805, 9810, 9429)) %>%
  good_plot()

# Validación de que no hayan cambiado los resultados
validation <- result %>% 
  inner_join(
    mod,
    by = c('nomenclatura', 'key')
  ) %>% 
  mutate(
    gasto_all_equal = 
      round(gasto_pred.x, 1) == round(gasto_pred.y, 1) | 
      (is.na(gasto_pred.x) & is.na(gasto_pred.y)),
    impacto_all_equal = 
      round(impacto_gasto.x, 1) == round(impacto_gasto.y, 1) | 
      (is.na(impacto_gasto.x) & is.na(impacto_gasto.y)),
    impactop_all_equal = 
      round(impacto_gasto_porc.x, 3) == round(impacto_gasto_porc.y, 3) | 
      (is.na(impacto_gasto_porc.x) & is.na(impacto_gasto_porc.y)),
    everything_equal = all(c(gasto_all_equal, impacto_all_equal, impactop_all_equal))
  )






