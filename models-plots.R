
library(tidyverse)
source('functions.R')

# Modelo PxV ------------------------------------------------------------------
pxv <- readRDS(sprintf('data/%s-pxv', Sys.Date()))

pxv_model <- create_model(pxv, grupo_tractor)
pxv_pred <- compare_prediction(pxv_model)

pxv_pred %>% 
  saveRDS(sprintf('data/%s-pxv_pred', Sys.Date()))

# Grafica PxV -------------------------------------------------------------
# Gráfica de costo/km
pxv_pred %>% 
  filter(
    !is.na(pred) & 
      whse_nbr %in% c(8033, 8034, 9804, 9805, 9810, 9429)
  ) %>% 
  good_plot()

# Gráfica de costo total
pxv_pred %>% 
  filter(
    !is.na(pred) & 
      whse_nbr %in% c(8033, 8034, 9804, 9805, 9810, 9429)
  ) %>% 
  mutate(dot_color = ifelse(rank_impacto_gasto <= 15, 'red', 'black')) %>% 
  ggplot(aes(avg_km)) +
  geom_point(aes(y = total_gasto / n_viajes, size = abs(impacto_gasto), color = dot_color), alpha = 0.4) +
  geom_line(aes(y = gasto_pred / n_viajes), color = 'blue') +
  facet_wrap(vars(whse_nbr, equipment_type), scales = 'free') +
  ylim(0, NA) + 
  labs(
    title = 'Tarifas vs la tendencia por CD-tipo de transporte',
    x = 'Kilómetros promedio por viaje',
    y = 'Costo total por viaje'
  ) + 
  scale_color_identity() + 
  scale_size_continuous(name = "Impacto en Pesos\n(absoluto)", labels = scales::comma) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 15)
  )

# Gráfica de top 15
interest_groups <- pxv_pred %>% 
  filter(
    rank_impacto_gasto <= 15 & 
      !is.na(rank_impacto_gasto)
  ) %>% 
  pull(grupo_tractor) %>% 
  unique()

pxv_pred %>% 
  filter(grupo_tractor %in% interest_groups) %>% 
  good_plot()
# group_by(grupo_tractor) %>%
# summarise(
#   puntos_top = sum(ifelse(rank_impacto_gasto <= 15 & !is.na(rank_impacto_gasto), 1, 0))
# ) %>%
# arrange(desc(puntos_top))


# Modelo dedicado ---------------------------------------------------------
ded <- readRDS(sprintf('data/%s-ded', Sys.Date()))

ded_model <- create_model(ded, grupo_tractor)
ded_pred <- compare_prediction(ded_model)

# Este se necesita para la comparativa
ded_pred %>% 
  saveRDS(sprintf('data/%s-ded_pred', Sys.Date()))

# Gráfica flota dedicada --------------------------------------------------
ded_pred %>% 
  filter(
    # grupo_tractor %in% (
    #   ded_pred %>%
    #     arrange(rank_impacto_gasto) %>%
    #     pull(grupo_tractor) %>%
    #     unique() %>%
    #     head(16) %>% 
    #     tail(16)
    # )
    grupo_tractor %in% interest_groups
    # grupo_tractor == '9428-SENCILLO'
  ) %>%
  good_plot()

ded_pred %>% 
  # filter(
  #   grupo_tractor %in% (ded %>%
  #     count(grupo_tractor) %>%
  #     arrange(desc(n)) %>%
  #     head(16) %>%
  #     pull(grupo_tractor))
  # ) %>% 
  filter(
    grupo_tractor == '9804-AR_S_48'
    # transportista == 'LBOSTD' # 'ESHMTD' # 'TUMOTD'
  ) %>% 
  mutate(
    # dot_color = ifelse(new_key %in% outliers, 'red', 'black')
    # dot_color = ifelse(avg_km >= 6 & costo_por_km >= 130, 'red', 'black'),
    dot_color = ifelse(rank_impacto_gasto <= 15 & !is.na(rank_impacto_gasto), 'red', 'black'),
    impacto_gasto = ifelse(is.na(impacto_gasto), 1000, impacto_gasto)
  ) %>% 
  ggplot(aes(avg_km)) +
  geom_point(aes(y = costo_por_km, size = n_viajes, color = transportista), alpha = 0.4) +
  geom_line(aes(y = pred), color = 'blue') +
  facet_wrap(vars(grupo_tractor), scales = 'free', nrow = 2) + 
  ylim(0, NA) + 
  labs(
    title = 'Rutas de flota dedicada por CD-Camión',
    x = 'Kilómetros promedio por viaje',
    y = 'Costo por kilómetro'
  ) + 
  # scale_color_identity() +
  scale_size_continuous(name = "Impacto (abs)", labels = scales::comma) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 15)
  )

# Output ------------------------------------------------------------------

pxv_pred %>% 
  arrange(rank_impacto_gasto) %>% 
  select(
    key,
    whse_nbr,
    region,
    store_nbr,
    transportista,
    equipment_type,
    whse_name,
    ciudad_destino,
    ruta_interes,
    all_store_nbrs,
    n_viajes,
    total_gasto,
    avg_km,
    total_km,
    costo_por_viaje,
    costo_por_km,
    pred,
    diff_vs_pred,
    diffp_vs_pred,
    gasto_pred,
    impacto_gasto
    # impacto_gasto_porc
  ) %>% 
  write_excel_csv(sprintf('output/%s-pxv_pred.csv', Sys.Date()), na = '')

ded_pred %>% 
  arrange(rank_impacto_gasto) %>% 
  select(-rank_impacto_gasto) %>% 
  write_excel_csv(sprintf('output/%s_ded-pred.csv', Sys.Date()), na = '')
