
# Código para generar una base con las tarifas válidas a diferentes niveles de agrupamiento

library(tidyverse)
source('functions.R')

# Assumptions
# Usar Cajas incompletas
# Camion por tienda con >0 cajas
# Costos "promedio"
# Ocupacion max = 100%


# Costos de transporte a diferentes niveles -------------------------------

store_state <- read_csv('data/20200812-store_state.csv')

dc_store_base <- pre_pxv %>% 
  create_costs_base(syms(c('whse_nbr', 'store_nbr', 'equipment_type')))

region_base <- pre_pxv %>% 
  create_costs_base(syms(c('equipment_type')))

# Faltan los estados de los SAMs
state_base <- pre_pxv %>% 
  inner_join(store_state, by = 'store_nbr') %>% 
  create_costs_base(syms(c('state_code', 'equipment_type')), add_region = FALSE)

total_base <- pre_pxv %>% 
  create_costs_base(syms(c('equipment_type')), add_region = FALSE)

dc_store_base %>% 
  write_csv('output/20200810-whse_store_truck-cost.csv')
state_base %>% 
  write_csv('output/20200812-state_truck-cost.csv')
total_base %>% 
  write_csv('output/20200811-truck-cost.csv')


# Costos de desconsolidador -----------------------------------------------

decon_cost <- read_csv('data/20200813-costos_desconsolidador.csv')
decon_states <- read_csv('data/20200813-estados_desconsolidador.csv') %>% 
  # Override manual
  filter(desconsolidador != 'San Martin Obispo')

decon_state_truck_view <- decon_states %>% 
  inner_join(decon_cost, by = 'desconsolidador') %>% 
  # mutate(
  #   camion = ifelse(camion == 'TR S 40', 'TR S 40F', camion)
  # ) %>% 
  group_by(desconsolidador, camion) %>% 
  mutate(
    across(c('costo_total', 'n_viajes'), ~.x/n(), .names = 'estado_{col}')
  ) %>% 
  ungroup()

state_truck_view <- a %>% 
  group_by(estado_spt, estado_teradata, camion) %>% 
  summarise(
    across(c('estado_costo_total', 'estado_n_viajes'), sum),
    .groups = 'drop'
  ) %>% 
  mutate(costo_por_viaje = estado_costo_total / estado_n_viajes)

truck_view <- a %>% 
  group_by(camion) %>% 
  summarise(
    across(c('estado_costo_total', 'estado_n_viajes'), sum),
    .groups = 'drop'
  ) %>% 
  mutate(costo_por_viaje = estado_costo_total / estado_n_viajes)


# Cantidad de entregas promedio por estado --------------------------------

state_deliveries <- processed_data %>% 
  inner_join(store_state, by = 'store_nbr') %>% 
  # count(state_name)
  group_by(id_viaje) %>% 
  summarise(
    across(c('entregas_viaje', 'state_name', 'state_code'), getmode),
    .groups = 'drop'
  ) %>% 
  group_by(state_name, state_code) %>% 
  summarise(n_entregas_viaje = mean(entregas_viaje), .groups = 'drop')


# Traducción de modelo de Excel de costo por caja -------------------------

# Bases independientes de los proveedores (bases de costos)
n_decon_states <- decon_states %>% 
  group_by(estado_teradata) %>% 
  summarise(n_desconsolidadores = n(), .groups = 'drop')

state_truck_costs <- state_base %>% 
  filter(tipo_remolque == 'S') %>% 
  group_by(state_code, capacidad_tarimas) %>% 
  summarise(
    across(c('total_gasto', 'n_entregas'), sum),
    equipment_type = first(equipment_type),
    .groups = 'drop'
  ) %>% 
  mutate(
    costo_por_viaje = total_gasto / n_entregas
  )

state_avg_costs <- total_base %>% 
  filter(tipo_remolque == 'S') %>% 
  group_by(capacidad_tarimas) %>% 
  summarise(
    camion = first(sort(equipment_type)),
    across(c('total_gasto', 'n_entregas'), sum),
    .groups = 'drop') %>% 
  mutate(
    costo_por_viaje = total_gasto / n_entregas
  )

decon_trucks_costs <- decon_cost %>% 
  mutate(
    capacidad_transporte = as.numeric(str_extract(camion, '\\d+')),
    # Override manual
    capacidad_tarimas = case_when(
      capacidad_transporte == 28 ~ 14,
      capacidad_transporte == 80 ~ 44,
      str_detect(camion, '40F$') ~ 44,
      TRUE ~ 2 * floor(capacidad_transporte * 0.3048 / 1.03) 
    ),
    costo_por_viaje = costo_total / n_viajes,
    # Override manual
    costo_por_viaje = ifelse(costo_por_viaje < 1000, 20147, costo_por_viaje)
  )

decon_avg_costs <- decon_trucks_costs %>% 
  group_by(camion, capacidad_tarimas) %>% 
  summarise(across(c('costo_total', 'n_viajes'), sum), .groups = 'drop') %>% 
  mutate(
    costo_por_viaje = costo_total / n_viajes,
    # Override manual
    costo_por_viaje = ifelse(costo_por_viaje < 1000, 20147, costo_por_viaje)
  )

# Bases dependientes por proveedor
corrective_vendor_names <- tibble(
  old_name = c('GANAD_PROD_LECHE_PURA', 'ESSITY_HIGIENE_SALUD_MX', 'GENOMMA_LAB_INTERNACNL_B'),
  new_name = c('GANAD_PROD_LEC_PURA', 'ESSITY_HIG_SALU_MEX', 'GENOMMA_LAB_INT_SAB')
)
query_ch <- RODBC::odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WM3;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", paste0('a0b01eb', '@@', rstudioapi::askForPassword("Teradata password"))))
state_cases_query <- readLines('state-cases.sql')

vendor_nbrs <- read_csv('data/costo_caja/20200820-top_vendors.csv') %>% 
  mutate(across(c('vendor_name', 'vendor_rfc'), str_clean, .names = '{col}_clean'))
vendor_pallet_cases <- read_csv('data/costo_caja/20200820-top_vendors_pallet_cases.csv') %>% 
  mutate(across(c('vendor_name', 'vendor_rfc'), str_clean, .names = '{col}_clean'))

prepared_state_cases_query <- state_cases_query %>% 
  str_replace(
    '\\?VENDOR_NBRS', 
    paste(
      vendor_nbrs %>% 
        # Es mejor filtrar por vendor_rfc_clean
        # filter(str_detect(vendor_name_clean, 'HERDEZ|VALLE')) %>% 
        pull(vendor_nbr) %>% 
        unique(),
      collapse = ','
    )
  ) %>% 
  str_subset('^\\s*--', negate = TRUE) %>%  #quitar lineas de comentarios
  stringi::stri_trans_general('ASCII') %>% # quitar no ASCII 
  paste(collapse = '\n')

prepared_state_cases_query %>% 
  RODBC::sqlQuery(channel = query_ch) %>% 
  as_tibble() %>% 
  set_names(tolower(names(.))) %>% 
  saveRDS(sprintf('data/%s-query_state_volume', Sys.Date()))

query_state_volume <- readRDS(sprintf('data/%s-query_state_volume', '2020-09-03')) %>% 
  mutate(
    across(where(is.factor), as.character),
    vendor_name_clean = str_clean(vendor_name),
    pallets = ifelse(pallets == 0, NA, pallets),
    pallet_cases = cases / pallets
  ) %>% 
  left_join(corrective_vendor_names, by = c('vendor_name_clean' = 'old_name')) %>% 
  mutate(vendor_name_clean = coalesce(new_name, vendor_name_clean)) %>% 
  select(-new_name)

vendor_needed_correction <- query_state_volume %>% 
  # filter(!is.na(cases), !is.na(pallets)) %>% 
  group_by(vendor_name_clean, state_code) %>% 
  summarise(across(c('cases', 'pallets'), sum), .groups = 'drop') %>% 
  mutate(pallet_cases = cases / pallets) %>% 
  left_join(
    select(vendor_pallet_cases, -vendor_name), 
    by = 'vendor_name_clean',
    suffix = c('_original', '_real')
  ) %>% 
  mutate(
    # desv = abs(pallet_cases_original - pallet_cases_real),
    # desv2 = desv / pallet_cases_real,
    requires_new_pallets = ifelse(abs(pallet_cases_original - pallet_cases_real) / pallet_cases_real > 0.2 | is.na(pallet_cases_original), TRUE, FALSE)
  )

state_volume <- query_state_volume %>% 
  left_join(
    vendor_needed_correction %>% 
      select(
        vendor_name_clean, 
        vendor_rfc_clean, 
        state_code,
        starts_with('pallet_cases'), 
        requires_new_pallets
      ), 
    by = c('vendor_name_clean', 'state_code')
  ) %>% 
  mutate(
    pallet_cases = ifelse(
      requires_new_pallets,
      pallet_cases_real, 
      pallet_cases_original
    ),
    pallets = cases / pallet_cases
  ) %>% 
  group_by(vendor_rfc_clean, state_name, state_code) %>% 
  summarise(
    across(c(starts_with('vendor'), n_tiendas), first),
    across(c('cases', 'pallets', 'requires_new_pallets'), sum),
    .groups = 'drop'
  ) %>% 
  mutate(pallet_cases = cases / pallets) %>% 
  select(
    starts_with('vendor'), 
    starts_with('state'),
    n_tiendas,
    cases,
    pallets,
    pallet_cases,
    everything(), 
  )

# Intermedios
decon_div <- state_volume %>% 
  rename('estado_teradata' = 'state_code') %>% 
  inner_join(n_decon_states, by = 'estado_teradata') %>% 
  mutate(across(c('cases', 'pallets'), ~.x / n_desconsolidadores, .names = 'route_{col}')) %>% 
  select(vendor_rfc_clean, estado_teradata, pallet_cases, starts_with('route_'))

decon_state_dis <- decon_states %>% 
  left_join(decon_div, by = 'estado_teradata')

# Resumenes
decon_summary <- decon_state_dis %>% 
  group_by(vendor_rfc_clean, desconsolidador) %>% 
  summarise(across(starts_with('route'), sum), .groups = 'drop') %>% 
  mutate(
    pallet_cases = cases / pallets,
    truck_pallets1 = min_greater(pallets, decon_avg_costs$capacidad_tarimas),
    truck_cases1 = truck_pallets1 * pallet_cases,
    n_camiones1 = floor(pallets / truck_pallets1),
    remain_pallets = pallets - n_camiones1 * truck_pallets1,
    truck_pallets2 = min_greater(remain_pallets, decon_avg_costs$capacidad_tarimas),
    truck_cases2 = remain_pallets * pallet_cases,
    n_camiones2 = ceiling(remain_pallets / truck_pallets2)
  ) %>% 
  left_join(
    decon_trucks_costs %>% 
      select(desconsolidador, capacidad_tarimas, costo_por_viaje) %>% 
      rename_with(paste0, ... = '1'),
    by = c('desconsolidador' = 'desconsolidador1', 'truck_pallets1' = 'capacidad_tarimas1')
  ) %>% 
  left_join(
    decon_trucks_costs %>% 
      select(desconsolidador, capacidad_tarimas, costo_por_viaje) %>% 
      rename_with(paste0, ... = '2'),
    by = c('desconsolidador' = 'desconsolidador2', 'truck_pallets2' = 'capacidad_tarimas2')
  ) %>% 
  left_join(
    decon_avg_costs %>% 
      select(capacidad_tarimas, camion, costo_por_viaje) %>% 
      rename_with(paste0, ... = '_avg1'),
    by = c('truck_pallets1' = 'capacidad_tarimas_avg1')
  ) %>% 
  left_join(
    decon_avg_costs %>% 
      select(capacidad_tarimas, camion, costo_por_viaje) %>% 
      rename_with(paste0, ... = '_avg2'),
    by = c('truck_pallets2' = 'capacidad_tarimas_avg2')
  ) %>% 
  mutate(
    n_camiones = n_camiones1 + n_camiones2,
    ocupacion_prom = (((remain_pallets / truck_pallets2) * n_camiones2) + n_camiones1) / (n_camiones1 + n_camiones2),
    cajas_prom = ((truck_cases1 * n_camiones1) + (truck_cases2 * n_camiones2)) / (n_camiones1 + n_camiones2),
    costo_por_viaje1 = coalesce(costo_por_viaje1, costo_por_viaje_avg1),
    costo_por_viaje2 = coalesce(costo_por_viaje2, costo_por_viaje_avg2),
    costo_total_camiones = case_when(
      # Override manual
      desconsolidador %in% c('Chalco', 'Cuautitlan', 'Santa Barbara') ~ 0,
      TRUE ~ (n_camiones1 * costo_por_viaje1) + (n_camiones2 * costo_por_viaje2)
    )
  ) %>% 
  select(
    vendor_rfc_clean,
    desconsolidador,
    cases,
    pallets,
    pallet_cases,
    ocupacion_prom,
    cajas_prom,
    n_camiones,
    truck_pallets1,
    c('camion1' = 'camion_avg1'),
    n_camiones1,
    costo_por_viaje1,
    remain_pallets,
    truck_pallets2,
    c('camion2' = 'camion_avg2'),
    n_camiones2,
    costo_por_viaje2,
    costo_total_camiones
  )

decon_state_complete_dis <- decon_state_dis %>% 
  left_join(
    decon_summary %>% 
      select(vendor_rfc_clean, desconsolidador, n_camiones, ocupacion_prom, cajas_prom, costo_total_camiones), 
    by = c('vendor_rfc_clean', 'desconsolidador')
  ) %>% 
  group_by(vendor_rfc_clean, desconsolidador) %>% 
  mutate(
    decon_route_pallets = sum(route_pallets),
    costo_individual = costo_total_camiones * (route_pallets / decon_route_pallets)
  ) %>% 
  group_by(vendor_rfc_clean, estado_teradata) %>% 
  summarise(
    costo_decon = sum(costo_individual), 
    ocupacion_prom_decon = sum(n_camiones * ocupacion_prom) / sum(n_camiones),
    cajas_prom_decon = sum(n_camiones * cajas_prom) / sum(n_camiones),
    n_camiones_decon = sum(n_camiones),
    .groups = 'drop'
  )

state_summary <- state_volume %>% 
  left_join(select(state_deliveries, -state_name), by = 'state_code') %>% 
  mutate(
    n_rutas = ceiling(n_tiendas / n_entregas_viaje),
    # route_cases = cases / n_rutas,
    route_pallets = pallets / n_rutas,
    truck_pallets1 = min_greater(route_pallets, total_base$capacidad_tarimas),
    truck_cases1 = truck_pallets1 * pallet_cases,
    n_camiones1 = floor(route_pallets / truck_pallets1),
    remain_pallets = route_pallets - n_camiones1 * truck_pallets1,
    truck_pallets2 = min_greater(remain_pallets, total_base$capacidad_tarimas),
    truck_cases2 = remain_pallets * pallet_cases,
    n_camiones2 = ceiling(remain_pallets / truck_pallets2)
  ) %>% 
  left_join(
    state_truck_costs %>% 
      select(state_code, capacidad_tarimas, costo_por_viaje) %>% 
      rename_with(paste0, ... = '1'),
    by = c('state_code' = 'state_code1', 'truck_pallets1' = 'capacidad_tarimas1')
  ) %>% 
  left_join(
    state_truck_costs %>% 
      select(state_code, capacidad_tarimas, costo_por_viaje) %>% 
      rename_with(paste0, ... = '2'),
    by = c('state_code' = 'state_code2', 'truck_pallets2' = 'capacidad_tarimas2')
  ) %>% 
  left_join(
    state_avg_costs %>% 
      select(capacidad_tarimas, camion, costo_por_viaje) %>% 
      rename_with(paste0, ... = '_avg1'),
    by = c('truck_pallets1' = 'capacidad_tarimas_avg1')
  ) %>% 
  left_join(
    state_avg_costs %>% 
      select(capacidad_tarimas, camion, costo_por_viaje) %>% 
      rename_with(paste0, ... = '_avg2'),
    by = c('truck_pallets2' = 'capacidad_tarimas_avg2')
  ) %>% 
  left_join(
    decon_state_complete_dis, 
    by = c('vendor_rfc_clean', 'state_code' = 'estado_teradata')
  ) %>%
  mutate(
    n_camiones = (n_camiones1 + n_camiones2) * n_rutas,
    ocupacion_prom = (((remain_pallets / truck_pallets2) * n_camiones2) + n_camiones1) / (n_camiones1 + n_camiones2),
    cajas_prom = ((truck_cases1 * n_camiones1) + (truck_cases2 * n_camiones2)) / (n_camiones1 + n_camiones2),
    costo_por_viaje1 = coalesce(costo_por_viaje1, costo_por_viaje_avg1),
    costo_por_viaje2 = coalesce(costo_por_viaje2, costo_por_viaje_avg2),
    costo_camiones_ruta = (n_camiones1 * costo_por_viaje1) + (n_camiones2 * costo_por_viaje2),
    costo_camiones = costo_camiones_ruta * n_rutas,
    costo_total = costo_camiones + costo_decon,
    across(c('costo_camiones', 'costo_decon', 'costo_total'), ~.x / cases, .names = '{col}_por_caja')
  ) %>% 
  rename(c('camion1' = 'camion_avg1', 'camion2' = 'camion_avg2')) %>% 
  select(-contains('avg'))

vendor_summary <- state_summary %>% 
  group_by(vendor_rfc_clean) %>% 
  summarise(
    across(ends_with('prom_decon'), ~sum(n_camiones_decon * .x) / sum(n_camiones_decon)),
    across(ends_with('prom'), ~sum(n_camiones * .x) / sum(n_camiones)),
    across(c('cases', 'pallets', 'n_camiones', 'n_camiones_decon', 'costo_camiones', 'costo_decon', 'costo_total'), sum),
    across(c('vendor_name_clean', 'vendor_name'), first),
    .groups = 'drop'
  ) %>% 
  mutate(across(c('costo_camiones', 'costo_decon', 'costo_total'), ~.x / cases, .names = '{col}_por_caja')) %>% 
  select(starts_with('vendor'), everything())

vendor_summary %>% 
  pull(costo_total_por_caja)

