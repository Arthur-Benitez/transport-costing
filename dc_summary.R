library(tidyverse)
library(ggthemes)
library(readxl)
source('functions.R')

# Union de bases de processed_data ----------------------------------------

processed_files <- list.files(path = 'data/', pattern = '.*-processing_result', full.names = TRUE)
pb <- progress_estimated(length(processed_files))
dc_dic <- read_csv('data/20200724-dc-dictionary.csv')
areas_nielsen <- read_csv('data/20210406-store-nielsen_zones.csv', locale(encoding = "CP1254"), col_names = TRUE, col_types = NULL)

regiones_dic <- tibble(
  whse_nbr = c(5726, 5729, 6014, 6034, 7760, 8032, 8033, 8038, 9696, 9801, 9802, 9804, 9805, 9806, 9810, 9811),
  whse_nbr2 = c(7455, 7453, 4188, 4640, 7471, 7460, 7461, 7459, 7487, 7464, 7494, 7493, 7490, 7482, 7492, 7468),
  grupo_cedis = c('norte', 'sureste', 'sureste', 'norte', 'chalco', 'guadalajara', 'norte', 'chalco', 'norte', 'cuautitlan', 'cuautitlan', 'guadalajara', 'norte', 'sta_barbara', 'megapark', 'sureste')
)
months_names <- tolower(names(lubridate::days_in_month(1:12)))

# Datos Ene-Dic 2020
pre_complete_data <- processed_files %>% 
  map(~{
    pb$tick()$print()
    # safely(function(x){
    #   x %>%
    #     readRDS() %>%
    #     special_summarise(month, whse_nbr)
    # })(.x)$result
    # safely(~readRDS %>% special_summarise(month, whse_nbr))(.x)$result
    safely(readRDS)(.x)$result$data$processed_data
  })%>%
  bind_rows()

# Poner bonito el nombre del CD
complete_data <- pre_complete_data %>% 
  left_join(
    select(dc_dic, -ends_with('_texto')), 
    by = c('whse_nbr', 'whse_nbr2')
  ) %>% 
  left_join(areas_nielsen, by = 'store_nbr') %>% 
  mutate(
    whse_name = coalesce(whse_name.y, whse_name.x),
    whse_name_complete = coalesce(whse_name_complete, whse_name.x)
  ) %>% 
  select(-c(whse_name.x, whse_name.y))

complete_data_comp <- complete_data %>% 
  filter(volumen_vnpk != 0) %>% 
  select(
    id_viaje,
    transportista,
    equipment_type,
    capacidad_transporte,
    placa_tractor,
    esquema,
    store_nbr,
    entregas_viaje,
    ruteo,
    month_nbr,
    month,
    n_viajes,
    download_time,
    journey_time,
    rest_time,
    delivery_time,
    whse_nbr,
    whse_nbr2,
    tipo_whse.x,
    km_total_tienda,
    volumen_vnpk,
    trip_hub_cost,
    km_total_viaje,
    costo_casetas,
    costo_total_spt,
    total_reclamos,
    total_aclaraciones,
    total_regresos,
    trip_remolque_cost_final,
    costo_final_dolly,
    quintas_distribuido,
    monitoreo_distribuido,
    gasto_variable_calculado,
    costo_final_tractor,
    total_cost_teorico,
    whse_name_complete2,
    red,
    nielsen_zone,
    cve_nielsen,
    whse_name
  ) %>% 
  group_by(
    id_viaje,
    transportista,
    equipment_type,
    capacidad_transporte,
    placa_tractor,
    esquema,
    store_nbr,
    ruteo,
    month_nbr,
    month,
    whse_nbr,
    whse_nbr2,
    tipo_whse.x,
    whse_name_complete2,
    red,
    nielsen_zone,
    cve_nielsen,
    whse_name
  ) %>% 
  summarise(
    entregas_viaje = sum(entregas_viaje),
    n_viajes = sum(n_viajes),
    download_time = mean(download_time),
    journey_time = mean(journey_time),
    rest_time = mean(rest_time),
    delivery_time = mean(delivery_time),
    km_total_tienda = sum(km_total_tienda),
    volumen_vnpk = sum(volumen_vnpk),
    trip_hub_cost = sum(trip_hub_cost),
    km_total_viaje = sum(km_total_viaje),
    costo_casetas = sum(costo_casetas),
    costo_total_spt = sum(costo_total_spt),
    total_reclamos = sum(total_reclamos),
    total_aclaraciones = sum(total_aclaraciones),
    total_regresos = sum(total_regresos),
    trip_remolque_cost_final = sum(trip_remolque_cost_final),
    costo_final_dolly = sum(costo_final_dolly),
    quintas_distribuido = sum(quintas_distribuido),
    monitoreo_distribuido = sum(monitoreo_distribuido),
    gasto_variable_calculado = sum(gasto_variable_calculado),
    costo_final_tractor = sum(costo_final_tractor),
    total_cost_teorico = sum(total_cost_teorico)
  )

write.table(complete_data_comp, 'output/complete_data3.txt',  row.names = F, quote = F, sep = ";")

# Volumenes de cajas ------------------------------------------------------

volumenes_base <- readxl::read_excel('data/Volumenes_por_formato.xlsx', sheet = 'Consolidado_Embarque')

n_meses <- volumenes_base %>% 
  filter(between(fecha, min(complete_data$fecha_embarque), max(complete_data$fecha_embarque))) %>% 
  pull(fecha) %>% 
  month() %>% 
  n_distinct()

volumenes <- volumenes_base %>% 
  filter(between(fecha, min(complete_data$fecha_embarque), max(complete_data$fecha_embarque))) %>% 
  group_by(dc_nbr) %>% 
  summarise(
    volumen_ocupacion = sum(volumen_ocupacion),
    avg_vol_mensual = volumen_ocupacion / n_meses
  )

vol_intercedis <- volumenes_base %>% 
  left_join(regiones_dic, by = c('dc_nbr' = 'whse_nbr2')) %>% 
  mutate(grupo_cedis = coalesce(grupo_cedis, as.character(dc_nbr))) %>% 
  group_by(grupo_cedis, familia) %>% 
  summarise(vnpk_intercedis = sum(volumen_ocupacion)) %>% 
  group_by(grupo_cedis) %>% 
  mutate(vnpk_inter_perc = vnpk_intercedis / sum(vnpk_intercedis)) %>% 
  filter(familia == 'Intercedis') %>% 
  select(-c(familia, vnpk_intercedis))

# Datos del P&L total 2019 ------------------------------------------------

pl <- tibble(
  wtms_whse_nbr = c(7471, 7490, 7493, 7468, 7464, 7487, 7494, 7482, 7492, 5907, 7459, 7457, 7461, 7460, 7453, 7455, 5922, 4188, 4640),
  total_gasto = c(682959610.82, 566434178.81, 661660422.66, 715815068.75, 359739578.71, 786201860.93, 437958753.94, 248421508.22, 154417296.21, NA, 93584879.19, 92478779.59, 77341342.31, 135657000.36, 14869264.00, 12945627.86, NA, 0, 0)
) %>% 
  mutate(
    avg_gasto_mensual = total_gasto / 12
  )

# Resúmenes e info para gráficas ------------------------------------------
# Costo por caja por cedis
case_cost_summary <- complete_data %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete2, red) %>% 
  summarise(total_gasto = sum(total_cost_teorico)) %>% 
  left_join(volumenes, by = c('whse_nbr2' = 'dc_nbr')) %>% 
  filter(digits(whse_nbr) == 4) %>% 
  mutate(costo_por_caja = total_gasto / volumen_ocupacion)

# Resumen por cedis/mes
whse_month_summary <- complete_data %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete2, month) %>%
  summarise(
    across(c(costo_total_spt, trip_tractor_cost, total_casetas, costo_final_remolque, trip_hub_cost, costo_final_dolly, gasto_variable_teorico_entrega), sum),
    .groups = 'drop'
  ) %>%
  # Hay que validar los que no tienen 4 digitos
  filter(digits(whse_nbr) == 4)

plot_whse_nbrs <- complete_data %>% 
  filter(whse_nbr2 != 7492) %>% 
  group_by(whse_nbr) %>%
  summarise(total_gasto = sum(total_cost_teorico), .groups = 'drop') %>% 
  arrange(desc(total_gasto)) %>% 
  pull(whse_nbr) %>% 
  unique() %>% 
  head(9)


# Comparativa vs P&L ------------------------------------------------------

# Resumen por CEDIS  
whse_summary <- complete_data %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete, month) %>% 
  summarise(total_gasto = sum(total_cost_teorico)) %>% 
  # Doble summarise para poder contabilizar los meses y hacer correctamente el promedio
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete) %>% 
  summarise(
    avg_gasto_mensual = mean(total_gasto, na.rm = TRUE), 
    # Puede haber meses en NA... pero deberían estar?
    n_meses = sum(!is.na(total_gasto)),
    total_gasto = sum(total_gasto, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  # Hay que validar los que no tienen 4 digitos
  filter(digits(whse_nbr) == 4)

comparison_base <- whse_summary %>% 
  left_join(
    select(regiones_dic, -whse_nbr2),
    by = 'whse_nbr'
  ) %>% 
  # Debiera ser un left join al final
  inner_join(
    pl,
    by = c('whse_nbr2' = 'wtms_whse_nbr'),
    suffix = c('_nuestro', '_pl')
  ) %>% 
  left_join(volumenes, by = c('whse_nbr2' = 'dc_nbr')) %>% 
  mutate(
    grupo_cedis = coalesce(grupo_cedis, as.character(whse_nbr))
  ) %>% 
  filter(!whse_nbr2 == 7492)

comparison <- comparison_base %>% 
  group_by(grupo_cedis) %>% 
  summarise(across(
    c(starts_with('avg_gasto_'), starts_with('total_gasto')), 
    sum, 
    na.rm = TRUE
  )) %>% 
  mutate(
    diff_gasto = avg_gasto_mensual_nuestro - avg_gasto_mensual_pl,
    diffp_gasto = diff_gasto / avg_gasto_mensual_pl,
    diffp_gasto = ifelse(is.infinite(diffp_gasto), NA, diffp_gasto)
  )

comparison_total <- comparison_base %>% 
  summarise(across(
    c(starts_with('avg_'), starts_with('total_gasto'), starts_with('volumen')), 
    sum, 
    na.rm = TRUE
  )) %>%
  mutate(
    # whse_name = 'Total',
    # whse_name_complete = 'Total',
    grupo_cedis = 'Total',
    diff_gasto = avg_gasto_mensual_nuestro - avg_gasto_mensual_pl,
    diffp_gasto = diff_gasto / avg_gasto_mensual_pl
  )

# Resumen comparativo
comparison %>% 
  arrange(desc(abs(diffp_gasto))) %>% 
  bind_rows(comparison_total) %>% 
  left_join(vol_intercedis, by = 'grupo_cedis') %>% 
  # filter(!is.na(avg_gasto_mensual_nuestro), !is.na(avg_gasto_mensual_pl)) %>% 
  select(
    # starts_with('whse_nbr'), 
    # starts_with('whse_name'), 
    # n_meses,
    grupo_cedis,
    # starts_with('total_gasto'),
    starts_with('avg_gasto_'), 
    starts_with('diff'),
    vnpk_inter_perc
  ) %>% 
  copy()

# Gráfico de Dispersión ---------------------------------------------------

grafica_dispersion <- case_cost_summary %>% 
  filter(!is.na(costo_por_caja), whse_nbr2 != 7492) %>% 
  mutate(whse_nbr = as.factor(whse_nbr)) %>% 
  ggplot(aes(x = reorder(whse_name_complete2, -costo_por_caja), y = costo_por_caja)) +
  geom_bar(position = "dodge", stat = "identity", fill = '#00498d') +
  geom_label(aes(label = sprintf("% .2f", costo_por_caja)), nudge_y = 1, alpha = 0.5) +
  theme(legend.position = "bottom") +
  facet_wrap(vars(red), ncol= 1, scales = 'free') +
  ylim(0, NA) +
  labs(
    title = 'Costo por caja promedio de Ene-Oct 2020',
    x = 'CEDIS',
    y = 'Costo por caja'
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12)
  ) +
  theme(axis.text.x = element_text(angle = 90))

grafica_dispersion

# Gráfica de barras por CEDIS/mes -----------------------------------------

cedis_mes <- whse_month_summary %>% 
  filter(whse_nbr %in% plot_whse_nbrs) %>% 
  pivot_longer(
    cols = c('costo_total_spt', 'trip_tractor_cost', 'total_casetas', 'costo_final_remolque', 'trip_hub_cost', 'costo_final_dolly', 'gasto_variable_teorico_entrega'),
    names_to = 'tipo_de_costo',
    values_to = 'total_gasto'
  ) %>%
  ggplot(aes(x = month, fill = tipo_de_costo, y = total_gasto)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(vars(whse_name_complete2), scales = 'free') + 
  ylim(0, NA) + 
  labs(
    title = 'Costos de transportes por mes',
    x = 'Mes',
    y = 'Total costos transporte',
    fill = 'Tipo de costo'
  ) +
  scale_x_discrete(limits = months_names[min(complete_data$month_nbr):max(complete_data$month_nbr)]) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_colorblind(labels = c('costo_total_spt' = 'SPT', 'trip_tractor_cost' = 'Tractores', 'total_casetas' = 'Casetas', 'costo_final_remolque' = 'Remolques', 'trip_hub_cost' = 'Hubs', 'costo_final_dolly' = 'Dollies', 'gasto_variable_teorico_entrega' = 'Variable')) +
  # scale_fill_colorblind() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 10)
  )

cedis_mes
