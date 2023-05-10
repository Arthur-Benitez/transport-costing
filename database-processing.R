# Variables globales -----------------------------------------------------------
library(tidyverse)
library(readxl)
library(geodist)
source('functions.R')
library(lubridate)

mes_de_lectura <- 'jan'

processing_code_version <- '0.1.0'

months_names <- tolower(names(days_in_month(1:12)))
months_nbrs <- tibble(
  month_nbr = c(1:12),
  month = months_names
)
meses <- read_csv('data/meses.csv')

dc_dic <- read_csv('data/20200724-dc-dictionary.csv')
duplicated_dc <- dc_dic$whse_nbr[duplicated(dc_dic$whse_nbr)]

volumenes_orig <- readRDS('data/20210120-volumen_cedis_tienda_fecha') %>% 
  filter(det_origen > 0, det_destino > 0, !is.na(det_origen), !is.na(det_origen))

volumen <- volumenes_orig %>%
  group_by(det_origen, det_destino, month) %>% 
  summarise(route_vol = sum(volumen_vnpk), .groups = 'drop') %>% 
  rename(
    'whse_nbr2' = 'det_origen',
    'store_nbr' = 'det_destino',
    'month_nbr' = 'month'
  )

#Información de tiendas
location_data <- read_excel(
  path = 'data/20200910_Catalogo_Tiendas-Cedis-Hub-Desconsolidador_vf.xlsx',
  sheet = 1,
  col_types = 'text'
  ) %>%
  mutate(
    across(
      c(determinante, lat, long),
      ~as.numeric(str_remove_all(.x, '[^-.0-9]'))
    )
  )

locs <- list()
locs$tiendas <- location_data$determinante[location_data$id_formato %in% c('BA', 'BAE', 'MB', 'SC', 'SM', 'SP')]
locs$cedis <- location_data$determinante[location_data$id_formato %in% c('CEDIS')]
locs$hubs <- location_data$determinante[location_data$id_formato %in% c('Desconsolidador', 'Patio Hub','Por definir')]
locs <- map(locs, sort)


# Procesamiento tiempos MTS ------------------------------------------

descargas_mts <- readRDS('data/20210106-mts-download')

#descargas_mts_1 <- read_excel(
#  path = 'data/Descargas MTS_v04.xlsx',
#  sheet = 'Viajes Despachados Jul')

columnas_descargas_mts <- c(
  whse_nbr               = "Determinante Gasto", 
  store_nbr              = "ID Destino",
  fecha_salida_cedis     = "Fecha CHECK OUT",
  fecha_llegada_destino  = "Fecha Llegada Destino", 
  fecha_termino_descarga = "Fecha Termino Descarga"
)

descargas_mts_filt <- descargas_mts %>%
  select(columnas_descargas_mts) %>% 
  mutate(across(starts_with('fecha_'), as.POSIXct)) %>% 
  filter(months_names[month(fecha_salida_cedis)] == mes_de_lectura, !is.na(fecha_llegada_destino), !is.na(fecha_salida_cedis)) %>% 
  mutate(
    journey_time = difftime(ymd_hms(fecha_llegada_destino), ymd_hms(fecha_salida_cedis), units = "hours"),
    download_time = difftime(ymd_hms(fecha_termino_descarga), ymd_hms(fecha_llegada_destino), units = "hours")
  ) %>% 
  filter(download_time > 0, journey_time > 0)

download_time_store <- descargas_mts_filt %>%
  group_by(store_nbr) %>%
  summarise(
    avg_download_time_store = mean(download_time),
    avg_journey_time_store = mean(journey_time)
  ) %>%
  filter(
    store_nbr %in% locs$tiendas
  )


# Cleasing Asistencias ---------------------------------

raw_data_tarifas <- read_excel(
  path = sprintf('data/asistencias/Asistencia %s.xlsx', mes_de_lectura),
  sheet = 'Base de Asistencia',
  skip = 6
)

columnas_tarifas <- c(
  'whse_nme'                = 'CEDIS',               
  'whse_nbr'                = 'DTT GTS',        
  'red'                     = 'RED',                
  'carrier_nme'             = 'TRANSPORTISTA',     
  'equipment_type'          = 'TIPO DE VEHICULO',  
  'equipment_type2'         = 'CAPACIDAD', 
  'categoria'               = 'CATEGORÍA (SECO/PERECEDERO)',
  'placa_tractor'           = 'PLACAS CONVENIO',
  'placa_sustituto'         = 'PLACAS SUSTITUTA',
  'temporada'               = 'UNIDADES DE TEMPORADA SI / NO',
  'monthly_cost'            = 'FIJO/MES',
  'daily_cost'              = 'COSTO DIARIO',
  'updated_month_cost'      = 'TARIFA ACTUALIZADA',
  'updated_daily_cost'      = 'COSTO DIARIO ACTUALIZADO',
  'pago_bruto'              = 'PAGO MENSUAL',
  'porc_max_pago'           = 'PARTICIPACION',
  'descuento'               = 'DESCUENTO MENSUAL',
  'bonus'                   = 'BONIFICACION MENSUAL',
  'total_fixed_cost'        = 'TOTAL MENSUAL' 
)

tarifas <- raw_data_tarifas %>% 
  select(
    columnas_tarifas
  ) %>%  
  mutate(
    whse_nbr = coalesce(as.numeric(whse_nbr), as.numeric(red)),
    placa_tractor = toupper(str_squish(ifelse(
    is.na(str_extract(placa_tractor, '.*(?=\\s\\()')),
    str_remove_all(placa_tractor,' |-'),
    str_extract(placa_tractor, '.*(?=\\s\\()'))))
  ) %>%
  filter(!(is.na(whse_nbr))) %>%
  group_by(placa_tractor) %>% 
  summarise(placa_tractor_cost = sum(total_fixed_cost, na.rm = TRUE))


# Gasto variable téorico --------------------------------------------------

mts_base <- read_excel('data/202011_MTS_Base.xlsx')

columnas_mts_base <- c(
  'tipo_reporte' = 'Tipo Reporte',
  'clave_reporte' = 'Clave Reporte',
  'tipo_folio' = 'Tipo Folio',
  'determinante' = 'Det.',
  'nomenclatura_gi' = 'Nomenclatura GI',
  'operacion' = 'Operacion',
  'cedis' = 'Cedis',
  'region' = 'Region',
  'mes' = 'Mes',
  'periodo' = 'Periodo',
  'km_ded' = 'Kms Dedicado',
  'km_ter' = 'Kms Tercero',
  'outbound_ded_sici' = 'Outbound Dedicado SICI',
  'outbound_ter_sici' = 'Outbound Tercero SICI',
  'monitoreo' = 'Monitoreo',
  'transferencias' = 'Transferencias:',
  'ferry' = 'Ferry',
  'arrendamiento_qui' = 'Arrendamiento...89',
  'mantenimiento_qui' = 'Mantenimiento...90',
  'arrastres_qui' = 'Arrastres...91',
  'otros_qui' = 'Verificación, Permisos y Otros...92',
  'desconsolidadores' = 'Desconsolidadores'
)

filter_outliers <- function(sample, median) {
  x <- tibble(sample, median)
  x %>% 
    mutate(
      filtered_data = ifelse(
        ((((-10 * median) > sample) | 
            (sample > (median * 10))) &
           sample != 0) | 
          is.na(sample),
        median,
        sample
      )
    ) %>% 
    pull(filtered_data)
}

gasto_variable <- mts_base %>% 
  select(columnas_mts_base) %>% 
  filter(
    tipo_reporte == 'Determinante',
    tipo_folio == 'Real',
    periodo == '2020'
  ) %>% 
  left_join(meses, by = 'mes') %>% 
  group_by(determinante, month) %>% 
  summarise(across(c(km_ded:desconsolidadores),  sum), .groups = 'drop') %>% 
  mutate(
    determinante = as.numeric(determinante),
    kms = km_ded + km_ter,
    sici = outbound_ded_sici + outbound_ter_sici,
    quintas = arrendamiento_qui + mantenimiento_qui + arrastres_qui + otros_qui,
    gasto_variable = sici + transferencias + ferry + desconsolidadores,
    across(
      c(gasto_variable, monitoreo, quintas),
      ~case_when(
        kms != 0 & .x != 0 ~ .x / kms,
        .x == 0 ~ 0,
        kms == 0 ~ NA_real_
      ),
      .names = "{col}_km"
    )
  ) %>% 
  group_by(determinante) %>% 
  mutate(
    across(
      ends_with('_km'),
      ~abs(median(na_if(.x, 0), na.rm = TRUE)),
      .names = "{col}_med"
    ),
    gasto_variable_km = filter_outliers(gasto_variable_km, gasto_variable_km_med),
    monitoreo_km = filter_outliers(monitoreo_km, monitoreo_km_med),
    quintas_km = filter_outliers(quintas_km, quintas_km_med)
  ) %>% 
  ungroup()


# Procesamiento inicial datos SPT --------------------------------------------------------

columnas_spt <- c(
  'fecha_embarque'       = 'FECHA DE EMBARQUE',
  'fecha_carga'          = 'FECHA CARGA',
  'id_viaje'             = 'No. VIAJE',
  # 'origen_viaje'         = 'ORIGEN VIAJE',
  'tipo_viaje'           = 'TIPO VIAJE',
  'transportista'        = 'NOMENCLATURA',
  'equipment_type'       = 'CLAVE GTS',
  'equipment_desc'       = 'TIPO DE VEHICULO',
  'capacidad_transporte' = 'CAP.FT',
  'placa_tractor'        = 'No. TRACTOR',
  'placa_remolque'      = 'No. CAMION REMOLQUE',
  'esquema'              = 'ESQUEMA',
  'whse_name'            = 'DET. ORIGEN',
  'tipo_origen'          = 'TIPO ORIGEN',
  'whse_name_cargo'      = 'DET. CARGO',
  'ciudad_destino'       = 'CIUDADES DESTINO',
  'km_total_viaje_original'             = 'KM TOTALES',
  'km_ida_viaje_original'               = 'KM IDA',
  'km_regreso_viaje_original'           = 'KM REGRESO',
  'estatus_pago'         = 'ESTATUS',
  'store_nbr'            = 'ENT1',
  'costo_prov_original'           = '$OUTB.PROV.',
  'costo_casetas_original'        = '$CASETAS',
  'costo_diesel_original'         = '$TOTAL DIESEL',
  'costo_total_spt_original'          = '$TOTAL PROV.'
)

spt_raw_data <- read_excel(
  path = sprintf('data/spt/%s SPT.xlsx', mes_de_lectura),
  skip = 6
)

whse_regions <- read_csv(
  'data/20200629-whse-regions.csv'
) %>%
  select(-whse_name) %>% 
  mutate(region = toupper(region))

pre_processed_data <- spt_raw_data %>% 
  rename(columnas_spt) %>% 
  set_names(str_replace(names(.), '^ENT', 'store_nbr')) %>% 
  select(names(columnas_spt), starts_with('store_nbr')) %>% 
  filter(
    # Algunas bases tienen al final algunos datos de los totales, esas filas tienen vacía la fecha de embarque
    !is.na(fecha_embarque),
    estatus_pago != 'CANCELADO',
    !(costo_total_spt_original %in% c(0.01)),
    !str_detect(equipment_type, 'FIJ')
  ) %>% 
  # Algunas entregas son a CD y tienen una letra después de la determinante
  mutate_at(
    vars(starts_with('store_nbr')),
    ~as.numeric(str_remove(., pattern = '\\D*$'))
  ) %>% 
  mutate(
    all_store_nbrs_vector = apply(
      select(., matches('^store_nbr')),
      1,
      function(x) unname(c(x[!is.na(x)]))
    ),
    entregas_viaje = apply(
      select(., matches('^store_nbr')),
      1,
      function(x) sum(!is.na(x))
    ),
    ruteo = entregas_viaje > 1,
    tipo_viaje = case_when(
      digits(store_nbr) > 4 & !(store_nbr %in% location_data$determinante) ~ 'BACKHAUL',
      str_detect(equipment_desc, 'CAS') ~ 'CASETAS',
      TRUE ~ tipo_viaje
    ),
    whse_nbr = as.numeric(str_extract(
      string = whse_name,
      # Busca todos los números dentro de los paréntesis
      pattern = '(?<=\\()\\d*(?=\\))'
    )),
    whse_nbr_cargo = as.numeric(str_extract(
      string = whse_name_cargo,
      # Busca todos los números dentro de los paréntesis
      pattern = '(?<=\\()\\d*(?=\\))'
    )),
    placa_tractor = toupper(str_remove(placa_tractor,'(?=-).*')),
    month_nbr = month(fecha_embarque),
    month = months_names[month_nbr]
  ) %>% 
  select(-matches('^store_nbr\\d+$')) %>% 
  unnest(all_store_nbrs_vector) %>% 
  # La nueva columna de tiendas es la que tiene la determinante de cada entrega
  rename(c(
    'primera_entrega' = 'store_nbr',
    'store_nbr' =  'all_store_nbrs_vector'
  )) %>% 
  mutate(
    tipo_remolque = case_when(
      str_detect(equipment_type, 'CA|TH|RA') ~ NA_character_,
      TRUE ~ str_extract(equipment_type, '(?<=\\s).+(?=\\s)')
    ),
    full_mult = ifelse(str_detect(equipment_type, 'F$'), 2, 1),
    capacidad_transporte = ifelse(capacidad_transporte == 80, 40, capacidad_transporte),
    key = paste0(id_viaje, store_nbr)
  ) %>% 
  group_by(id_viaje) %>% 
  mutate(
    n_remolques = full_mult / n(),
    n_viajes = 1 / n(),
    tipo_viaje = case_when(
      tipo_viaje %in% c('OUTBOUND', 'OUTB FULL H', 'OUTB FULL T', 'OUTB HUB P', 'OUTB HUB S') & store_nbr %in% locs$cedis ~ 'INTERCEDIS OUTBOUND',
      TRUE ~ tipo_viaje
    ),
    # Se multiplica por los viajes solo para tener una distribución equitativa. Esta es solo una distribución inicial para poder tener los costos distribuidos de los hubs, pero la distribución correcta va más adelante (se sobrescribe).
    # Hay algunos viajes que tienen entregas en hubs y en tiendas, para esos, la distribución podría aumentar o reducir ligeramente el total del viaje.
    across(
      .cols = c(km_total_viaje_original:km_regreso_viaje_original, costo_prov_original:costo_total_spt_original),
      .fns = ~.x * n_viajes,
      .names = '{col}_dividido'
    )
  ) %>% 
  rename_with(~str_remove(.x, '_original_dividido'), ends_with('original_dividido')) %>% 
  ungroup() %>% 
  mutate(
    control_tractores_placa = 
      tipo_viaje %in% c('BACKHAUL', 'BOB', 'CENTERPOINT', 'CENTER POINT', 'DEVOLUCIONES', 'INTERCEDIS OUTBOUND', 'PORTEO', 'Porteo Full', 'TARIMAS', 'TRANSF TDA', 'TRANSFERENCIA', 'VACIO', 'VACIO FULL', 'OUTBOUND', 'OUTB FULL H', 'OUTB FULL T', 'OUTB HUB P', 'OUTB HUB S') & 
      !(tipo_viaje == 'BACKHAUL' & str_detect(equipment_desc, 'CAS')),
    # Los pxv si deben tener costo extra de tractores por placa pero no por cedis
    control_tractores_whse = control_tractores_placa & esquema == 'DEDICADO',
    original_placa_tractor = placa_tractor,
    placa_tractor = ifelse(control_tractores_placa, placa_tractor, NA)
  )


# Unión SPT y Tarifas -----------------------------------------------------

tarifas_no_coincidentes <- raw_data_tarifas %>% 
  select(
    columnas_tarifas
  ) %>%
  mutate(
    whse_nbr = coalesce(as.numeric(whse_nbr), as.numeric(red)),
    placa_tractor = toupper(str_squish(ifelse(
      is.na(str_extract(placa_tractor, '.*(?=\\s\\()')),
      str_remove_all(placa_tractor,' |-'),
      str_extract(placa_tractor, '.*(?=\\s\\()'))))
  ) %>%
  group_by(placa_tractor) %>% 
  anti_join(pre_processed_data, by = 'placa_tractor') %>%
  filter(!(is.na(whse_nbr))) %>%
  group_by(whse_nbr) %>% 
  summarise(whse_tractor_cost = sum(total_fixed_cost, na.rm = TRUE))

tractor_processed_data <- pre_processed_data %>% 
  left_join(
    download_time_store,
    by = 'store_nbr'
  ) %>% 
  left_join(
    tarifas, 
    by = 'placa_tractor'
  ) %>% 
  group_by(placa_tractor) %>% 
  mutate(
    download_time = as.numeric(
      replace_na(
        avg_download_time_store, 
        median(descargas_mts_filt$download_time)
      )
    ),
    journey_time = as.numeric(
      replace_na(
        avg_journey_time_store,
        median(descargas_mts_filt$journey_time)
      )
    ),
    rest_time = (journey_time %/% 8) * 3,
    delivery_time = journey_time + download_time + rest_time,
    time_tractor_share = replace_na(delivery_time / sum(delivery_time, na.rm = TRUE),0),
    trip_tractor_cost = replace_na(placa_tractor_cost * time_tractor_share, 0)
  ) %>% 
  select(-c(avg_download_time_store, avg_journey_time_store)) %>%
  ungroup() %>%
  left_join(
    tarifas_no_coincidentes,
    by = 'whse_nbr'
  ) %>%
  mutate(
    whse_tractor_cost = replace_na(whse_tractor_cost, 0),
    whse_distribution_factor = case_when(
      !control_tractores_whse ~ 0,
      is.na(placa_tractor_cost) ~ 2,
      TRUE ~ 1
    )
  ) %>% 
  group_by(whse_nbr) %>% 
  mutate(
    # El ifelse se necesita para handlear los NaNs
    whse_tractor_share = ifelse(control_tractores_whse, whse_distribution_factor / sum(whse_distribution_factor), 0),
    trip_whse_tractor_cost = whse_tractor_cost * whse_tractor_share,
    final_trip_tractor_cost = trip_tractor_cost + replace_na(trip_whse_tractor_cost,0)
  ) %>% 
  ungroup ()


# Reclamos, regresos y aclaraciones ---------------------------------------

reclamos <- tractor_processed_data %>% 
  select(transportista, equipment_desc, whse_nbr_cargo, costo_total_spt_original, estatus_pago) %>% 
  filter(str_detect(equipment_desc, 'CAS') & !str_detect(transportista, 'K$')) %>% 
  group_by(transportista, whse_nbr_cargo) %>% 
  summarise(
    reclamos = sum(costo_total_spt_original)
  )

aclaraciones <- spt_raw_data %>% 
  rename(columnas_spt) %>% 
  select(tipo_viaje, transportista, equipment_desc, whse_name_cargo, costo_total_spt_original, estatus_pago) %>%
  filter(
    estatus_pago != 'CANCELADO',
    !(costo_total_spt_original %in% c(0.01)),
    tipo_viaje %in% c('ESPECIAL', 'ESPECIAL CEDIS'),
    !str_detect(equipment_desc, 'FIJ'),
    !(str_detect(equipment_desc, 'CAS') & !str_detect(transportista, 'K$'))
  ) %>%
  group_by(transportista, whse_name_cargo) %>% 
  summarise(aclaraciones = sum(costo_total_spt_original), .groups = 'drop') %>% 
  mutate(whse_nbr_cargo = as.numeric(str_extract(
    string = whse_name_cargo,
    # Busca todos los números dentro de los paréntesis
    pattern = '(?<=\\()\\d*(?=\\))'
     ))
  )

regresos <- tractor_processed_data %>%
  select(tipo_viaje, transportista, whse_nbr_cargo, costo_total_spt_original, estatus_pago, final_trip_tractor_cost) %>% 
  filter(str_detect(tipo_viaje, 'VACIO')) %>% 
  group_by(transportista, whse_nbr_cargo) %>% 
  summarise(
    regresos = sum(costo_total_spt_original),
    tractores_regresos = sum(final_trip_tractor_cost, na.rm = TRUE)
  )


# Procesamiento de hubs ---------------------------------------------------

pre_hubs_data <- tractor_processed_data %>% 
  filter(
    # Viajes de CD a tienda, incluyendo CD virtuales (Hubs)
    tipo_origen %in% c('CDW', 'CDV'),
    # Viajes de CD a tienda
    tipo_viaje %in% c('OUTBOUND', 'OUTB FULL H', 'OUTB FULL T', 'OUTB HUB P', 'OUTB HUB S'),
    # Esto no *debiera* suceder con viajes de outbound.
    whse_nbr != store_nbr
  )

dc_hub <- pre_hubs_data %>% 
  # Las tiendas que no están en el catálogo de tiendas, no son tiendas, son determinantes de los hubs, bodegas madres y desconsolidadores.
  # Origen = CEDIS
  # Destino = Hub/Desc (falta BM)
  filter(tipo_origen == 'CDW' & store_nbr %in% locs$hubs) %>% 
  rename(c('hub_nbr' = 'store_nbr')) %>% 
  group_by(whse_nbr, hub_nbr) %>%
  summarise(
    group_hub_cost = sum(costo_total_spt) + sum(final_trip_tractor_cost) + sum(costo_casetas),
    .groups = 'drop'
  )

# Este no funciona bien, no tiene todo lo que no se pega abajo, de tractores solo se pega 83%, del faltante 17% solo hay 4% aquí (quizás por el join)
# Gastos de inbound a hubs que no tienen outbound
no_outbound_hubs <- pre_hubs_data %>% 
  filter(tipo_origen == 'CDW' & store_nbr %in% locs$hubs) %>% 
  anti_join(filter(pre_hubs_data, tipo_origen == 'CDV'), by = c('store_nbr' = 'whse_nbr'))

#Viajes de HUB a Tienda
hub_store <- pre_hubs_data %>% 
  filter(tipo_origen == 'CDV') %>% 
  rename('hub_nbr' = 'whse_nbr') %>% 
  left_join(dc_hub, by = 'hub_nbr') %>% 
  # El tipo_whse cambia porque cambió el whse
  left_join(select(dc_dic, whse_nbr, tipo_whse), by = 'whse_nbr') %>% 
  left_join(select(location_data, determinante, tipo_store), by = c('store_nbr' = 'determinante')) %>% 
  mutate(valido = tipo_whse == tipo_store) %>% 
  group_by(id_viaje) %>% 
  filter(valido | is.na(valido) | sum(valido) == 0) %>% 
  ungroup() %>% 
  group_by(hub_nbr, .add = TRUE) %>% 
  mutate(
    # Para que no se quede vacío
    whse_nbr = coalesce(whse_nbr, hub_nbr),
    # Hay algunos hubs de los que solo hay registro de outbound pero no de inbound.
    group_hub_cost = replace_na(group_hub_cost, 0),
    # Esta variable debe calcularse después del filtro de validez para que el costo no disminuya
    hub_whse_share = replace_na(group_hub_cost / sum(unique(group_hub_cost)), 1),
    # Vamos a distribuir los costos y km de cada viaje entre varios "viajes virtuales" que provengan de cada uno de los CD que entregan a cada hub. Esto con el objetivo de que los costos de los viajes estén atribuidos a cada CD con base en la participación de costos de cada uno.
    across(
      .cols = c(n_remolques:costo_total_spt, 
                trip_tractor_cost, 
                trip_whse_tractor_cost,
                final_trip_tractor_cost),
      .fns = ~.x * hub_whse_share
    )
  ) %>% 
  # Para que no se dupliquen con el join de abajo
  select(-c(tipo_whse, tipo_store)) %>% 
  ungroup()


# Unión viajes y hubs -----------------------------------------------------

bound_data <- pre_hubs_data %>% 
  # Viajes de CD (no hubs) a tiendas (no hubs)
  filter(tipo_origen == 'CDW' & store_nbr %in% locs$tiendas) %>% 
  bind_rows(hub_store, no_outbound_hubs) %>% 
  left_join(whse_regions, by = 'whse_nbr') %>% 
  # Ya con los cedis correctos (después de la correción de los hubs)
  left_join(select(dc_dic, whse_nbr, whse_nbr2, tipo_whse), by = 'whse_nbr') %>% 
  left_join(volumen, by = c('whse_nbr2', 'store_nbr', 'month_nbr')) %>% 
  left_join(
    select(location_data, determinante, whse_lat = lat, whse_long = long),
    by = c('whse_nbr' = 'determinante')
  ) %>%
  left_join(
    select(location_data, determinante, store_lat = lat, store_long = long, tipo_store),
    by = c('store_nbr' = 'determinante')
  ) %>%
  mutate(
    valido = tipo_whse == tipo_store,
    # Este cálculo no se puede hacer agrupado
    km_total_tienda = geodist(
      x = select(., x = whse_long, y = whse_lat),
      y = select(., x = store_long, y = store_lat),
      paired = TRUE,
      measure = 'geodesic'
    ) / 1000
  ) %>% 
  filter(!(whse_nbr %in% duplicated_dc) | valido | is.na(valido)) %>% 
  group_by(whse_nbr2, store_nbr) %>% 
  mutate(
    vol_share = case_when(
      is.na(sum(full_mult * capacidad_transporte)) ~ 1 / n(),
      TRUE ~ (full_mult * capacidad_transporte) / sum(full_mult * capacidad_transporte)
    ),
    volumen_vnpk = replace_na(vol_share * route_vol, 0)
  ) %>%
  # Esto es para repartir los costos de viajar a cada hub desde cada CD dependiendo del volumen de esa tienda.
  group_by(hub_nbr, whse_nbr) %>%
  mutate(
    hub_vol_share = volumen_vnpk / sum(volumen_vnpk),
    trip_hub_cost = replace_na(ifelse(is.nan(group_hub_cost * hub_vol_share), 0, group_hub_cost * hub_vol_share), 0)
  )%>%
  group_by(id_viaje) %>% 
  mutate(
    km_vol_entrega_share = case_when(
      # Route_vol porque volumen_vnpk no puede ser NA (no conviene)
      is.na(sum(km_total_tienda)) & is.na(sum(route_vol)) ~  1 / n(),
      is.na(sum(km_total_tienda)) ~ volumen_vnpk / sum(volumen_vnpk),
      is.na(sum(route_vol)) ~ km_total_tienda / sum(km_total_tienda),
      TRUE ~ (km_total_tienda * volumen_vnpk) / sum(km_total_tienda * volumen_vnpk)
    ),
    across(
      .cols = c(km_total_viaje_original:km_regreso_viaje_original, costo_prov_original:costo_total_spt_original),
      .fns = ~.x * km_vol_entrega_share,
      .names = '{col}_dividido'
    ),
    grupo_tractor = paste(	
      whse_nbr,	
      str_replace_all(equipment_type, ' ', '_'),	
      sep = '-'	
    ),
    grupo_remolque = ifelse(
      is.na(tipo_remolque) | (esquema == 'SENCILLO' & !str_detect(equipment_type, 'AR')),
      NA_character_,
      paste(
        whse_nbr,
        tipo_remolque,
        capacidad_transporte,
        sep = '-'
      )
    ),
    control_gv = as.numeric(str_remove(grupo_remolque, '(=?-).*'))
  ) %>% 
  # Las de la distribución uniforme serán sustituidas por la nueva distribución vol-km
  select(-c(km_total_viaje:km_regreso_viaje, costo_prov:costo_total_spt)) %>% 
  rename_with(~str_remove(.x, '_original_dividido'), ends_with('original_dividido')) %>% 
  ungroup()


# Procesamiento base de remolques ---------------------------------------------
# La sección de remolques debe estar aquí en medio porque requiere de los grupo_remolques finales que tendrá la base para poder hacer su filtro y distribución.

raw_data_remolques <- tibble(
  path = 'data/remolques_2020.xlsx',
  sheet = c('Seco', 'Perecedero')
  ) %>% 
  rowwise() %>% 
  summarise(
    data = list(read_excel(path = path, sheet = sheet))
  )

dc_rem_region <- read_csv('data/20200618-cd_region.csv') %>% 
  filter(whse_nbr %in% bound_data$whse_nbr)

months_cols <- c(rbind(
  paste(months_names, 'month_cost', sep = '_'),
  paste(months_names, 'days', sep = '_')))

existing_grupos_remolques <- bound_data %>% 
  pull(grupo_remolque) %>% 
  unique() %>% 
  sort(na.last = NA)

columnas_remolques <- c(
  'placa'           = 'Placa',               
  'trailer_type'    = 'Tipo_Unidad',        
  'category'        = 'Categoria_Unidad',                
  'capacidad'       = 'Capacidad',
  'region'          = 'Cedis',
  'whse_name'       = 'Operación',
  'contract_type'   = 'Titulo Comercial 2', 
  'contract_status' = 'Status_Contrato',
  'unit_status'     = 'Status_Unidad'
)

remolques_base <- raw_data_remolques %>% 
  rowwise() %>% 
  mutate(
    data = list(
      data %>% 
        select(c(
          columnas_remolques,
          seq(
            (which(names(.) == 'Enero')),
            length.out = 24
          ) %>% 
            set_names(months_cols)
        ))
    )
  ) %>% 
  unnest(data) %>% 
  mutate(
    across(ends_with('month_cost'), ~ifelse(.x == 0, NA, .x)),
    capacidad = as.numeric(str_extract(capacidad, '\\d+')),
    category = substr(category, 1, 1)
  )

remolques_dolly <- remolques_base %>% 
  filter(
    trailer_type == 'Dolly' &
      contract_status %in% c('Vigente', 'Renovacion') & 
      unit_status == 'Activo' 
  ) %>% 
  inner_join(dc_rem_region, by = 'region') %>%
  group_by(whse_nbr) %>%
  summarise(
    across(ends_with('month_cost'), sum, na.rm = TRUE),
    full_mult = 2,
    .groups = 'drop'
  ) %>% 
  pivot_longer(
    cols = ends_with('_month_cost'),
    names_to = 'month', 
    values_to = 'dolly_monthly_cost'
  ) %>% 
  mutate(month = str_extract(month, '.{3}'))

remolques_rem <- remolques_base %>% 
  filter(
    trailer_type == 'Remolque' &
      contract_status %in% c('Vigente', 'Renovacion') & 
      unit_status == 'Activo' 
  )

remolques_costo_capacidad <- remolques_rem %>%
  mutate(
    year_cost = rowMeans(select(., ends_with('month_cost')), na.rm = TRUE)
  ) %>% 
  group_by(capacidad, category) %>%
  summarise(avg_year_cost_capacidad = mean(year_cost, .groups = 'drop', na.rm = TRUE))

remolques <- remolques_rem %>%
  group_by(category, capacidad, region) %>%
  summarise(
    across(ends_with('month_cost'), list(sum = sum, mean = mean), .names = '{col}_{fn}', na.rm = TRUE),
    .groups = 'keep'
  ) %>%
  # Hay tres regiones en la base de remolques que no están en nuestro diccionario y este inner join quita (BKHL, CMA, IDC), representan 10% del costo total de la base para febrero
  inner_join(dc_rem_region, by = 'region') %>%
  mutate(
    grupo_remolque = paste(
      whse_nbr,
      category,
      capacidad,
      sep = '-'
    ),
    group_existing_keys = sum(grupo_remolque %in% existing_grupos_remolques)
  ) %>% 
  # no quitar aquellas grupo_remolques de las que ninguna a nivel category-capacidad-region esta en las remkeys de ded
  filter(grupo_remolque %in% existing_grupos_remolques | group_existing_keys == 0) %>%
  mutate(across(ends_with('month_cost_sum'), ~.x/n())
  ) %>% 
  ungroup() %>%
mutate(
    avg_year_cost_region = rowMeans(select(., ends_with('month_cost_mean')), na.rm = TRUE)
  ) %>% 
  select(-ends_with('month_cost_mean')) %>%
  pivot_longer(
    cols = ends_with('month_cost_sum'),
    names_to = 'month',
    values_to = 'group_remolque_cost',
    names_pattern = '(.*)_month_cost_sum'
  )

remol_tot <- remolques %>% 
  filter(month == mes_de_lectura) %>% 
  anti_join(bound_data, by = c('grupo_remolque', 'month')) %>% 
  group_by(whse_nbr) %>% 
  summarise(tot_remol = sum(group_remolque_cost))

# Procesamiento final ---------------------------------------------------------------

processed_data <- bound_data %>% 
  left_join(
    remolques %>% 
      select(grupo_remolque, month, group_remolque_cost, avg_year_cost_region),
    by = c('grupo_remolque', 'month')
  ) %>% 
  left_join(
    remolques_costo_capacidad,
    by = c('capacidad_transporte' = 'capacidad', 'tipo_remolque' = 'category')
  ) %>% 
  left_join(
    remol_tot,
    by = c('whse_nbr')
  ) %>% 
  left_join(
    select(
      gasto_variable,
      c(determinante, month, quintas, monitoreo, quintas_km, monitoreo_km)
    ),
    by = c('control_gv' = 'determinante', 'month')
  ) %>% 
  left_join(
    select(
      gasto_variable,
      c(determinante, month, gasto_variable, gasto_variable_km)
    ),
    by = c('whse_nbr' = 'determinante', 'month')
  ) %>% 
  left_join(
    remolques_dolly, 
    by = c('whse_nbr', 'full_mult', 'month')
  ) %>% 
  left_join(
    reclamos,
    by = c('transportista', 'whse_nbr_cargo')
  ) %>%
  left_join(
    aclaraciones,
    by = c('transportista', 'whse_nbr_cargo')
  ) %>%
  left_join(
    regresos,
    by = c('transportista', 'whse_nbr_cargo')
  ) %>%
  group_by(transportista, whse_nbr_cargo) %>%
  mutate(
    total_reclamos = reclamos / n(),
    total_aclaraciones = aclaraciones / n(),
    total_regresos = regresos / n(),
    total_tractores_regresos = tractores_regresos / n()
  ) %>% 
  group_by(grupo_remolque) %>%
  # Prorateo del costo del remolque
  mutate(
    time_remolque_share = (delivery_time * full_mult) / sum(delivery_time * full_mult),
    trip_remolque_cost = group_remolque_cost * time_remolque_share
  ) %>% 
  group_by(whse_nbr) %>% 
  mutate(remolques_falt = tot_remol / n()) %>% 
  group_by(whse_nbr, full_mult) %>% 
  # Prorateo del costo de los dollies
  mutate(
    time_dolly_share = delivery_time / sum(delivery_time),
    costo_final_dolly = dolly_monthly_cost * time_dolly_share
  ) %>% 
  group_by(control_gv, store_nbr) %>% 
  mutate(entregas_tienda = n()) %>% 
  group_by(control_gv) %>% 
  # Prorateo del costo de las quintas y monitoreos (solo viajes ded con remolque)
  mutate(
    gv_share = entregas_tienda / sum(entregas_tienda),
    quintas_distribuido = quintas * gv_share,
    monitoreo_distribuido = monitoreo * gv_share
  ) %>% 
  group_by(whse_nbr, store_nbr) %>% 
  mutate(entregas_tienda_completo = n()) %>% 
  group_by(whse_nbr) %>% 
  # Prorateo del gasto variable
  mutate(
    gv_share_completo = entregas_tienda_completo / sum(entregas_tienda_completo),
    gasto_variable_distribuido = gasto_variable * gv_share_completo
  ) %>% 
  ungroup() %>% 
  mutate(
    # Cálculo teórico del costo del remolque
    costo_remolque = ifelse(
      is.na(grupo_remolque),
      0,
      coalesce(avg_year_cost_region, avg_year_cost_capacidad)
    ),
    tarifa_por_hora_remolque = costo_remolque / (30.4 * 24),
    costo_final_remolque = full_mult * delivery_time * tarifa_por_hora_remolque,
    # Cálculo del GV a partir del costo por km
    gasto_variable_calculado = gasto_variable_km * km_total_viaje,
    quintas_calculado = quintas_km * km_total_viaje,
    monitoreo_calculado = monitoreo_km * km_total_viaje,
    across(
      c(
        total_reclamos:gasto_variable_distribuido, 
        costo_final_remolque:monitoreo_calculado
      ), 
      replace_na, 0
    ),
    trip_remolque_cost_final = trip_remolque_cost + remolques_falt,
    costo_final_tractor = final_trip_tractor_cost + total_tractores_regresos,
    # Total teórico
    total_cost_teorico = costo_total_spt + costo_final_tractor + costo_casetas + trip_remolque_cost_final + trip_hub_cost + costo_final_dolly + gasto_variable_calculado + quintas_distribuido + monitoreo_distribuido + total_reclamos + total_aclaraciones + total_regresos
  )


# Guardado de datos -------------------------------------------------------

data <- list(
    processed_data = processed_data,
    processed_month = mes_de_lectura,
    processed_month_num = pull(months_nbrs[months_nbrs$month == mes_de_lectura, 1])
  )

metadata <- list(
    data_file_name = sprintf(
      '%s-[%s]-(%02d)%s-processing_result', 
      Sys.Date(), 
      processing_code_version,
      data$processed_month_num,
      data$processed_month
    ),
    processing_code_version = processing_code_version,
    processing_date = Sys.time()
  )

saveRDS(
  list(data = data, metadata = metadata), 
  sprintf('data/%s', metadata$data_file_name)
)


# Procesamiento de PxV ----------------------------------------------------

pre_pxv <- processed_data %>% 
  filter(
    # No flota dedicada
    esquema == 'SENCILLO'
  )

pxv <- pre_pxv %>% 
  special_summarise(region, whse_nbr, store_nbr, transportista, equipment_type, key)

pxv %>% 
  saveRDS(sprintf('data/%s-pxv', Sys.Date()))

pxv %>% 
  write_csv(sprintf('output/%s-pxv.csv', Sys.Date()))


# Pre-procesamiento del dedicado ----------------------------------------------

pre_ded <- processed_data %>% 
  filter(
    esquema == 'DEDICADO' 
  )

ded <- pre_ded %>% 
  special_summarise(region, whse_nbr, store_nbr, transportista, equipment_type, key)

ded %>% 
  saveRDS(sprintf('data/%s-ded', Sys.Date()))
