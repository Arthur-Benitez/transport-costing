# Prueba de entregas múltiples --------------------------------------------
x <- processed_data %>% 
  group_by(whse_nbr, store_nbr, equipment_type) %>% 
  filter(n_distinct(n_stores) > 1)

set.seed(1234)
y <- sample(unique(x$store_nbr), 16)

x %>% 
  filter(store_nbr %in% y) %>% 
  ggplot(aes(x = n_stores, y = costo_total)) + 
  geom_point() +
  facet_wrap(facets = ~store_nbr)

x %>% 
  filter(store_nbr == 2840) %>% view

# Lo que importa son los km (por CD, equipo), no la tienda en particular, porque además las rutas pueden variar.
# Hay tarifas del costo_prov que ya incluyen el diesel, vamos a ocupar el costo_total para que sean comparables.
# Vamos a grupar por ruta (cd-all_stores-equipo) para que los viajes en una misma ruta tengan distancias extremadamente similares o iguales. Si sólo agrupamos por la primera tienda, la distancia promedio puede variar mucho por las tiendas subsecuentes.


# Validaciones ----------------------------------------------------------------
ed <- read_excel('data/20200505-base-datos-erick.xlsx') %>% 
  mutate(
    store_nbr = substr(key, 5, 8),
    our_key = paste(
      as.numeric(substr(key, 1, 4)),
      as.numeric(substr(key, 5, 8)),
      nomenclatura,
      str_replace_all(clave_gts, ' ', '_'),
      sep = '-'
    )
  )

w <- result %>% 
  mutate(
    # store_nbr = substr(key, 5, 8),
    our_key = paste(
      as.numeric(substr(key, 1, 4)),
      as.numeric(substr(key, 5, 8)),
      nomenclatura,
      str_replace_all(clave_gts, ' ', '_'),
      sep = '-'
    )
  ) %>% 
  arrange(rank_impacto_gasto) %>% 
  pull(our_key)

q <- pxv %>% 
  full_join(
    y = ed %>% 
      rename(c(ed_whse_nbr = whse_nbr, ed_store_nbr = store_nbr)),
    by = 'key',
    suffix = c('_nuestro', '_suyo')
  ) %>% 
  select(
    key,
    # sólo en nuestra base
    whse_nbr,
    store_nbr,
    transportista,
    equipment_type,
    # sólo en la base de erick
    ed_whse_nbr,
    ed_store_nbr,
    nomenclatura,
    clave_gts,
    # datos
    starts_with('n_viajes'),
    total_diesel,
    total_gasto_nuestro,
    total_prov,
    total_gasto,
    total_gasto_suyo,
    # starts_with('total_gasto'),
    total_km,
    starts_with('avg_km'),
    starts_with('costo_por_viaje'),
    starts_with('costo_por_km')
  ) %>% 
  mutate(
    base = case_when(
      is.na(n_viajes_suyo) ~ 'base_viajes',
      is.na(n_viajes_nuestro) ~ 'base_erick',
      TRUE ~ 'ambas'
    ),
    viajes_iguales = n_viajes_nuestro == n_viajes_suyo,
    gasto_igual = abs(total_gasto - total_gasto_suyo)/total_gasto_suyo <= 0.03,
    avg_km_igual = abs(avg_km_nuestro - avg_km_suyo)/avg_km_suyo <= 0.03,
    costo_viaje_igual = abs(costo_por_viaje_nuestro - costo_por_viaje_suyo)/costo_por_viaje_suyo <= 0.03,
    costo_km_igual = abs(costo_por_km_nuestro - costo_por_km_suyo)/costo_por_km_suyo <= 0.03
  )

q %>% 
  count(base, viajes_iguales) %>% 
  mutate(perc = n / sum(n) * 100)
# arrange(desc(viajes_iguales)) 

### Comparativa de gráficas nls vs lm
a <- result %>% filter(whse_nbr == 9429 & clave_gts == 'TR S 53') %>% select(key, costo_por_km, avg_km, pred)
b <- pxv_pred %>% filter(whse_nbr == 9429 & equipment_type == 'TR S 53') %>% select(key, costo_por_km, avg_km, pred)
c <- full_join(a, b, 'key', suffix = c('_nls', '_lm')) %>% select(key, starts_with('pred'), avg_km_lm,) %>% arrange(avg_km_lm,) %>% mutate(pred_dif = pred_nls - pred_lm) %>% view()


### Revisión de unidas con viajes diferentes
q %>% 
  filter(base == 'ambas' & !viajes_iguales) %>% 
  count(
    avg_km_igual,
    gasto_igual,
    costo_viaje_igual,
    costo_km_igual
  ) %>% 
  mutate(perc = round(n / sum(n) * 100, 1)) 

# No se encontró un patrón generalizable para detectar cuales viajes no considera Erick, pero no es completamente necesario igualar los viajes, mientras los costos marginales se mantengan iguales
# Cuando los costos marginales son diferentes, usualmente también es porque el no considera algunos viajes y los costos cambian entre viajes, por el número de entregas. Pareciera que el patrón es quitar viajes con ciertos estatus de pagos, pero no es generalizable (supongo que no es el único criterio).
# También hay casos en los que él tiene más viajes que nosotros, no sé porqué
# Los que discrepan con estas causas se pueden agrupar a que son como el 17% de este subconjunto.
# Los que ya son (suficientemente) iguales (considerando la diferencia en viajes), es como el 80%
q %>% 
  filter(
    base == 'ambas' & 
      !viajes_iguales &
      !avg_km_igual & 
      !gasto_igual & 
      costo_viaje_igual &
      !costo_km_igual
  ) %>% 
  view()

### Revisión de las que ya tienen viajes iguales
q %>% 
  filter(base == 'ambas' & viajes_iguales) %>% 
  count(
    avg_km_igual,
    gasto_igual,
    costo_viaje_igual,
    costo_km_igual
  ) %>% 
  mutate(perc = round(n / sum(n) * 100, 1))

count(filter(q, base != 'base_erick'), total_prov == 0, total_diesel == 0)

q %>% 
  filter(
    base == 'ambas' & 
      viajes_iguales &
      avg_km_igual &
      !gasto_igual
  ) %>% 
  view()

### Revisión de las que faltan en la base de erick
w <- q %>% 
  filter(
    base == 'base_viajes'
  ) %>% 
  # view()
  pull(key) %>%
  unique()

processed_data %>% 
  filter(
    key %in% (q %>% 
                filter(base == 'base_viajes') %>% 
                pull(key)) & 
      !(whse_nbr %in% ed$whse_nbr)
  ) %>% 
  count(whse_nbr) %>% 
  arrange(desc(n)) %>% 
  summarise(sum(n))
# 98% de los registros de processed data que no están en la base de erick, es porque sus CD no están en la base, que representan 945 de las 984 llaves
# (9809 8063 9808 9807 7761 8037 9675 6012 7489)
# De los restantes 39, 9 son porque las tiendas no estan en la base de erick (aperturas?)
# (2845 1464 3184 3455  155 1763 1634)
# De los restantes 30, 2 son porque el transportista no esta en la base de erick
# (AJRVTP)
q %>% 
  filter(
    base == 'base_viajes' & 
      (whse_nbr %in% ed$whse_nbr) & 
      (store_nbr %in% ed$store_nbr) &
      (transportista %in% ed$nomenclatura)
  ) %>% 
  pull(key)
# count(transportista) %>% 
# count(whse_nbr) %>%
# count(whse_nbr, store_nbr) %>%
# arrange(desc(n))
# summarise(sum(n))

### Los que faltan en la nuestra
# De los de que sólo están en la base de Erick, la mayoría son del transportista TGALTP, de los que hay muy pocos en la base de viajes, y ka mayoría de los whse_nbr (9811, 8033)
# De los 50 que están en la base de erick y no en la nuestra, 3 tienen 8032 tanto en el CD como en la tienda. Nosotros todos los viajes que tenemos con el CD 8032, son de viajes dedicados.
# De los 47 cuyos CD sí tenemos en nuestra base, hay 26 cuyas tiendas no están en la base PXV, de los cuales 7 sí tienen la alineación de CD-tienda correcta en la base original, pero 4 son de flota dedicada y los otros 3 son viajes de tipo "Especial Cedis".
# (8033-1474 9811-1952 9811-1435 9811-2817 9811-1983 8033-5619 8033-4709)
# Los restantes 19 de esos 26 son porque la ruta CD-tienda ni siquiera está en la base.
# (8033-5437 9811-1528 9811-1639 9811-1919 9811-2093 9811-2289 9811-2469 9811-2583 9811-2592 9811-2608 9811-2877 9811-3053 9811-3109 9811-3551 9811-3656 9811-3941 9811-4131 9811-4132 9811-4636)
# De los otros 21 de 47 cuyas tiendas sí están en la base de PXV, hay 1 ruta que no tiene la alineación CD en la base original
# (9811-2286)
# Los otros 20 sí tienen una alineación correcta, pero perdemos 1 porque son de esquemas dedicados, los demás no tienen el mismo transportista para esa ruta, aunque en algunos casos sólo difiere una letra
# (8033-1856-TCMDTP-TH_S_20 -> 8033-1856-TCMDTD-TH_S_20), será otra nomenclatura dentro del nombre del proveedor?
a <- q %>% 
  filter(
    base == 'base_erick' 
    # (ed_whse_nbr %in% pxv$whse_nbr) & 
    # (ed_store_nbr %in% pxv$store_nbr)
    # (nomenclatura %in% pxv$transportista)
  ) %>% 
  count(nomenclatura)
# pull(key)
# count(transportista) %>%
# count(ed_whse_nbr) %>%
mutate(cd_store = substr(key, 1, 9)) %>%
  count(cd_store, key, nomenclatura) %>%
  view()
pull(cd_store)
# count(ed_store_nbr) %>%
# count(ed_whse_nbr, ed_store_nbr, key) %>%
arrange(desc(n))
# summarise(sum(n))

processed_data %>% 
  mutate(cd_store = substr(key, 1, 9)) %>%
  filter(cd_store %in% w) %>%
  # filter(key %in% w) %>% 
  # pull(cd_store) %>%
  # pull(key) %>%
  # unique()
  group_by(esquema, tipo_origen, tipo_viaje) %>% 
  summarise(cd_stores = n_distinct(cd_store))

e <- processed_data %>% 
  mutate(cd_store = substr(key, 1, 9)) %>% 
  filter(cd_store %in% w) %>% 
  pull(cd_store) %>% 
  unique()

f <- processed_data %>% 
  mutate(cd_store = substr(key, 1, 9)) %>% 
  filter(
    cd_store %in% w & 
      esquema == 'SENCILLO'
  ) %>% 
  pull(cd_store) %>% 
  unique()

w[(w %in% f)]

a %>% 
  filter((cd_store %in% w[(w %in% f)]))

### Compilación de la comparativa vs base de erick
q %>% 
  # filter(base == 'ambas') %>% 
  count(
    base,
    viajes_iguales,
    avg_km_igual,
    gasto_igual,
    costo_viaje_igual,
    costo_km_igual
  ) %>% 
  mutate(perc = round(n / sum(n), 1)) %>% 
  copy()

### Validación cambio de filtro store_nbr2
a %>% 
  full_join(
    y = b,
    by = 'key',
    suffix = c('_filt', '_todos')
  ) %>% 
  select(
    key,
    starts_with(c('base', 'viajes_iguales'))
  ) %>% 
  mutate(
    clas = case_when(
      is.na(base_filt) ~ 'todos',
      is.na(base_todos) ~ 'filt',
      TRUE ~ 'ambas'
    )
  ) %>% 
  count(clas)

b$n_viajes_nuestro[b$key == '9809-9819-TRECMR-TR-R-48']

# Nuevos en común
b %>%
  filter(
    # base == 'ambas' & 
    !(key %in% a$key)
  ) %>% 
  # pull(key)
  count(base, viajes_iguales)

### Compilación de casos
q %>% 
  group_by(base, viajes_iguales) %>% 
  mutate(n_row = row_number(key)) %>% 
  arrange(n_row, .by_group = TRUE) %>% 
  ungroup() %>% 
  filter(n_row <= 5) %>% 
  select(key, base, viajes_iguales, everything(), -n_row) %>% 
  # write_csv('output/problem-samples.csv')
  view()

### Suponiendo que tienen el mismo costo
a %>% 
  filter(base == 'ambas' & viajes_iguales == TRUE) %>% 
  mutate(
    total_gasto_nuestro = total_gasto_suyo,
    costo_por_viaje_nuestro = total_gasto_nuestro / n_viajes_nuestro,
    costo_por_km_nuestro = total_gasto_nuestro / total_km,
    costo_por_viaje_iguales = costo_por_viaje_nuestro == costo_por_viaje_suyo,
    costo_por_km_iguales = costo_por_km_nuestro == costo_por_km_suyo
  ) %>% 
  view()

### Misc

lapply(list(pxv, ed, a), nrow)
lapply(list(pxv_filt, pxv_todos, a, b), dim)

all(pull(ca_or, key) == pull(ad_or, key))

pxv %>% 
  filter(
    det_admin == 5726 &
      det_origen == 19 &
      equipment_type == 'TR S 53'
  ) %>% 
  view()

filter(a, key == '5726-19-AEGMTP-TR-S-53') %>% view()

f <- pxv_model %>% 
  mutate(
    columnas = unlist(map(data, ncol)),
    filas = unlist(map(data, nrow))
  ) %>% 
  ungroup()

f %>% 
  mutate(exitoso = filas >= 10) %>% 
  group_by(columnas, exitoso) %>% 
  summarise(
    cuenta = n()
  ) %>% 
  ungroup() %>% 
  filter(exitoso == TRUE | columnas == 9) %>%
  mutate(
    perc = cuenta / sum(cuenta),
    clas = case_when(
      columnas == 9 ~ 'exito',
      # exitoso == FALSE ~ 'NA',
      TRUE ~ 'fracaso'
    )
  ) %>% 
  group_by(clas) %>% 
  summarise(perc = sum(perc))

f %>% 
  # tuvo éxito o tenía probabilidad de éxito
  filter(filas >= 10 | columnas == 9) %>%
  mutate(
    clas = case_when(
      columnas == 9 ~ 'exito',
      # filas < 10 ~ 'NA',
      TRUE ~ 'fracaso'
    )
  ) %>% 
  group_by(clas) %>% 
  summarise(cuenta = n()) %>% 
  ungroup() %>% 
  mutate(perc = cuenta / sum(cuenta))


digits <- function(x){
  floor(log10(abs(x))) + 1
} 

is_key <- function(x, col_name){
  col_name <- deparse(substitute(col_name))
  n_distinct(x[[col_name]]) == nrow(x)
}

find_keys <- function(x){
  row_numbers <- x %>% 
    apply(2, n_distinct)
  keys <- names(which(row_numbers == nrow(x)))
  if(length(keys) == 0) NA
  else keys
}

copy <- function(x, names = TRUE){
  x %>% 
    write.table(
      file = 'clipboard',
      sep = '\t',
      row.names = FALSE,
      col.names = names
    )
}

compare_cols <- function(left, right) {
  list(
    'left'  = setdiff(names(left), names(right)),
    'right' = setdiff(names(right), names(left))
  )
}

list_function <- function(x, fun, ...){
  unlist(lapply(x, fun, ...))
}

test_join <- function(left, right, key_cols){
  dfs <- list('left' = left, 'right' = right)
  keys <- dfs %>% 
    lapply(function(x){
      x %>% 
        select(key_cols) %>%
        apply(1, paste, collapse = '-')
    })
  n_keys <- list_function(keys, n_distinct)
  n_rows <- list_function(dfs, nrow)
  difs <- n_keys != n_rows
  res <- list(
    'left_join' = left %>% 
      left_join(right, by = key_cols) %>% 
      dim(),
    'right_join' = left %>% 
      right_join(right, by = key_cols) %>% 
      dim(),
    'inner_join' = left %>% 
      inner_join(right, by = key_cols) %>% 
      dim(),
    'full_join' = left %>% 
      full_join(right, by = key_cols) %>% 
      dim(),
    'n_rows' = n_rows,
    'n_cols' = list_function(dfs, ncol)
  )
  if(any(n_keys != n_rows)){
    warning('The columns given are not keys in at least one of the data frames.', call. = FALSE)
    # return(list('n_keys' = n_keys[difs], 'n_rows' = n_rows[difs]))
    res$keys <- n_keys
  }
  res
}

# Anotaciones:
# avg_km, total_km son iguales
# cambia el gasto total y n_viajes (a veces tenemos más (67%) a veces menos (33%))
# cambian algunas rutas, pero se parecen las graficas


# Filtrar tipo viaje in outbound, center point, porteo
# tipo origen = CDW
# digits(ent1) <= 4


# Variables que fueron descartadas
names(original_data)[!(names(original_data) %in% variables)]

# Análisis de costos
pxv_pred %>% 
  # filter(costo_por_km < 1)
  filter(grupo_comparable == '8033-TH_S_20') %>%
  # good_plot()
  mutate(
    dot_color = ifelse(key %in% a, 'red', 'black'),
    impacto_gasto = ifelse(is.na(impacto_gasto), 1000, impacto_gasto)
  ) %>%
  ggplot(aes(avg_km)) +
  geom_point(aes(y = costo_por_km, size = abs(impacto_gasto), color = dot_color), alpha = 0.4) +
  geom_line(aes(y = pred), color = 'blue') +
  scale_color_identity() +
  labs(
    title = '8033-TH_S_20',
    x = 'Kilómetros promedio por viaje',
    y = 'Costo por kilómetro'
  )

#### Comparativa lm vs rlm
lm <- pxv %>% 
  group_by(whse_nbr, equipment_type) %>% 
  nest() %>% 
  mutate(
    model = map(data, function(x){
      safely(lm)(
        costo_por_km ~ I(1/avg_km),
        data = x
      )$result
    }),
    data = map2(data, model, ~{
      if (!is.null(.y)) {
        .x$pred <- predict(.y, .x)
        .x$intercept <- .y$coefficients[1]
        .x$slope <- .y$coefficients[2]
      }
      .x
    })
  ) %>% 
  select(-model) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(
    diff_vs_pred = costo_por_km - pred,
    diffp_vs_pred = diff_vs_pred / pred,
    gasto_pred = n_viajes * avg_km * pred,
    impacto_gasto = n_viajes * avg_km * diff_vs_pred,
    impacto_gasto_porc = impacto_gasto / gasto_pred,
    rank_impacto_gasto = row_number(desc(impacto_gasto))
  )

nlm <- pxv %>% 
  group_by(whse_nbr, equipment_type) %>% 
  nest() %>% 
  mutate(
    model = map(data, function(x){
      safely(rlm)(
        costo_por_km ~ I(1/avg_km),
        data = x,
        psi = MASS::psi.huber
      )$result
    }),
    data = map2(data, model, ~{
      if (!is.null(.y)) {
        .x$pred <- predict(.y, .x)
        .x$intercept <- .y$coefficients[1]
        .x$slope <- .y$coefficients[2]
        .x$residual_weight <- .y$weights
      }
      .x
    })
  ) %>% 
  select(-model) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(
    diff_vs_pred = costo_por_km - pred,
    diffp_vs_pred = diff_vs_pred / pred,
    gasto_pred = n_viajes * avg_km * pred,
    impacto_gasto = n_viajes * avg_km * diff_vs_pred,
    impacto_gasto_porc = impacto_gasto / gasto_pred,
    rank_impacto_gasto = row_number(desc(impacto_gasto))
  )

rlm %>% 
  inner_join(
    y = select(lm, c(key, pred, intercept, slope, diff_vs_pred)),
    by = 'key',
    suffix = c('_rlm', '_lm')
  ) %>% 
  mutate(
    pred_diff = (pred_rlm - pred_lm),
    intercept_diff = (intercept_rlm - intercept_lm),
    slope_diff = (slope_rlm - slope_lm),
    pred_diffp = ((pred_rlm - pred_lm) / pred_lm),
    intercept_diffp = ((intercept_rlm - intercept_lm) / intercept_lm),
    slope_diffp = ((slope_rlm - slope_lm) / slope_lm)
  ) %>% 
  select(
    whse_nbr,
    equipment_type,
    store_nbr,
    transportista,
    grupo_comparable,
    key,
    residual_weight,
    starts_with('pred'),
    starts_with('intercept'),
    starts_with('slope')
    # starts_with('diff_vs_pred')
  ) %>% 
  # write_csv(sprintf('output/%s_model-comparison.csv', Sys.Date()))
  summary() %>% 
  copy()


# Procesamiento de texto de ventanas de recibo ----------------------------
library(tidyverse)
# library(readxl)

a <- readxl::read_excel('data/ventanas_recibo_tienda.xlsx')

names_list <- paste(
  rep_len(c('start', 'stop'), length.out = 7),
  rep(1:4, each = 2, length.out = 7), sep = '_'
)

rec_stores <- a %>% 
  rename(c(
    store_nbr = `Determinante tienda`,
    dc_name = 'Cedis',
    receiving_time = 'Ventanas'
  )) %>% 
  # filter(
  #   (is.na(as.numeric(receiving_time)) |
  #     as.numeric(receiving_time) >= 1) &
  #     receiving_time != 'APERTURA'
  # ) %>%
  mutate(
  all_receiving =
    map(
      str_extract_all(receiving_time, '\\d+:\\d+'),
      function(x){
        if(length(x) == 0) NA
        else paste(x, collapse = ',')
      }
    ), 
  all_receiving = 
    case_when(
      is.na(all_receiving) ~ 
        map(
          str_extract_all(receiving_time, '\\d+'),
          function(x){
            if(length(x) == 0) NA
            else paste(x, collapse = ',')
          }
        ),
      TRUE ~ all_receiving
    )
  ) %>% 
  separate(
    col = all_receiving,
    into = names_list,
    sep = ',',
    remove = FALSE,
    convert = TRUE,
    extra = 'drop',
    fill = 'right'
  ) %>% 
  mutate_at(
    vars(starts_with('start'), starts_with('stop')),
    function(x){
      case_when(
        is.na(lubridate::parse_date_time(x, orders = 'H:M')) ~ 
          lubridate::parse_date_time(x, orders = 'H'),
        TRUE ~ 
          lubridate::parse_date_time(x, orders = 'H:M')
      )
    }
  ) %>% 
  mutate(
    period_1 = case_when(
      receiving_time == 'VENTANA LIBRE' ~ 
        as.difftime('24', format = '%H', units = 'hours'),
      TRUE ~ difftime(stop_1, start_1, units = 'hours')
    ),
    period_2 = difftime(stop_2, start_2, units = 'hours'),
    period_3 = difftime(stop_3, start_3, units = 'hours')
  ) %>% 
  mutate_at(
    vars(starts_with('period')),
    function(x){
      case_when(
        x < 0 ~ x + 24,
        TRUE ~ x
      )
    }
  ) %>% 
  # view()
  select(-all_receiving) %>% 
  write_csv('output/reception_times.csv')

a %>% 
  rename(c(
    store_nbr = `Determinante tienda`,
    dc_name = 'Cedis',
    receiving_time = 'Ventanas'
  )) %>% 
  filter(
       as.numeric(receiving_time) < 1 | 
      receiving_time == 'APERTURA'
  ) %>% nrow()

# unnest(all_receiving) %>% 
# group_by(store_nbr) %>% 
# mutate(
#   col_name = paste(
#     rep_len(c('start', 'stop'), length.out = n()),
#     rep(1:4, each = 2, length.out = n()),
#     sep = '_'
#   ),
#   nrow = n()
# ) %>% 
  # spread(store_nbr, all_receiving)


# Volúmenes y viajes x tienda ---------------------------------------------

# grecia <- readxl::read_excel('analyses/20200608_costo_caja_x_tienda_v05.xlsx', skip = 1)

a <- processed_data %>% 
  filter(
    esquema == 'DEDICADO' & 
      !str_detect(whse_name, 'FYV') & 
      !(whse_nbr %in% c(9675, 8063, 8037, 9808, 9809, 7450, 9430, 9807, 7761, 6012))
  ) %>% 
  select(
    id_viaje,
    store_nbr,
    all_store_nbrs,
    all_store_nbrs_vector,
    n_stores
  ) %>%
  unnest(all_store_nbrs_vector) %>% 
  group_by(all_store_nbrs_vector) %>% 
  summarise(
    n_viajes = sum(1 / n_stores)
  ) %>% 
  write_csv('data/20200609-viajes_x_tienda.csv')

# Gastos distribuidos por tienda de acuerdo al kilometraje ----------------

distances <- read_csv('data/20200612-store_distances.csv') %>% 
  set_names(c('store_nbr', 'km_ida', 'km_regreso', 'km_total'))

processed_data %>% 
  filter(
    esquema == 'DEDICADO' & 
      tipo_viaje == 'OUTBOUND' & 
      !str_detect(whse_name, 'FYV') & 
      !(whse_nbr %in% c(9675, 8063, 8037, 9808, 9809, 7450, 9430, 9807, 7761, 6012))
  ) %>% 
  select(
    id_viaje,
    whse_nbr,
    transportista,
    equipment_type,
    store_nbr,
    # all_store_nbrs,
    all_store_nbrs_vector,
    costo_prov,
    costo_diesel,
    costo_total
  ) %>%
  unnest(all_store_nbrs_vector) %>% 
  left_join(
    y = distances,
    by = c('all_store_nbrs_vector' = 'store_nbr')
  ) %>% 
  group_by(id_viaje) %>% 
  mutate(
    costo_prov_dis = costo_prov * (km_total / sum(km_total)),
    costo_diesel_dis = costo_diesel * (km_total / sum(km_total)),
    costo_total_dis = costo_total * (km_total / sum(km_total))
  ) %>% 
  select(-store_nbr) %>%
  rename(c('store_nbr' = 'all_store_nbrs_vector')) %>% 
  write_csv('output/20200612-distributed_cost.csv')


# Ideas para calculo de costos de remolques -------------------------------

# Para que los costos no dependan de la cantidad del día del mes, hay que sacar un costo "promedio" anual
raw_data_remolques %>% 
  select(c(columnas_remolques, days_cols)) %>% 
  mutate(
    daily_rent_cost = case_when(
      rent_period == 'Mensual' ~ (rent_cost * 12)/365,
      TRUE ~ rent_cost
    ),
    monthly_maint_cost = case_when(
      maint_period == 'Diario' ~ (maint_cost * 365)/12,
      TRUE ~ maint_cost
    ),
    monthly_insur_cost = case_when(
      insur_period == 'Diario' ~ (insur_cost * 365)/12,
      TRUE ~ insur_cost
    ),
    across(
      ends_with('_days'),
      .fns = function(x) {}
    )
  )

cd_data <- read_csv('data/cd_data.csv') %>% 
  set_names(c(
    'texto1',
    'whse_code',
    'whse_name',
    'red',
    'operacion',
    'otro',
    'determinante',
    'determinante_gls',
    'whse_nbr',
    'whse_code2',
    'whse_name2',
    'red_2'
  ))

# Entregable preliminar junio 19 ------------------------------------------

# Detectar manualmente los outliers de flota dedicada
outliers <- c(
  '6012-6263-GAPSTD-TR_R_53',
  '6012-6396-GTRATD-TR_R_53',
  '8035-3204-MEOCTD-CA_S_4',
  '9809-1819-AEBITD-TH_R_20',
  '9809-2982-AEBITD-TH_R_20'
)

ded_pred %>% 
  filter(
    grupo_comparable == '9809-TH_R_20' &
      costo_por_km > 60 & 
      avg_km >700
  ) %>% 
  pull(key)
  # view
  
xlsx::write.xlsx(
  ded_pred %>% 
    select(whse_nbr:key, n_viajes:costo_por_km) %>% 
    mutate(outlier = ifelse(key %in% outliers, "SI", 'NO')) %>% 
    set_names(c(
      'CD', 'Tienda', 'Camión', 'Transportista', 'CD-Camión', 'Llave', '# de Viajes', 'Total gasto de ruta', 'Km promedio', 'Total km de ruta', 'Costo por viaje', 'Costo por km', 'Outliers'
    )),
  file = 'output/20200619-costeo-dedicada.xlsx',
  # row.names = FALSE,
  showNA = FALSE
)


# Validación de joins de remolques usando CD ------------------------------

# Estas 34 llaves tienen 30% del costo y si están en la base de ded
si_estan_rem <- remolques %>% 
  filter(rem_key %in% ded$rem_key) %>% 
  pull(rem_key)

# 30% del costo
b <- remolques %>% 
  filter(rem_key %in% si_estan_rem) %>% 
  pull(group_remolque_cost) %>% 
  sum()

si_estan_ded <- ded %>% 
  filter(rem_key %in% remolques$rem_key) %>% 
  pull(rem_key) %>% 
  unique()

comunes <- intersect(remolques$rem_key, ded$rem_key)

rems <- remolques %>% 
  select(rem_key, group_remolque_cost)

deds <- ded %>% 
  select(rem_key, equipment_type, tipo_remolque, group_remolque_cost, time_remolque_share, trip_remolque_cost) %>% 
  rename(c('group_remolque_cost0' = 'group_remolque_cost', 'trip_remolque_cost0' = 'trip_remolque_cost'))

c <- deds %>% 
  full_join(rems, 'rem_key') %>% 
  mutate(
    trip_remolque_cost = ifelse(
      is.na(time_remolque_share),
      group_remolque_cost,
      group_remolque_cost * time_remolque_share
    ),
    across(
      starts_with(c(
        'group_remolque_cost', 
        'time_remolque_share',
        'trip_remolque_cost'
      )),
      ~ifelse(is.na(.), 0, .)),
    pega = ifelse(is.na(tipo_remolque), FALSE, TRUE),
    nuevos = group_remolque_cost != 0,
  )

#Si estan en ded_base pero ya no estan en ded
problemas <- setdiff(si_estan_ded, comunes)

c %>% 
  group_by(pega, nuevos) %>% 
  summarise(
    n = n(),
    trip_cost = sum(trip_remolque_cost),
    perc = sum(trip_remolque_cost) / a
  )

# Estas 80 llaves tienen 60% del costo y no están en la base de ded
q <- c %>% 
  filter(!pega & nuevos) %>% 
  pull(rem_key) %>% 
  sort()

# 60% del costo
q_cost <- c %>% 
  filter(!pega & nuevos) %>% 
  pull(group_remolque_cost) %>% 
  sum()

w <- remolques %>% 
  mutate(in_ded = rem_key %in% ded$rem_key) %>% 
  copy()



# Validación doble tendencia y casos raros --------------------------------

# Función para resumir rápido variables importantes

brief <- function(x, .round = 0){
  x <- x %>% 
    group_by(new_key) %>% 
    summarise(
      n_entregas = n(),
      n_viajes = n_distinct(id_viaje),
      rest_time = max(rest_time),
      download_time = max(download_time),
      trip_time = max(trip_time),
      delivery_time = max(delivery_time),
      avg_km = mean(km_ida_viaje),
      total_km = sum(km_ida_viaje),
      costo_spt = sum(costo_total_spt),
      casetas = sum(total_casetas),
      tractor = sum(trip_tractor_cost),
      remolque = sum(trip_remolque_cost),
      total_gasto = sum(total_cost),
      .groups = 'drop'
    ) %>% 
    mutate(
      costo_por_viaje = total_gasto / n_viajes,
      costo_por_km = total_gasto / total_km
    ) %>% 
    arrange(desc(new_key))
  
  if(!isFALSE(.round)){
    x %>% 
      mutate(across(where(is.numeric), round, .round))
  } else x
}

# Función para sustituir el tiempo de descarga y re-procesar la info

reprocess <- function(data, search_time = NA_integer_, repl_time = NA_real_){
  data %>% 
    inner_join(
      stores_data %>% 
        select(
          store_nbr,
          casetas_ida
        ),
      by = 'store_nbr'
    ) %>% 
    unnest(all_store_nbrs_vector) %>% 
    rename(c(
      'primera_entrega' = 'store_nbr',
      'store_nbr' =  'all_store_nbrs_vector'
    )) %>% 
    inner_join(
      stores_data %>% 
        select(
          store_nbr,
          km_total_tienda,
          speed,
          download_time
        ),
      by = 'store_nbr'
    ) %>% 
    inner_join(
      tarifas, 
      by = 'grupo_tractor'
    ) %>% 
    left_join(
      remolques %>% 
        select(grupo_remolque, group_remolque_cost),
      by = 'grupo_remolque'
    ) %>% 
    group_by(id_viaje) %>% 
    mutate(
      km_entrega_share = km_total_tienda / sum(km_total_tienda),
      km_ida_viaje = km_ida_viaje * km_entrega_share,
      km_regreso_viaje = km_regreso_viaje * km_entrega_share,
      km_total_viaje = km_total_viaje * km_entrega_share,
      trip_time = km_ida_viaje / speed,
      # El tiempo de descanso es mucho mejor calcularlo con base en el tiempo real del trayecto que pegarle un supuesto por tienda y luego hacerse bolas en la distribución.
      rest_time = (trip_time %/% 16) * 9,
      download_time = case_when(
        all(is.na(search_time)) | is.na(repl_time) ~ download_time,
        round(download_time) %in% search_time ~ repl_time,
        TRUE ~ download_time
      )
    ) %>% 
    group_by(grupo_tractor) %>% 
    mutate(
      # mientras no sepamos si el download time es correcto, es mejor neutralizar su efecto en los resultados, porque es mucho
      download_mean_time = mean(download_time),
      delivery_time = trip_time + download_mean_time + rest_time,
      time_tractor_share = delivery_time / sum(delivery_time)
    ) %>% 
    group_by(grupo_remolque) %>% 
    mutate(
      full_mult = ifelse(str_detect(equipment_type, 'F'), 2, 1),
      time_remolque_share = (delivery_time * full_mult) / sum(delivery_time * full_mult),
      costo_total_spt = costo_total_spt * km_entrega_share,
      total_casetas = casetas_ida * km_entrega_share,
      trip_tractor_cost = group_tractor_cost * time_tractor_share,
      trip_remolque_cost = case_when(
        is.na(group_remolque_cost) ~ 0,
        TRUE ~ group_remolque_cost * time_remolque_share
      ),
      total_cost = costo_total_spt + trip_tractor_cost + total_casetas + trip_remolque_cost,
      new_key = paste(
        whse_nbr,
        store_nbr,
        transportista,
        str_replace_all(equipment_type, ' ', '_'),
        tipo_remolque,
        capacidad_transporte,
        sep = '-'
      ),
    ) %>% 
    ungroup
}

regroup <- function(data){
  data %>% 
  group_by(region, whse_nbr, store_nbr, transportista, equipment_type, tipo_remolque, capacidad_transporte, new_key, grupo_tractor, grupo_remolque, grupo_comparable) %>% 
    summarise(
      n_entregas = n(),
      n_viajes = n_distinct(id_viaje),
      total_gasto = sum(total_cost),
      avg_km = mean(km_ida_viaje),
      total_km = sum(km_ida_viaje),
      .groups = 'drop'
    ) %>% 
    mutate(
      costo_por_viaje = total_gasto / n_viajes,
      costo_por_km = total_gasto / total_km
    )
}

remodel <- function(data){
  data %>% 
    group_by(grupo_comparable) %>% 
    nest() %>% 
    mutate(
      model = map(data, function(x){
        safely(MASS::rlm)(
          costo_por_km ~ I(1/avg_km),
          data = x,
          psi = MASS::psi.huber
        )$result
      }),
      data = map2(data, model, ~{
        if (!is.null(.y)) {
          .x$pred <- predict(.y, .x)
        }
        .x
      })
    ) %>% 
    select(-model) %>% 
    unnest(data) %>% 
    ungroup() %>% 
    mutate(
      diff_vs_pred = costo_por_km - pred,
      diffp_vs_pred = diff_vs_pred / pred,
      gasto_pred = n_viajes * avg_km * pred,
      impacto_gasto = n_viajes * avg_km * diff_vs_pred,
      rank_impacto_gasto = row_number(desc(impacto_gasto))
    )
}

replot <- function(data, colored_keys = c(), model = FALSE){
  if(model) {
    data <- data %>% 
      mutate(impacto_gasto = ifelse(is.na(impacto_gasto), 1000, impacto_gasto))
  }
  p <- data %>% 
    mutate(
      dot_color = ifelse(new_key %in% colored_keys, 'red', 'black'),
    ) %>% 
    ggplot(aes(avg_km)) +
    facet_wrap(vars(grupo_comparable), scales = 'free') + 
    ylim(0, NA) + 
    labs(
      title = 'Rutas de flota dedicada por CD-Camión',
      x = 'Kilómetros promedio por viaje',
      y = 'Costo por kilómetro'
    ) + 
    scale_color_identity() +
    scale_size_continuous(name = ifelse(model, "Impacto (abs)", 'Viajes'), labels = scales::comma) + 
    theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 15)
    )
  if(model){
    p + 
      geom_point(aes(y = costo_por_km, size = abs(impacto_gasto), color = dot_color), alpha = 0.4) + 
      geom_line(aes(y = pred), color = 'blue')
  } else {
    p + 
      geom_point(aes(y = costo_por_km, size = n_viajes, color = dot_color), alpha = 0.4)
  }
}

# El grupo 7760-THORTON tiene 7 puntos que parecen una tendencia extra, se pueden identificar por avg_km >= 70 y costo >= 50. Vamos a meter estos puntos en el primer grupo de interés. ya vimos que se debe a que los tiempos de descarga son muy altos en comparación a los demás, corrigiendo estos tiempos, el costo se normaliza.
# Vamos a analizar el 9808-SENCILLO. Sucedió lo mismo que con el anterior.

b <- ded_pred %>% 
  mutate(
    interest_group = case_when(
      grupo_comparable == '7760-THORTON' & avg_km >= 70 & costo_por_km >= 50 ~ 1,
      grupo_comparable == '9808-SENCILLO' & avg_km >= 35 & costo_por_km >= 80 ~ 2,
      grupo_comparable == '9807-TH_R_28' & avg_km >= 20 & costo_por_km >= 140 ~ 3,
      grupo_comparable == '9428-SENCILLO' & avg_km >= 6 & costo_por_km >= 130 ~ 4,
      TRUE ~ 0,
    )
  )

grupo_analizado <- '9428-SENCILLO'

# Estas son las llaves con problemas..
interest_keys <- b %>% 
  filter(interest_group == 4) %>% 
  pull(new_key)

# Vamos a hacer zoom en un rango de km para analizar a fondo las razones de que sea tan caro.

b %>%
  filter(
    grupo_comparable == grupo_analizado & 
      transportista == 'ESHMTD' & 
      avg_km >= 100 & 
      avg_km <= 110
  ) %>% 
  arrange(avg_km) %>% 
  view

# Del zoom anterior, tenemos dos llaves, una que es muy cara y otra con costo normal, vamos a ver los datos de cada una para explicar el costo.

interest_keys <- c('9428-4970-GTRATD-SENCILLO', '9428-4970-MEOCTD-SENCILLO', '9428-4970-SICETD-SENCILLO', '9428-4970-SPIMTD-SENCILLO', '9428-4970-TRCUTD-SENCILLO', '9428-6497-GTRATD-SENCILLO', '9428-6497-MEOCTD-SENCILLO', '9428-6497-SPIMTD-SENCILLO', '9428-6497-TRCUTD-SENCILLO', '9428-6397-GTRATD-SENCILLO', '9428-6397-MEOCTD-SENCILLO', '9428-6397-MTRATD-SENCILLO', '9428-6397-SICETD-SENCILLO', '9428-6397-SPIMTD-SENCILLO', '9428-6397-TRCUTD-SENCILLO')

pre_ded %>% 
  filter(new_key %in% interest_keys) %>% 
  arrange(desc(new_key)) %>% 
  view()

# En este resumen, se puede ver que hay mucha diferencia en el tiempo de descarga, lo que provoca un diferencia enorme en el costo del tractor y por lo tanto, en el costo por km 

pre_ded %>% 
  filter(new_key %in% interest_keys) %>% 
  brief() %>% 
  view()

# Comparativa de tiempos de descarga de llaves problema en comparativa con las demás de su grupo..

pre_ded %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  brief(.round = 1) %>% 
  mutate(interest_group = ifelse(new_key %in% interest_keys, 1, 0)) %>%
  arrange(avg_km) %>%
  filter(interest_group == 1) %>% 
  copy()
  view()

# Para obtener los mayores tiempos de descarga:
pre_ded %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  brief(.round = 2) %>% 
  count(download_time) %>% 
  arrange(desc(download_time))
  # pull(download_time) %>% 
  # hist()
  # unique() %>% 
  # sort(decreasing = TRUE)

# Para obtener el tiempo promedio de descarga quitando outliers
ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess() %>% 
  filter(!(round(download_time) %in% c(16, 24))) %>% 
  pull(download_time) %>% 
  mean

# Para simular el comportamiento de los puntos problemáticos cuando su tiempo de descarga es promedio

ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess(search_time = c(16, 24), repl_time = 2.61) %>% 
  brief() %>% 
  mutate(interest_group = ifelse(new_key %in% interest_keys, 1, 0)) %>%
  arrange(avg_km) %>%
  view()

ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess(search_time = c(8, 16, 24), repl_time = 2.61) %>%
  # reprocess() %>% 
  regroup() %>% 
  remodel() %>% 
  replot(colored_keys = interest_keys, model = TRUE)

#### -- -- -- -- -- --- -- -- - - - - - Esto es obsoleto
# Cambiar el tiempo de descarga sí cambia muchos puntos, pero no todos, vamos a investigar a fondo los que aún no cambian, primero obtenemos sus llaves. Sí cambian con el tiempo de descarga, solo que su tiempo no era exacto, tenía decimales.
specials <- ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess(search_time = c(16, 24), repl_time = 2.61) %>% 
  brief() %>% 
  filter(avg_km >= 70 & costo_por_km >= 130) %>% 
  pull(new_key)

# Luego las ploteamos para confirmar
ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess(search_time = c(16, 24), repl_time = 2.61) %>% 
  regroup() %>% 
  replot(colored_keys = specials)
  
# Luego analizamos a fondo
ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  # reprocess(search_time = c(8, 24), repl_time = 4.46) %>%
  reprocess() %>% 
  brief() %>% 
  mutate(interest_group = ifelse(new_key %in% specials, 3, 0)) %>%
  arrange(avg_km) %>% 
  view()
  
# Antes del cambio
ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess() %>% 
  filter(new_key %in% '9807-2850-GTRATD-TH_R_28') %>% 
  # brief() %>% 
  view()
  copy()

# Después del cambio
ded_base %>% 
  filter(grupo_comparable == grupo_analizado) %>% 
  reprocess(search_time = c(16, 24), repl_time = 2.61) %>% 
  brief() %>% 
  filter(new_key %in% specials) %>% 
  view()

#### -- -- -- -- -- --- -- -- - - - - - Lo de arriba es obsoleto
  
# Para revisar los costos de los refrigerados:
viajes_ref <- ded_pred %>% 
  filter(grupo_comparable == '9807-TR_R_53' & costo_por_km >= 7500) %>% 
  pull(new_key)

ded_pred %>% 
  filter(grupo_comparable == '9807-TR_R_53') %>% 
  replot(viajes_ref)

pre_ded %>% 
  filter(new_key %in% viajes_ref) %>% 
  view()

q <- ded_base %>% 
  # filter(grupo_comparable == grupo_analizado) %>% 
  # reprocess(search_time = c(8, 16, 24), repl_time = 2.61) %>%
  reprocess() %>%
  regroup() %>% 
  remodel()

q %>%   
  filter(grupo_comparable %in% interest_groups) %>%
  replot(colored_keys = c(), model = TRUE)

interest_groups <- ded_pred %>%
  arrange(rank_impacto_gasto) %>% 
  pull(grupo_comparable) %>% 
  unique() %>% 
  head(16) %>%
  sort()

# Hemos visto que se requieren dos correcciones: calcular el rest time en vez de pegarlo de las tiendas y repartirlo y corregir los tiempos de descarga de las tiendas para asegurar su veracidad.
# El tiempo de descanso tiene poco efecto en los resultados y no hay problema corrigiéndolo, pero el download_time tiene un efecto importante y si lo dejamos fuera de la ecuación, la mayoría de los grupos cambia mucho su información y hace mucho más dificil la interpretación.
# Vamos a investigar porqué cambia tanto la información dependiendo del download_time. Uno de los grupos en los que es más notorio el cambio es el 9808-SENCILLO

process1 <- function(x){
  x %>% 
    filter(grupo_comparable == '9808-SENCILLO') %>% 
    brief(.round = 1) %>% 
    arrange(avg_km)
}

# Sí considera el download time
a <- pre_ded %>% 
  process1()

# No considera el download time
b <- ded_base %>% 
  reprocess() %>% 
  process1()

c <- a %>% 
  select(
    new_key,
    download_time,
    delivery_time,
    avg_km,
    total_km,
    tractor,
    total_gasto,
    costo_por_km
  ) %>% 
  inner_join(
    y = b %>% 
      select(
        new_key,
        tractor,
        total_gasto,
        costo_por_km
      ),
    by = 'new_key',
    suffix = c('_si', '_no')
  )

c %>% 
  ggplot(aes(total_km)) + 
  geom_point(aes(y = total_gasto_si), color = 'red') + 
  geom_point(aes(y = total_gasto_no), color = 'blue')

# Conclusiones del análisis: La diferencia se debe a que en el costo, el tiempo de descarga es la única variable independiente. El costo total se determina usando el costo del remolque y costo del tractor (dependen del delivery time). Los costos de spt y casetas son despreciables en proporción. El delivery time depende del trip time y rest time (depende del trip time), ambos dependen casi únicamente de la distancia, porque la diferencia en velocidades es muy pequeña.
# Al no incluir el download_time, básicamente el costo sube linealmente respecto a la distancia, lo que implica una sola cuota variable única y que al obtener un costo unitario como el costo por km, se convierte en una constante (la pendiente de la relación).
# Pero al incluir el tiempo de descarga, la relación vuelve a ser una cuota fija (virtual) + una cuota variable.

# Vamos a analizar un caso que sí puede tener una diferencia clara en costos debido al proveedor, la gráfica denota una doble tenencia a pesar de todas las correcciones anteriores.

a <- ded_pred %>% 
  filter(grupo_comparable == '9428-SENCILLO') %>% 
  arrange(avg_km)

# De inicio, la diferencia del costo se debe a los remolques.
# Grupo que tienen gráficas bonitas con costos de remolques agruados:
# 9808-SENCILLO -- son NAs
# 7760-THORTON -- son NAs
# 7760-SENCILLO
# 9804-SENCILLO
# 9811-SENCILLO
a <- pre_ded %>% 
  filter(grupo_comparable == '9428-SENCILLO') %>% 
  group_by(whse_nbr, store_nbr, equipment_type, equipment_type_anterior, transportista, grupo_comparable, grupo_comparable_anterior, rem_key, new_key, region) %>% 
  summarise(
    n_entregas = n(),
    n_viajes = n_distinct(id_viaje),
    total_gasto = sum(total_cost),
    avg_km = mean(km_ida_viaje),
    total_km = sum(km_ida_viaje),
    .groups = 'drop'
  ) %>% 
  mutate(
    costo_por_viaje = total_gasto / n_viajes,
    costo_por_km = total_gasto / total_km
  )

a %>% 
  filter(!is.na(rem_key)) %>% 
  ggplot(aes(avg_km)) + 
  geom_point(aes(y = costo_por_km, color = equipment_type_anterior)) + 
  facet_wrap(vars(rem_key), scales = 'free', ncol = 1)

# Conclusión del análisis: Las diferencias entre las dos tendencias sí eran por los costos de los remolques. Alo que no estábamos considerando es que cuando formamos los nuevos grupo comparables para distribuir el costo de tarifas al 100%, dentro del mismo grupo comparable "9808-SENCILLO" puede haber agregados camiones de varias capacidades y tipos diferentes (R-28, S-40), y esos tienen costos de remolques diferntes, y en general, son camiones diferentes, por lo que no son comparables aunque el costo de las tarifas sí se distribuya bien. La solución fue renombrar el "grupo_comparable" por "grupo_tractor" y "rem_key" por "grupo_remolque". Y Formamos una nueva variable de grupo remolque en la que sí se incluyen las variables de tipo de remolque y capacidad, además de en la nueva llave. Aí que cuando agrupamos por grupo comparable, ahora sí quedan rutas de transportes similares con el mismo costo de tractor y de remolque, son comparables.

# Después de las correcciones previas, las gráficas todas se ven mucho mejor, ya no se perciben dobles tendencias. El único detalle que podría restar por analizar es que en algunos casos, el costo por km parece constante a lo largo de la distancia, dificultando la predicción de la línea de tendencia. Un buen ejemplo de esto es el:
# 9806-SENCILLO-S-40

# Vamos a echar un vistazo a la gráfica
ded_pred %>% 
  filter(grupo_comparable == '9806-SENCILLO-S-40') %>% 
  replot(model = TRUE)

costs_cols <- c('costo_spt', 'casetas', 'tractor', 'remolque', 'total_gasto')

# Vamos a analizar con detalle estos
pre_ded %>% 
  filter(grupo_comparable == '9806-SENCILLO-S-40') %>% 
  brief() %>% 
  # filter(costo_por_km >= 60 & costo_por_km <= 70) %>% 
  mutate(across(
    .cols = costs_cols,
    .fns = ~.x / total_km,
    .names = '{col}_por_km'
  )) %>% 
  select(-costs_cols, -costo_por_km) %>% 
  pivot_longer(ends_with('_por_km'), names_to = 'tipo_costo', values_to = 'valor_costo') %>% 
  ggplot(aes(x = avg_km)) + 
  geom_line(aes(y = valor_costo, color = tipo_costo)) + 
  geom_point(aes(y = valor_costo, color = tipo_costo))

# Esta gráfica muestra que en el costo de los remolques y de los tractores, el costo por km sí tiene un comportamiento como el deseado, aunque muy leve. Sin embargo, el costo de SPT es mucho más impredecible, y siendo este casi siempre el más caro de todos, la forma final también es impredecible.
# Quiero ver si de casualidad el transportista hace alguna diferencia..
# Pero no es así, un transportista el que casi siempre se usa en todas las rutas, existen algunos casos en los que otro proveedor toma la misma ruta por un menor costo pero son pocas rutas y son pocos viajes, supongo que usualmente no tienen disponibles esas rutas y por eso se usa la más cara. Decidí que ya no vale la pena profundizar más en esto, la mayoría ya están muy bien y voy a aprovechar la info para hacer las comparativas.


# Pruebas de unión entre pxv y ded ----------------------------------------

# Este código es para generar un join entre pxv y ded sin tener que reprocesar la info entera. La desventaja es que hay que promediar las predicciones y en algunos casos, son considerablemente diferentes.
a <- pxv_pred %>% 
  create_common_groups(ded_pred) %>% 
  select(-c(equipment_type_anterior, grupo_tractor_anterior)) %>% 
  # Este es un resumen del resumen previo (pxv_pred), por lo que hay que tener cuidado con la forma en la que se sumariza.
  group_by(region, whse_nbr, store_nbr, transportista, equipment_type, tipo_remolque, capacidad_transporte, new_key, grupo_tractor, grupo_remolque, grupo_comparable) %>% 
  # Para evitar clashes en los nombres
  rename_with(
    ~paste0(.x, '_anterior'),
    any_of(c('n_entregas', 'n_viajes', 'total_gasto', 'total_km', 'avg_km'))
  ) %>% 
  summarise(
    n_entregas = sum(n_entregas_anterior),
    n_viajes = sum(n_viajes_anterior),
    total_gasto = sum(total_gasto_anterior),
    total_km = sum(total_km_anterior),
    # Para hacer el nuevo promedio, no se puede re-promediar. Tiene que ser así.
    avg_km = sum(avg_km_anterior * n_entregas_anterior) / sum(n_entregas_anterior),
    .groups = 'drop'
  ) %>% 
  mutate(
    costo_por_viaje = total_gasto / n_viajes,
    costo_por_km = total_gasto / total_km,
    esquema = 'pxv'
  ) %>% 
  full_join(
    ded_pred %>% 
      mutate(esquema = 'ded'),
    by = 'new_key'
  )

a %>% 
  filter(new_key %in% pull(a[duplicated(a$new_key),], new_key)) %>% 
  count(new_key) %>% 
  # filter(n > 2)
  view()

# Revisión costo excesivo de los tractores de SMO ------------------

pre_ded
processed_data %>% 
  filter(
    grupo_tractor == '9807-TR_R_53'
  ) %>% 
  # nrow()
  count(esquema)
  # pull(group_tractor_cost) %>% 
  # unique()


# Comparativa de tarifas vs Victor ----------------------------------------

interest_keys <- c(
  '8031-9805-METRTP-TR_S_53',
  '9804-9696-GREXTP-TR_S_40F',
  '9804-3747-AEXOTP-AR_S_48',
  '9804-9811-BUZMTP-TR_S_40F',
  '9428-9432-MELHTP-TR_S_48',
  '9804-1425-AEXOTP-AR_S_48',
  '9428-9432-BUZMTP-TR_S_40F',
  '9804-1550-AEXOTP-AR_S_48',
  '9428-9811-BUZMTP-TR_S_40F',
  '8031-9811-BUZMTP-TR_S_40F',
  '9428-9696-BUZMTP-TR_S_40F',
  '9414-6236-AEGMTP-TR_S_53',
  '9429-4941-TRCVTP-TR_S_53',
  '9428-9811-CULVTP-TR_S_40F',
  '9428-6218-TYSETP-TR_S_48'
)

pretty_names_pxv <- c(
  'Cedis_Camión' = 'grupo_tractor',
  'Región' = 'region',
  'Cedis' = 'whse_nbr',
  'Tienda' = 'store_nbr',
  'Transportista' = 'transportista',
  # grupo_remolque
  'Camión' = 'equipment_type',
  'Llave' = 'key',
  'Tipo remolque' = 'tipo_remolque',
  'Capacidad de caja' = 'capacidad_transporte',
  'Mes' = 'month',
  'Camión más ocupado (moda)' = 'moda_transporte',
  'Entregas' = 'n_entregas',
  'Viajes' = 'n_viajes',
  'Remolques' = 'n_remolques',
  'Total gastado' = 'total_gasto',
  'Km prom entrega' = 'avg_km_entrega',
  'Km prom viaje' = 'avg_km_viaje',
  'Total km' = 'total_km_entrega',
  'Costo por viaje' = 'costo_por_viaje',
  'Costo por entrega' = 'costo_por_entrega',
  'Costo por km' = 'costo_por_km',
  'Costo predicho' = 'pred',
  'Diferencia vs predicción' = 'diff_vs_pred',
  'Diferencia vs predicción (porcentual)' = 'diffp_vs_pred',
  'Gasto predicho' = 'gasto_pred',
  'Impacto en gasto' = 'impacto_gasto',
  'Ranking de impacto' = 'rank_impacto_gasto',
  'Ruta de interés' = 'ruta_interes'
)

pretty_names_ded <- c(
  'Cedis-Grupo de Camión' = 'grupo_comparable',
  'Región' = 'region',
  'Cedis' = 'whse_nbr',
  'Tienda' = 'store_nbr',
  'Transportista' = 'transportista',
  # grupo_tractor
  # grupo_remolque
  'Camión' = 'equipment_type',
  'Llave' = 'new_key',
  'Tipo remolque' = 'tipo_remolque',
  'Capacidad de caja' = 'capacidad_transporte',
  'Mes' = 'month',
  'Camión más ocupado (moda)' = 'moda_transporte',
  'Entregas' = 'n_entregas',
  'Viajes' = 'n_viajes',
  'Remolques' = 'n_remolques',
  'Total gastado' = 'total_gasto',
  'Km prom' = 'avg_km',
  'Total km' = 'total_km',
  'Costo por viaje' = 'costo_por_viaje',
  'Costo por km' = 'costo_por_km',
  'Costo predicho' = 'pred',
  'Diferencia vs predicción' = 'diff_vs_pred',
  'Diferencia vs predicción (porcentual)' = 'diffp_vs_pred',
  'Gasto predicho' = 'gasto_pred',
  'Impacto en gasto' = 'impacto_gasto',
  'Ranking de impacto' = 'rank_impacto_gasto'
)

pxv_pred %>% 
  filter(key %in% interest_keys) %>% 
  select(key, whse_nbr, store_nbr, transportista, equipment_type, costo_por_viaje, costo_por_km, rank_impacto_gasto) %>% 
  arrange(key) %>% 
  copy()

pxv_pred %>% 
  mutate(ruta_interes = ifelse(key %in% interest_keys, 1, 0)) %>%
  arrange(desc(ruta_interes), rank_impacto_gasto) %>%
  select(pretty_names_pxv) %>% 
  write_excel_csv(sprintf('output/%s-pxv_pred-%s.csv', Sys.Date(), unique(pxv_pred$month)), na = '')

ded_pred %>% 
  arrange(rank_impacto_gasto) %>% 
  select(pretty_names_ded) %>% 
  write_excel_csv(sprintf('output/%s-ded_pred-%s.csv', Sys.Date(), unique(ded_pred$month)), na = '')


# Pedido especial de Felipe -----------------------------------------------

may_processed_data <- processed_data

feb <- feb_processed_data %>% 
  filter(esquema == 'SENCILLO') %>% 
  rename('total_cost' = 'costo_total_spt') %>% 
  special_summarise(whse_nbr, store_nbr, capacidad_transporte) %>%
  mutate(
    key = paste(whse_nbr, store_nbr, capacidad_transporte, sep = '-'),
    mes = 'febrero'
  )

feb_ded <- pre_ded %>% 
  special_summarise(whse_nbr, store_nbr, capacidad_transporte) %>% 
  mutate(
    key = paste(whse_nbr, store_nbr, capacidad_transporte, sep = '-'),
    mes = 'febrero'
  )

feb %>%
  bind_rows(mar, abr, may, jun) %>%
  filter(!is.na(costo_por_viaje)) %>% 
  select(key, costo_por_viaje, mes) %>% 
  write_csv('output/costo_por_viaje_stores.csv')

feb_ded %>% 
  filter(!is.na(costo_por_viaje)) %>% 
  select(key, costo_por_viaje, mes) %>% 
  write_csv('output/costo_por_viaje_stores.csv')

# Con la llave de whse_nbr, store_nbr, capacidad_transporte y sólo con info de febrero hay 189/281 (67%) de NAs. 27 de ellos es porque son de los cd mérida y chihuahua.
# Con la misma llave pero con info de febrero y junio, hay 152/281 (54%) de NAs, corrigiendo lo de los CD merida y chihuahua, aún quedan 150 NAs.
# Con la misma llave pero info de febrero - junio, quedan 141 NAs.
# Con todas las bases pero quitando el whse_nbr de la llave, quedan 87/281 (30%) NAs.
# Quedan 62 NAs
# 47 despues del bugfix
# 46

# Investigando un bug -----------------------------------------------------

a <- ded_base %>% 
  anti_join(tarifas, 'grupo_tractor') %>% 
  pull(grupo_tractor) %>% 
  unique()

# Son grupos que no están en la base de tarifas

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
  # rowwise() %>% 
  # mutate(
  #   costo_por_viaje_avg = mean(c(costo_por_viaje_ded, costo_por_viaje_pxv), na.rm = TRUE),
  #   costo_por_km_avg = mean(c(costo_por_km_ded, costo_por_km_pxv), na.rm = TRUE)
  # ) %>% 
  # ungroup() %>% 
  mutate(
    esquema_incluido = case_when(
      is.na(total_gasto_ded) ~ 'pxv',
      is.na(total_gasto_pxv) ~ 'ded',
      TRUE ~ 'ambos'
    ),
    across(.fns = ~ifelse(is.na(.x), 0, .x)),
    total_gasto = total_gasto_ded + total_gasto_pxv,
    total_km = total_km_ded + total_km_pxv,
    n_entregas  = n_entregas_ded + n_entregas_pxv,
    costo_por_viaje_calc = total_gasto / n_entregas,
    costo_por_km_calc = total_gasto / total_km
  ) %>% 
  # relocate(ends_with('_avg'), .after = last_col()) %>% 
  left_join(det_dict, by = c('whse_nbr' = 'transportes'))




# Revisión de costos del Toreo --------------------------------------------

a %>% 
  filter(store_nbr == 2344) %>% 
  select(costo_por_viaje_pxv, costo_por_viaje_ded)

pre_ded %>% 
  filter(
    # store_nbr == 2344
    # grupo_tractor == '9801-SENCILLO'
    grupo_remolque == '9801-S-53'
  ) %>%
  # select(
  #   store_nbr,
    # grupo_remolque,
    # equipment_type,
    # full_mult
  #   costo_total_spt, 
  #   trip_tractor_cost,
  #   trip_remolque_cost, 
  #   total_casetas, 
  #   group_tractor_cost, 
  # group_remolque_cost
  #   time_tractor_share,
  #   km_ida_viaje, 
  #   speed,
  #   trip_time,
  #   download_time,
  #   rest_time,
  #   delivery_time
  # ) %>%
  pull(group_remolque_cost) %>%
  # sum()
  # length()
  unique()
  distinct()
  view()

  
# Revisión de distribución por entrega ------------------------------------

pre_pxv %>% 
  filter(
    # whse_nbr == 9808 & store_nbr == 4813
    id_viaje == 1005844216
  ) %>% 
  view()
  pull(id_viaje) %>% 
  unique()
  
ded_base %>% 
  filter(id_viaje == 1005962049) %>% 
  view()
  
processed_data %>% 
  filter(
    # id_viaje == 1005844216
    # id_viaje == 1005962049
    id_viaje %in% b
  ) %>% 
  view()

view()

# Playground de desarrollo de los hubs ------------------------------------

b <- pre_processed_data %>% 
  anti_join(stores_data, by = 'store_nbr') %>% 
  rename(c('hub_nbr' = 'store_nbr')) %>% 
  filter(hub_nbr == 74) %>% 
  pull(id_viaje)
  # count(tipo_origen)
  # filter(tipo_origen == 'CDV') %>% 
  # pull(whse_nbr) %>% 
  # unique() %>% 
  # sort()

q <- pre_processed_data %>% 
  filter(tipo_origen == 'CDV') %>% 
  rename('hub_nbr' = 'whse_nbr') %>% 
  anti_join(dc_hub, by = 'hub_nbr') %>% 
  pull(id_viaje) %>% 
  n_distinct()
  # pull(hub_nbr) %>% 
  # unique()
  # count(hub_nbr) %>% 
  # arrange(desc(n))
  # view()

pre_processed_data %>% 
  filter(tipo_origen == 'CDV') %>% 
  rename('hub_nbr' = 'whse_nbr') %>% 
  pull(costo_total_spt_original) %>% 
  sum()

pre_hub_store <- pre_processed_data %>% 
  filter(tipo_origen == 'CDV') %>% 
  rename('hub_nbr' = 'whse_nbr')

w <- pre_processed_data %>% 
  anti_join(stores_data, by = 'store_nbr') %>% 
  rename(c('hub_nbr' = 'store_nbr')) %>% 
  # group_by(tipo_origen) %>% 
  # summarise(costo = sum(costo_total_spt))
  filter(tipo_origen == 'CDV') %>% 
  pull(id_viaje) %>%
  unique() %>%
  sort()
  # pull(costo_total_spt) %>% 
  # sum()

q <- dc_hub %>% 
  anti_join(pre_hub_store, 'hub_nbr') %>% 
  pull(group_hub_cost) %>%
  sum()
  # pull(hub_nbr) %>%
  # unique() %>%
  # sort()

(sum(pre_processed_data$costo_total_spt)) == (sum(processed_data$costo_total_spt) + q)

e <- hub_store %>% 
  filter(id_viaje %in% w) %>% 
  pull(id_viaje) %>% 
  duplicated() %>% 
  which()

e <- hub_store %>% 
  filter(is.na(km_total_tienda)) %>% view
  pull(id_viaje) %>% 
  unique() %>% 
  sort()

# Distribución de costos de remolque en pxv -------------------------------

a <- processed_data %>% 
  filter(esquema == 'DEDICADO') %>% 
  mutate(
    grupo_remolque = ifelse(
      is.na(tipo_remolque),
      NA_character_,
      paste(
        whse_nbr,
        tipo_remolque,
        capacidad_transporte,
        sep = '-'
      )
    )
  ) %>% 
  pull(grupo_remolque) %>%
  unique() %>% 
  sort()

b <- pre_ded_inc %>% 
  # ded_base %>% 
  # filter(!is.na(tipo_remolque)) %>% 
  pull(grupo_remolque) %>% 
  unique() %>% 
  sort()
  
all(a == b)

# Los grupos de remolque no se han modificado desde processed data, excepto por el filtro

# 1 - crear el mes en pre_processed_data
# 2 - crear el grupo remolque en processed data - controlar los que se crean en pxv, dejar los de ded iguales
# 3 - poner el procesamiento de los remolques después de processed data y extraer sus grupos de remolques
# 4 - terminar processed data pegando los remolques y calculando el trip_remolque_cost
# 5 - en pre_pxv, crear total cost con la suma del costo spt, el de los hubs y el de los remolques
# 6 - en pre_ded, agregar el costo de los hubs


# Dif precio -------------------------------------------------

pre_ded %>% 
  # filter(equipment_type %in% c('TR S 28', 'TH S 28', 'TH S 20') | str_detect(equipment_type, 'CA')) %>% 
  special_summarise(equipment_type_anterior) %>% 
  # mutate(costo_por_viaje2 = total_gasto / n_viajes) %>% 
  view()
  
a <- pre_pxv %>% 
  filter(equipment_type %in% c('CA R 6'), total_cost < 200) %>% 
  pull(id_viaje) %>% 
  sort()


# Costos pxv para análisis de costo por caja ------------------------------

# Tablas de capacidades
dc_store_base %>% 
  # filter(tipo_remolque == 'R') %>% 
  select(equipment_type, capacidad_tarimas) %>% 
  distinct() %>% 
  arrange(desc(capacidad_tarimas))
  copy()

trucks <- dc_store_base %>% 
  pull(equipment_type) %>% 
  unique() %>% 
  sort(decreasing = TRUE)
  

# Para obtener la moda del transporte -------------------------------------

a <- processed_data %>% 
  group_by(whse_nbr, store_nbr) %>% 
  summarise(
    moda_transporte = getmode(equipment_type),
    .groups = 'drop'
  )

b <- a %>% 
  mutate(
    key = paste(whse_nbr, store_nbr, sep = '-')
  ) %>% 
  pull(key)

c <- complete_base %>% 
  mutate(
    key = paste(whse_nbr, store_nbr, sep = '-')
  ) %>% 
  pull(key)

setdiff(b, c)
setdiff(c, b)

complete_base <- complete_base %>% 
  inner_join(a, by = c('whse_nbr', 'store_nbr'))


# Revisar costos de las camionetas ----------------------------------------

processed_data %>% 
  filter(
    equipment_type == 'CA S 8', 
    esquema == 'SENCILLO', 
    !is.na(region), 
    !str_detect(equipment_type, 'AR|RA'),
    !ruteo
  ) %>% 
  mutate(total_cost = costo_total_spt + trip_remolque_cost + trip_hub_cost) %>% 
  summarise(
    across(
      c('costo_total_spt', 'trip_remolque_cost', 'trip_hub_cost', 'total_cost'),
      sum
    ),
    n_entregas = n()
  ) %>% 
  mutate(costo_por_entrega = total_cost / n_entregas)


# Costo por caja total WM -------------------------------------------------

a <- read_tsv('data/costo_caja/20200903-total_state_cases.txt') %>% 
  set_names(tolower(names(.))) %>% 
  mutate(vendor_name = 'TOTAL_WALMART') %>% 
  select(vendor_name, everything())

query_state_volume <- readRDS(sprintf('data/%s-query_state_volume', Sys.Date())) %>% 
  filter(!str_detect(vendor_name, 'MODELO|CLARA')) %>% 
  select(vendor_name, state_name, state_code, n_tiendas, cases, pallets, n_semanas) %>% 
  bind_rows(a)

costo_caja_pretty_names <- c(
  'RFC' = 'vendor_rfc_clean',
  'Proveedor' = 'vendor_name',
  'Cajas semanales' = 'cases',
  'Tarimas semanales' = 'pallets',
  'Camiones T1 (CDMX-descon)' = 'n_camiones_decon',
  'Ocupación T1' = 'ocupacion_prom_decon',
  'Cajas por camión T1' = 'cajas_prom_decon',
  'Costo por caja T1' = 'costo_decon_por_caja',
  # 'Costo T1' = 'costo_decon',
  'Camiones T2 (descon-tiendas)' = 'n_camiones',
  'Ocupación T2' = 'ocupacion_prom',
  'Cajas por camión T2' = 'cajas_prom',
  'Costo por caja T2' = 'costo_camiones_por_caja',
  # 'Costo T2' = 'costo_camiones',
  'Costo total por caja' = 'costo_total_por_caja'
  # 'Costo total' = 'costo_total',
)

vendor_summary %>% 
  select(costo_caja_pretty_names) %>% 
  copy()

# Resumen costo caja vendor -----------------------------------------------

case_cost_vendor_pretty_names <- c(
  'Proveedor' = 'vendor_name_clean',
  # 'Estado' = 'state_name',
  'Cajas semanales' = 'cases',
  'Tarimas' = 'pallets',
  'Costo por caja T1 (CDMX-descon)' = 'costo_decon_por_caja',
  'Ocupación T1' = 'ocupacion_prom_decon',
  'Cajas por camión T1' = 'cajas_prom_decon',
  'Costo por caja T2 (descon-tiendas)' = 'costo_camiones_por_caja',
  'Ocupación T2' = 'ocupacion_prom',
  'Cajas por camión T2' = 'cajas_prom',
  'Costo total por caja' = 'costo_total_por_caja'
)

state_summary %>% 
  filter(str_detect(vendor_name_clean, 'REGIOM|MASTER')) %>% 
  select(append(case_cost_vendor_pretty_names, c('Estado' = 'state_name'), 1)) %>% 
  copy()

vendor_summary %>% 
  filter(str_detect(vendor_name_clean, 'REGIOM|MASTER')) %>% 
  select(case_cost_vendor_pretty_names) %>% 
  copy()

# Tiempos de descaga de MTS -----------------------------------------------

a <- read_excel('data/Facturados_Ene-Oct_2020.xlsx')

# a$`Fecha CHECK OUT`[length(a$`Fecha CHECK OUT`)] %>% 
a$`Fecha CHECK OUT`[2] %>% 
  as.numeric() %>% 
  openxlsx::convertToDateTime()

names(a)[str_detect(names(a), '.*Fecha.*')]

b <- a %>% 
  mutate(
    across(starts_with('Fecha'), ~openxlsx::convertToDateTime(as.numeric(.x)))
  )

to_date <- function(x){
  tryCatch(
    as.POSIXct(x), 
    error = function(e) {
      as.POSIXct(
        suppressWarnings(as.numeric(x)) * 86400, 
        origin = "1899-12-30",
        tz = 'GMT'
      )
    }
  )
}

b <- a %>% 
  mutate(
    across(starts_with('Fecha'), ~to_date(.x))
  )

b %>% 
  select(starts_with('Fecha')) %>% 
  head(10) %>% 
  view

saveRDS(b, 'output/20201208-mts_download')


# N viajes al año ---------------------------------------------------------

complete_data %>% 
  filter(whse_nbr  %in% c(6033,
                          6034,
                          8033,
                          9429,
                          9696,
                          9809)) %>% 
  group_by(whse_nbr, month) %>% 
  summarise(
    gasto = sum(total_cost_teorico),
    casetas = sum(total_casetas),
    viajes = sum(n_viajes),
    n_entregas = n()
  ) %>% 
  copy()

complete_data %>% 
  filter(whse_nbr %in% c(6033, 6034)) %>% 
  group_by(whse_nbr, month, month_nbr) %>%  
  summarise(
    across(c(total_cost_teorico, n_viajes, n_remolques), sum),
    entregas = n()
  ) %>% 
  arrange(whse_nbr, month_nbr)


complete_data %>% 
  # filter(whse_nbr == 9804, trip_tractor_cost > 0) %>%
  filter(whse_nbr == 6034, month_nbr %in% c(7, 10)) %>%
  group_by(month, month_nbr) %>% 
  summarise(
    tractor = sum(trip_tractor_cost),
    spt = sum(costo_total_spt),
    avg_cost = mean(total_cost_teorico),
    # avg_tractor = mean(trip_tractor_cost),
    # avg_placa_cost = mean(placa_tractor_cost, na.rm = TRUE),
    stores = n_distinct(store_nbr),
    entregas = n(),
    viajes = sum(n_viajes),
    placas = n_distinct(placa_tractor)
  ) %>% view
  group_by(month, month_nbr) %>%
  mutate(
    perc = tractor / sum(tractor)
  ) %>%
  pivot_wider(names_from = esquema, values_from = c(tractor, avg_cost, spt, perc, stores, entregas, viajes, placas)) %>%
  # pivot_wider(names_from = esquema, values_from = c(tractor, stores, entregas, placas)) %>% 
  arrange(month_nbr) %>% 
  view

complete_data %>% 
  # filter(whse_nbr == 9804, trip_tractor_cost > 0) %>%
  filter(whse_nbr == 6034, month_nbr %in% c(7, 10)) %>%
  group_by(month, month_nbr, esquema) %>% 
  summarise(
    # tractor = sum(trip_tractor_cost),
    # spt = sum(costo_total_spt),
    # avg_cost = mean(total_cost_teorico),
    across(c(costo_total_spt, trip_tractor_cost, total_casetas, costo_final_remolque, trip_hub_cost, avg_year_cost_dolly, gasto_variable_teorico_entrega, total_cost_teorico), mean)
    # avg_tractor = mean(trip_tractor_cost),
    # avg_placa_cost = mean(placa_tractor_cost, na.rm = TRUE),
    # stores = n_distinct(store_nbr),
    # entregas = n(),
    # viajes = sum(n_viajes),
    # placas = n_distinct(placa_tractor)
  ) %>% 
  # group_by(month, month_nbr) %>%
  # mutate(
  #   perc = entregas / sum(entregas)
  # ) %>%
  # pivot_wider(names_from = esquema, values_from = c(avg_cost, spt, perc, stores, entregas, viajes, placas)) %>%
  pivot_wider(names_from = esquema, values_from = c(costo_total_spt, trip_tractor_cost, total_casetas, costo_final_remolque, trip_hub_cost, avg_year_cost_dolly, gasto_variable_teorico_entrega, total_cost_teorico)) %>%
  # pivot_wider(names_from = esquema, values_from = c(tractor, stores, entregas, placas)) %>% 
  arrange(month_nbr) %>% 
  view

complete_data %>% 
  filter(whse_nbr == 6034, month == 'oct') %>%
  # filter(whse_nbr == 9804, month == 'may', esquema == 'DEDICADO') %>% 
  count(equipment_type) %>% 
  arrange(desc(n))

complete_data %>% 
  filter(whse_nbr == 6034, month %in% c('jul', 'oct'), esquema == 'SENCILLO') %>% 
  # ggplot(aes(x = month, y = costo_total_spt)) + 
  boxplot(costo_total_spt ~ month, data = .)


complete_data <- complete_data %>% 
  mutate(
    gasto_variable_teorico_entrega = replace_na(gasto_variable_teorico_entrega, 0),
    total_cost_teorico = costo_total_spt + trip_tractor_cost + total_casetas + costo_final_remolque + trip_hub_cost + avg_year_cost_dolly + gasto_variable_teorico_entrega
  )
# Resumen Karina Garcia ---------------------------------------------------

complete_data %>% 
  # a es un vector con determinantes que no tienen gastos variables (hubs, etc. no son cedis)
  # filter(!(whse_nbr %in% a)) %>% 
  group_by(whse_nbr, store_nbr, month, month_nbr) %>% 
  summarise(gasto_transporte = sum(total_cost_teorico)) %>% 
  arrange(whse_nbr, store_nbr, month_nbr) %>% 
  write_csv('output/2020-12-22-gastos_cedis_tienda_mes.csv')


# Analisis costos de hubs nulos -------------------------------------------

# whse_nbr2 %in% c(4971, 7487)
# whse_nbr %in% c(9414, 9696)

complete_data %>% 
  filter(
    whse_nbr %in% c(8035, 9806),
    # whse_nbr %in% c(8037, 6033, 9809)
    # whse_nbr %in% c(6033, 6034, 8037, 9809)
    # whse_nbr %in% c(9414, 9696)
  ) %>% 
  select(whse_nbr, whse_name) %>% 
  distinct()

# El problema
complete_data %>% 
  filter(whse_nbr %in% c(9414, 9696)) %>% 
  group_by(whse_nbr, month_nbr) %>% 
  summarise(
    hubs = sum(trip_hub_cost),
    stores = n_distinct(store_nbr)
  ) %>% 
  arrange(whse_nbr)

# Son 8 hubs que se comparten entre los dos CD, ambos mandan a todos
# Los hubs 215 y 216 son los que más tiendas tienen (17, 13) en sams y (17, 14) en auto
complete_data %>% 
  filter(whse_nbr %in% c(9414, 9696), trip_hub_cost > 0) %>% 
  group_by(whse_nbr, hub_nbr) %>% 
  summarise(
    hubs = sum(trip_hub_cost),
    stores = n_distinct(store_nbr)
  ) %>% 
  arrange(whse_nbr, desc(stores))

# Para ver el cambio de no. de hubs en el tiempo
complete_data %>% 
  filter(whse_nbr %in% c(9414, 9696)) %>% 
  group_by(whse_nbr, month_nbr) %>% 
  summarise(
    hubs = n_distinct(hub_nbr, na.rm = TRUE)
  ) %>% 
  pivot_wider(
    names_from = month_nbr,
    values_from = hubs
  )

# Tiendas más representativas de cada cedis
a <- complete_data %>% 
  filter(whse_nbr %in% c(9414, 9696), trip_hub_cost > 0) %>% 
  group_by(whse_nbr, hub_nbr, store_nbr) %>% 
  summarise(
    hubs = sum(trip_hub_cost)
  ) %>% 
  arrange(whse_nbr, desc(hubs)) %>% 
  split(.$whse_nbr) %>% 
  map(head, 5) %>% 
  bind_rows()

# Revisar qué pasó con las tiendas de esos hubs
# En esta muestra, todas las tiendas tienen ambos CD porque todas usan hubs que son usados por ambos cedis
complete_data %>% 
  filter(store_nbr %in% a$store_nbr, whse_nbr %in% c(9414, 9696)) %>% 
  # group_by(whse_nbr, is.na(hub_nbr), month_nbr) %>% 
  filter(whse_nbr == 9414) %>% 
  group_by(whse_nbr, is.na(hub_nbr), month_nbr) %>%
  summarise(viajes = n()) %>%
  pivot_wider(
    names_from = month_nbr,
    values_from = viajes
  )

######## Revisar viajes de cedis sams a tiendas auto y viceversa
b <- complete_data %>% 
  # Se surte un sams desde un cedis de autoservicio?
  filter(store_nbr == 6288, whse_nbr == 9696, month_nbr == 1) %>% 
  pull(id_viaje) %>% 
  unique()

# Revisar la funte original de esos viajes
# Es un solo hub que surte a esa tienda
pre_processed_data %>% 
  filter(id_viaje %in% b) %>% 
  select(whse_nbr, whse_name, store_nbr) %>% 
  distinct()

# Pero tiene dos cedis usandolo
pre_processed_data %>% 
  filter(store_nbr == 121) %>% 
  select(whse_nbr, whse_name, store_nbr) %>% 
  distinct()

# El problema es que los costos del cedis al hub no se pueden repartir solo en las tiendas que les corresponde porque no se sabe cuales son.
# Los costos no se repiten, son los correctos, el único rastro equivocado es el cedis que puede no ser congruente.

########## Revisar alineaciones de las tiendas problemáticas
complete_data %>% 
  # Excluir los que no son cedis y los que son de culiacan FYV (8037) y MTY FYV 9807, que además no cambian
  filter(store_nbr %in% a$store_nbr, !(whse_nbr %in% c(8037, 9809)), digits(hub_nbr) == 6) %>% 
  group_by(hub_nbr, month_nbr) %>% 
  summarise(viajes = n()) %>% 
  ggplot(aes(x = as.factor(month_nbr), y = viajes, fill = as.factor(hub_nbr))) + 
  geom_bar(stat = 'identity') +
  scale_fill_colorblind()

complete_data %>% 
  # Excluir los que no son cedis y los que son de culiacan FYV (8037) y MTY FYV 9807, que además no cambian
  filter(
    store_nbr %in% a$store_nbr, 
    !(whse_nbr %in% c(8037, 9809)),
    digits(whse_nbr) == 4
  ) %>% 
  group_by(whse_nbr, month_nbr) %>% 
  summarise(viajes = n()) %>% 
  ggplot(aes(x = as.factor(month_nbr), y = viajes, fill = as.factor(whse_nbr))) + 
  geom_bar(stat = 'identity')  
# scale_fill_colorblind()

# Quiero ver el cambio de hubs para estas tiendas
complete_data %>% 
  filter(store_nbr %in% a$store_nbr) %>% 
  group_by(hub_nbr, month_nbr) %>%
  # group_by(digits(hub_nbr), month_nbr) %>% 
  # group_by(month_nbr) %>% 
  summarise(viajes = n()) %>% 
  pivot_wider(
    names_from = month_nbr,
    values_from = viajes
  )

# Extraer los hubs nuevos
c <- complete_data %>% 
  filter(store_nbr %in% a$store_nbr, digits(hub_nbr) == 6) %>% 
  pull(hub_nbr) %>% 
  unique()

complete_data %>% 
  filter(hub_nbr %in% c, store_nbr %in% a$store_nbr) %>% 
  count(whse_nbr, hub_nbr) %>% 
  mutate(perc = n / sum(n)) %>% 
  arrange(desc(n))

# Estos son los hubs nuevos:
# 101400 101300 101401 101601 100600 106000 749800 106100
# De esos, faltan estos 3 hubs en el diccionario:
# 106000 749800 106100

# Pero aún de los que sí están en el dic., no hay viajes desde los cedis, no hay forma segura actualmente de asignarlos a un cedis, por eso se quedan fuera.
dc_hub %>% 
  filter(hub_nbr %in% c)

tractor_processed_data %>% 
  filter(store_nbr %in% c)


# Comparativa costos por caja ---------------------------------------------
rod_base <- read_csv('analyses/comparativa_costos/rodrigo/20210105_cost_per_case.csv')

rod <- rod_base %>% 
  set_names(tolower(names(.))) %>% 
  pivot_longer(cols = ene:dic, names_to = 'month', values_to = 'value') %>% 
  pivot_wider(names_from = unidad, values_from = value) %>% 
  mutate(costo = costo_caja * volumen)



# Resumen por cedis/mes
whse_month_summary <- complete_data %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete2, month) %>%
  summarise(
    across(c(costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, costo_final_dolly, gasto_variable_teorico_entrega, costo_final_quintas , total_reclamos , total_regresos), sum),
    .groups = 'drop'
  ) %>%
  # Hay que validar los que no tienen 4 digitos
  filter(digits(whse_nbr) == 4)

complete_data %>% 
  filter(
    whse_nbr %in% c(9807),
    # whse_nbr %in% c(8037, 6033, 9809)
    # whse_nbr %in% c(6033, 6034, 8037, 9809)
    # whse_nbr %in% c(9414, 9696)
  ) %>% 
  select(whse_nbr, whse_name) %>% 
  distinct()

complete_data %>% 
  filter(store_nbr == 1762) %>% 
  count(esquema)

complete_data %>% 
  filter(store_nbr == 1762) %>% 
  group_by(whse_nbr, whse_nbr2, whse_name) %>% 
  summarise(sum(total_cost_teorico))

complete_data %>% 
  filter(
    # store_nbr %in% c(1023, 1029, 1031, 1101, 1239, 1526, 1693, 1762, 1951, 2142, 2332, 2468, 3049, 3084, 3293, 3558, 3693, 3711, 3894, 4060, 4185, 4575, 4828, 5736)
    # Tiendas Zona 1
    store_nbr %in% c(1239, 1762, 1951, 2332, 3049, 3084, 3693, 3711, 4060, 4828),
    whse_nbr == 9807 # 7761
  ) %>% 
  group_by(store_nbr) %>% 
  summarise(
    across(c(costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, costo_final_dolly, gasto_variable_teorico_entrega, costo_final_quintas , total_reclamos , total_regresos, total_cost_teorico), sum),
    .groups = 'drop'
  ) %>% 
  copy()


# Para comparativa con Karina ---------------------------------------------

zones_base <- read_csv('data/20210113-zone_dic.csv')

zones <- zones_base %>% 
  set_names(tolower(names(.))) %>% 
  mutate(origen = tolower(str_remove_all(str_extract(cedis_origen, '(?<=_).*(?=-)'), '\\s'))) %>% 
  # filter(origen == 'villahermosa') %>% 
  select(cd, zona, tienda)

# month_vol <- volumenes_base %>% 
#   filter(between(fecha, min(complete_data$fecha_embarque), max(complete_data$fecha_embarque))) %>% 
#   mutate(month_nbr = month(fecha)) %>% 
#   group_by(dc_nbr, month_nbr) %>% 
#   summarise(volumen_ocupacion = sum(volumen_ocupacion), .groups = 'drop')

# Del proyecto R de bases maestras
month_vol_base <- read_csv('data/20201016-resumen-cedis-tienda-mes-red.csv')

month_vol <- month_vol_base %>% 
  select(det_origen, det_destino, month, volumen_vnpk)

comp_summary <- complete_data %>% 
  filter(whse_nbr %in% c(7761, 9807)) %>% 
  inner_join(zones, by = c('store_nbr' = 'tienda', 'whse_nbr' = 'cd')) %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete2, store_nbr, month, month_nbr, zona) %>% 
  summarise(
    n_entregas = n(),
    across(c(n_viajes, costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, costo_final_dolly, gasto_variable_teorico_entrega, costo_final_quintas , total_reclamos , total_regresos, total_cost_teorico), sum),
    .groups = 'drop'
  ) %>% 
  left_join(
    month_vol, 
    by = c(
      'whse_nbr2' = 'det_origen', 
      'store_nbr' = 'det_destino', 
      'month_nbr' = 'month'
    )
  ) %>% 
  mutate(
    subtotal_comparable = total_cost_teorico - (costo_final_dolly + total_reclamos),
    costo_por_caja = replace_na(total_cost_teorico / volumen_vnpk, 0)
  )

a <- c(
  'entregas' = 'n_entregas',
  'viajes' = 'n_viajes',
  'spt' = 'costo_total_spt',
  'tractores' = 'trip_tractor_cost',
  'casetas' = 'costo_casetas',
  'remolques' = 'costo_final_remolque',
  'hubs' = 'trip_hub_cost',
  # 'dollies' = 'costo_final_dolly',
  'variables' = 'gasto_variable_teorico_entrega',
  'quintas' = 'costo_final_quintas',
  # 'reclamos' = 'total_reclamos',
  'regresos' = 'total_regresos'
  # 'total' = 'total_cost_teorico'
)

comp_summary %>% 
  rename(a) %>% 
  write_csv('output/2021-01-14-comp_summary3.csv')

# Por tienda
store_comp_summary <- comp_summary %>% 
  filter(whse_nbr == 7761) %>% 
  group_by(zona, store_nbr) %>% 
  summarise(
    across(c(n_entregas, n_viajes, costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, gasto_variable_teorico_entrega, costo_final_quintas , total_regresos, volumen_vnpk, subtotal_comparable), sum),
    .groups = 'drop'
  )
  
store_comp_summary %>% 
  rename(a) %>% 
  write_csv('output/2021-01-14-store_comp_summary3.csv')



# Distribucion por volumen ------------------------------------------------
new_complete_data <- complete_data %>% 
  # Está duplicando algunos registros
  left_join(
    month_vol, 
    by = c(
      'whse_nbr2' = 'det_origen', 
      'store_nbr' = 'det_destino', 
      'month_nbr' = 'month'
    )
  ) %>% 
  group_by(id_viaje) %>% 
  mutate(
    km_entrega_share_new = case_when(
      is.na(sum(volumen_vnpk)) ~ 1 / n(),
      TRUE ~ volumen_vnpk / sum(volumen_vnpk)
    ),
    across(
      .cols = ends_with('_original'),
      .fns = ~.x * km_entrega_share_new,
      .names = '{col}_dividido'
    )
  ) %>% 
  ungroup() %>% 
  rename_with(~str_replace(.x, '_original_dividido', '_new'), ends_with('original_dividido')) %>% 
  mutate(
    gasto_variable_teorico_entrega_new = replace_na(gasto_variable_teorico * km_total_viaje_new, 0),
    total_cost_teorico_new = costo_total_spt_new + trip_tractor_cost + costo_casetas_new + costo_final_remolque + trip_hub_cost + costo_final_dolly + gasto_variable_teorico_entrega_new + costo_final_quintas + total_reclamos + total_regresos
  )

complete_data %>% 
  filter(whse_nbr %in% c(7761, 9807)) %>% 
  summarise(across(c(km_total_viaje, costo_total_spt, costo_casetas, gasto_variable_teorico_entrega, total_cost_teorico), sum)) %>% 
  copy()

# Para estos cedis, con la nueva distribución hay una diferencia de entre 1 y 2% (5M), hay que revisar bien que no haya errores
new_complete_data %>% 
  filter(whse_nbr %in% c(7761, 9807)) %>%
  summarise(across(c(km_total_viaje, costo_total_spt, costo_casetas, gasto_variable_teorico_entrega, total_cost_teorico, km_total_viaje_new, costo_total_spt_new, costo_casetas_new, gasto_variable_teorico_entrega_new, total_cost_teorico_new), sum)) %>% 
  copy()

new_complete_data %>% 
  filter(whse_nbr %in% c(7761, 9807)) %>% 
  pull(id_viaje) %>% 
  head(10)

new_complete_data %>% 
  filter(id_viaje == 1005843030) %>% 
  copy()

month_vol %>% 
  filter(
    det_origen == 7466, 
    det_destino %in% c(3839, 3761, 3837, 3824, 3840),
    month == 1
  ) %>% 
  pull(volumen_vnpk) %>% 
  sum

complete_data %>% 
  filter(month == 'oct', hub_nbr == 100800) %>% 
  pull(whse_nbr)

# Nueva comparativa -------------------------------------------------------

new_comp_summary <- complete_data %>% 
  filter(whse_nbr %in% c(7761, 9807)) %>% 
  inner_join(zones, by = c('store_nbr' = 'tienda', 'whse_nbr' = 'cd')) %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete2, store_nbr, month, month_nbr, zona) %>% 
  summarise(
    n_entregas = n(),
    across(c(n_viajes, costo_prov, costo_diesel, costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, costo_final_dolly, gasto_variable_teorico_entrega, costo_final_quintas , total_reclamos , total_regresos, total_cost_teorico, volumen_vnpk), sum),
    .groups = 'drop'
  ) %>% 
  # left_join(
  #   month_vol, 
  #   by = c(
  #     'whse_nbr2' = 'det_origen', 
  #     'store_nbr' = 'det_destino', 
  #     'month_nbr' = 'month'
  #   )
  # ) %>% 
  mutate(
    zone = str_extract(zona, '\\d+'),
    subtotal_comparable = total_cost_teorico - (costo_final_dolly + total_reclamos),
    costo_por_caja = replace_na(total_cost_teorico / volumen_vnpk, 0)
  )

a <- c(
  'entregas' = 'n_entregas',
  'viajes' = 'n_viajes',
  'prov' = 'costo_prov',
  'diesel' = 'costo_diesel',
  'spt' = 'costo_total_spt',
  'tractores' = 'trip_tractor_cost',
  'casetas' = 'costo_casetas',
  'remolques' = 'costo_final_remolque',
  'hubs' = 'trip_hub_cost',
  # 'dollies' = 'costo_final_dolly',
  'variables' = 'gasto_variable_teorico_entrega',
  'quintas' = 'costo_final_quintas',
  # 'reclamos' = 'total_reclamos',
  'regresos' = 'total_regresos'
  # 'total' = 'total_cost_teorico'
)

# Cedis-tienda_new
new_comp_summary %>% 
  rename(a) %>% 
  write_csv('output/2021-01-27-comp_summary_new.csv')

# Por tienda_new
new_store_comp_summary <- new_comp_summary %>% 
  filter(whse_nbr == 7761) %>% 
  group_by(zona, store_nbr) %>% 
  summarise(
    across(c(n_entregas, n_viajes, costo_prov, costo_diesel, costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, gasto_variable_teorico_entrega, costo_final_quintas , total_regresos, volumen_vnpk, subtotal_comparable), sum),
    .groups = 'drop'
  )

new_store_comp_summary %>% 
  rename(a) %>% 
  write_csv('output/2021-01-27-store_comp_summary_new.csv')


# Revisión extra Karina ---------------------------------------------------

new_complete_data %>% 
  filter(store_nbr %in% c(1237, 1424), whse_nbr == 7761) %>% 
  group_by(hub_nbr) %>% 
  summarise(sum(trip_hub_cost))
  copy()
  
month_vol %>% 
  filter(det_destino == 1783, det_origen == 7466)

# Workshop

new_comp_summary %>% 
  filter(zone %in% c(5, 6)) %>% 
  group_by(zone) %>% 
  summarise(
    transporte = getmode(equipment_type),
    prom = mean(trip_tractor_cost),
    across(c(trip_tractor_cost, volumen_vnpk), sum)
  )

complete_data %>% 
  left_join(
    select(location_data, determinante, id_formato),
    by = c('store_nbr' = 'determinante')
  ) %>% 
  group_by(id_formato, month, month_nbr) %>% 
  summarise(
    across(c(n_viajes, costo_prov, costo_diesel, costo_total_spt, trip_tractor_cost, costo_casetas, costo_final_remolque, trip_hub_cost, costo_final_dolly, gasto_variable_teorico_entrega, costo_final_quintas , total_reclamos , total_regresos, total_cost_teorico, volumen_vnpk), sum),
    download_time = mean(download_time),
    equipment_type = getmode(equipment_type)
  ) %>% 
  mutate(
    costo_por_caja = replace_na(total_cost_teorico / volumen_vnpk, 0)
  ) %>% 
  copy()

complete_data %>% 
  left_join(
    select(location_data, determinante, id_formato),
    by = c('store_nbr' = 'determinante')
  ) %>% 
  filter(month_nbr == 10, id_formato == 'MB') %>% 
  # group_by(id_formato) %>% 
  count(equipment_type) %>% 
  arrange(desc(n))

# Output para Charlie
a <- complete_data %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, store_nbr) %>% 
  summarise(
    camion_frecuente = getmode(equipment_type),
    n_entregas = n(),
    across(c(n_viajes, km_total_viaje, total_cost_teorico, volumen_vnpk), sum)
  ) %>% 
  mutate(
    costo_por_entrega = total_cost_teorico / n_entregas,
    costo_por_viaje = total_cost_teorico / n_viajes,
    costo_por_km = total_cost_teorico / km_total_viaje,
    costo_por_caja = ifelse(volumen_vnpk == 0, 0, total_cost_teorico / volumen_vnpk)
  ) %>% 
  filter(digits(whse_nbr) == 4)

write_csv(a, 'output/20210309-costos_por_ruta.csv')
  
# Revisión para Karina
# 9M originales
complete_data %>% 
  filter(whse_nbr %in% c(
    9	,
    96	,
    98	,
    99	,
    201	,
    6012	,
    7761	,
    8037	,
    8063	,
    9675	,
    9807	,
    9808	,
    9809	,
    100800	,
    101000	,
    102000	,
    170000	,
    749800), 
    tipo_store == 'sams'
  ) %>% 
  pull(volumen_vnpk) %>% 
  sum()
  
# Distribución de cajas por cedis
complete_data %>% 
  filter(whse_nbr %in% c(
    9	,
    96	,
    98	,
    99	,
    201	,
    6012	,
    7761	,
    8037	,
    8063	,
    9675	,
    9807	,
    9808	,
    9809	,
    100800	,
    101000	,
    102000	,
    170000	,
    749800), 
    tipo_store == 'sams'
  ) %>% 
  group_by(whse_nbr, whse_nbr2, whse_name_complete2) %>% 
  summarise(cajas = sum(volumen_vnpk)) %>% 
  ungroup() %>% 
  mutate(perc = cajas/sum(cajas))

# Realmente, viene todo de estos cedis de transportes: 6012 y 8037, o estos cedis operativos: 4996, 7458

# En la base original de esos 2 cd hay 17.2M
volumen %>% 
  filter(whse_nbr2 %in% c(4996, 7458)) %>% 
  pull(route_vol) %>% 
  sum()

# Vamos a revisar primero por cedis
volumen %>% 
  filter(whse_nbr2 %in% c(4996, 7458)) %>% 
  group_by(whse_nbr2) %>% 
  summarise(cajas = sum(route_vol)) %>% 
  ungroup() %>% 
  mutate(perc = cajas/sum(cajas))

# De Tepeji (4996), hay 9.067 / 9.276M, 97% del total. Voy a enfocarme en el de villahermosa, que tenemos 36K / 7.928M (0.4% del total)

volumen %>% 
  filter(whse_nbr2 %in% c(7458, 4996)) %>% 
  left_join(select(location_data, determinante, tipo_store), by = c('store_nbr' = 'determinante')) %>% 
  group_by(tipo_store, whse_nbr2) %>% 
  summarise(cajas = sum(route_vol)) %>% 
  mutate(perc = cajas/sum(cajas))

# Split por mes
complete_data %>% 
  filter(whse_nbr == 8037, tipo_store == 'sams') %>% 
  group_by(month_nbr) %>% 
  summarise(
    cajas = sum(volumen_vnpk), 
    entregas = n(), 
    tiendas = n_distinct(store_nbr)
  ) %>% 
  ungroup() %>% 
  mutate(perc = cajas/sum(cajas), perc_ent = entregas / sum(entregas))

# Las entregas tienen numeros coherentes

volumen %>% 
  left_join(select(location_data, determinante, tipo_store), by = c('store_nbr' = 'determinante')) %>% 
  filter(whse_nbr2 == 7458, tipo_store == 'sams') %>% 
  group_by(month_nbr) %>% 
  summarise(cajas = sum(route_vol), tiendas = n_distinct(store_nbr)) %>% 
  ungroup() %>% 
  mutate(perc = cajas/sum(cajas))

# El volumen tiene números coherentes en la base
# Hay muchas más tiendas en la base que en los viajes

# Ningún mes está bueno
# Voy a revisar el diccionario
dc_dic %>% 
  filter(whse_nbr2 == 7458)

# Voy a revisar si con tiendas comparables, se parecen los números, y que sean las mismas tiendas
a <- complete_data %>% 
  filter(whse_nbr == 8037, tipo_store == 'sams', month_nbr == 2) %>% 
  pull(store_nbr) %>% 
  unique()


# Importante --- la mayoría de estos pueden no ser sams
b <- volumen %>% 
  left_join(select(location_data, determinante, tipo_store), by = c('store_nbr' = 'determinante')) %>% 
  filter(whse_nbr2 == 7458, month_nbr == 2, tipo_store %in% c('sams', NA)) %>% 
  pull(store_nbr) %>% 
  unique()

length(a)
length(b)
setdiff(a, b)
setdiff(b, a)
sum(a %in% b)
sum(b %in% a)
# Solo tienen una tienda coincidente (4981), voy a revisar el volumen
volumen %>% 
  filter(whse_nbr2 == 7458, store_nbr == 4981, month_nbr == 2) %>% 
  pull(route_vol) %>% 
  sum()

complete_data %>% 
  filter(whse_nbr == 8037, store_nbr == 4981, month_nbr == 2) %>% 
  pull(volumen_vnpk) %>% 
  sum()

complete_data %>% 
  filter()

# El vol coincidente si es igual, me intriga que la mayoria de a no esté en b
volumen %>% 
  filter(whse_nbr2 == 7458, store_nbr %in% a, month_nbr == 2)

pre_processed_data %>% 
  filter(whse_nbr == 8037)


# Tractos osciosos para Grace ---------------------------------------------

a <- processed_data %>% 
  # complete_data %>% 
###
  left_join(
    select(dc_dic, -ends_with('_texto')),
    by = c('whse_nbr', 'whse_nbr2')
  ) %>%
  mutate(
    whse_name = coalesce(whse_name.y, whse_name.x),
    whse_name_complete = coalesce(whse_name_complete, whse_name.x)
  ) %>%
  select(-c(whse_name.x, whse_name.y))

a %>% 
####
  # filter(!is.na(red), esquema != 'REDONDO') %>% 
  group_by(month, month_nbr, whse_nbr, whse_name, equipment_type, red, esquema) %>%
  # group_by(whse_nbr, whse_name, equipment_type, red, esquema) %>%
  # summarise(placas = n_distinct(placa_tractor)) %>% 
  # group_by(esquema) %>% summarise(placas = sum(placas))
  # group_by(month, month_nbr) %>% 
  summarise(placas = n_distinct(original_placa_tractor)) %>% 
  pull(placas) %>% 
  sum

  
  mutate(esquema = tolower(esquema)) %>% 
  pivot_wider(
    names_from = esquema, 
    names_prefix = 'placas_', 
    values_from = placas
  ) %>% 
  mutate(
    across(starts_with('placas_'), replace_na, 0),
    placas_total = placas_dedicado + placas_sencillo
  ) %>% 
  arrange(month_nbr, whse_nbr, desc(placas_total)) %>% 
  ungroup()
  write_csv('output/2021-02-10-tractos-osciosos21.csv')

# Revision de volumen
volumenes_raw <- readRDS('data/20210120-volumen_cedis_tienda_fecha')

volumenes_raw %>% 
  # filter(month == 1) %>% 
  # group_by(month) %>% 
  summarise(cajas = sum(volumen_vnpk))
  

# Revision vol Sams -------------------------------------------------------

new_complete_data <- complete_data %>% 
  left_join(select(location_data, determinante, id_formato), by = c('store_nbr' = 'determinante')) %>% 
  filter(
    red == 'perecederos'
    # id_formato == 'SM'
  )

new_volumen <- volumen %>% 
  left_join(select(location_data, determinante, tipo_store, id_formato), by = c('store_nbr' = 'determinante')) %>% 
  filter(
    whse_nbr2 %in% c(4996, 7458),
    id_formato != 'CEDIS'
    # id_formato == 'SM'
  ) %>% 
  mutate(volumen_vnpk = route_vol)

vnpks <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    summarise(
      cajas = sum(volumen_vnpk),
      .groups = 'drop'
    )
}

new_complete_data %>% 
  filter(volumen_vnpk > 0) %>% 
  vnpks(id_formato)

complete_data %>% 
  left_join(select(location_data, determinante, id_formato), by = c('store_nbr' = 'determinante')) %>% 
  filter(
    red == 'perecederos',
    volumen_vnpk > 0
  ) %>% 
  group_by(id_formato) %>% 
  summarise(
    cajas = sum(volumen_vnpk),
    .groups = 'drop'
  )

new_volumen %>% 
  vnpks(id_formato)

# Comparativa por CD
new_complete_data %>% 
  vnpks(whse_nbr, whse_nbr2) %>% 
  filter(cajas > 0)

new_volumen %>% 
  vnpks(whse_nbr2)

# Comparativa por mes
new_complete_data %>% 
  filter(id_formato == 'SM') %>% 
  vnpks(month_nbr)

volumen %>% 
  mutate(volumen_vnpk = route_vol) %>% 
  filter(whse_nbr2 == 7502, store_nbr == 6584) %>% 
  vnpks(whse_nbr2, store_nbr)

dc_dic %>% 
  filter(whse_nbr2 %in% c(7502, 6239, 6245)) %>% 
  select(whse_nbr, whse_nbr2, whse_name_complete)



# Validacion costos de remolques ------------------------------------------
sumar_costo <- function(data, ...){
  data %>% 
    group_by(...) %>% 
    summarise(costo = sum(jan_month_cost, na.rm = TRUE)+ sum(feb_month_cost, na.rm = TRUE) + sum(mar_month_cost, na.rm = TRUE) + sum(apr_month_cost, na.rm = TRUE) + sum(may_month_cost, na.rm = TRUE) + sum(jun_month_cost, na.rm = TRUE) + sum(jul_month_cost, na.rm = TRUE) + sum(aug_month_cost, na.rm = TRUE) + sum(sep_month_cost, na.rm = TRUE) + sum(oct_month_cost, na.rm = TRUE) + sum(nov_month_cost, na.rm = TRUE) + sum(dec_month_cost, na.rm = TRUE)) 
}

remolques_rem %>%
  filter(!region %in% c('BKHL', 'CMA', 'CPK', 'IDC')) %>% 
  sumar_costo(region)

remolques_rem %>%
  filter(!region %in% c('BKHL', 'CMA', 'CPK', 'IDC', 'CDC MX')) %>%
  sumar_costo(region) %>% 
  pull(costo) %>% 
  sum

a <- remolques_rem %>%
  group_by(category, capacidad, region) %>%
  summarise(
    across(ends_with('month_cost'), sum, na.rm = TRUE),
    .groups = 'keep'
  ) %>%
  # Hay tres regiones en la base de remolques que no están en nuestro diccionario y este inner join quita (BKHL, CMA, IDC), representan 10% del costo total de la base para febrero
  inner_join(dc_rem_region, by = 'region')

a %>% 
  sumar_costo(region)

sum(complete_data$trip_remolque_cost)
sum(complete_data$costo_final_remolque)
sum(processed_data$trip_remolque_cost)
sum(remolques$group_remolque_cost)

remolques %>% 
  filter(month == 'nov') %>%
  pull(group_remolque_cost) %>% 
  sum()

processed_data %>% 
  filter(whse_nbr == 9810) %>% 
  pull(trip_remolque_cost) %>% 
  sum()
  

remolques_rem %>%
  group_by(category, capacidad, region) %>%
  summarise(
    across(ends_with('month_cost'), list(sum = sum), .names = '{col}', na.rm = TRUE),
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
  # 18 llaves que no existe ninguna de sus variantes en viajes (no van a pegar, son 15% del total), 34 llaves que sí existen en ambas bases y de las que quitamos sus variantes inexistentes (si van a pegar, son 75% del total)
  filter(grupo_remolque %in% existing_grupos_remolques | group_existing_keys == 0) %>%
  # mutate(across(ends_with('month_cost'), ~.x/n())) %>% 
  sumar_costo()

remolques %>% 
  filter(month == 'nov') %>% 
  pull(group_remolque_cost) %>% 
  sum()

sum(processed_data$trip_remolque_cost)



# Output para área de Piso y Back -----------------------------------------

complete_data %>% 
  filter(whse_nbr %in% c(9801, 9807), store_nbr == 2344) %>% 
  group_by(whse_nbr, whse_nbr2, whse_name_complete2, store_nbr, esquema, month, month_nbr) %>% 
  summarise(
    n_entregas = n(),
    across(c(total_cost_teorico, km_total_viaje, n_viajes, volumen_vnpk), sum)
  ) %>% 
  mutate(
    costo_por_km = total_cost_teorico / km_total_viaje,
    costo_por_viaje = total_cost_teorico / n_viajes,
    costo_por_entrega = total_cost_teorico / n_entregas,
    costo_por_caja = total_cost_teorico / volumen_vnpk
  ) %>% 
  write_csv('output/2021-02-25-costo-transporte-SC_Toreo.csv')


processed_data %>% 
  filter(
    esquema == 'SENCILLO', 
    equipment_type == 'AR S 53', 
    !ruteo
    # km_total_viaje > 100
  ) %>%
  select(whse_nbr, store_nbr, equipment_type, km_total_viaje, trip_remolque_cost_final, total_cost_teorico) %>% 
  mutate(
    porcentaje_de_costo_total = trip_remolque_cost_final / total_cost_teorico,
    tienda = ifelse(store_nbr == 2344, 'Toreo', 'Otra')
  ) %>% 
  summary()
  # arrange(whse_nbr, store_nbr) %>% 
  ggplot() + 
  geom_point(aes(x= km_total_viaje, y = porcentaje_de_costo_total, color = tienda)) + 
  scale_color_colorblind()


# Proceso para transformar resultados anteriores en actuales
a <- list()

for (i in 1:12) {
  a[i] <- i
}

for (i in 1:12) {
  processed_month <- str_extract(
    string = processed_files[i],
    pattern = '(?<=\\)).*(?=-)'
  )
  processed_month_num <- as.numeric(str_extract(
    string = processed_files[i],
    pattern = '(?<=\\()\\d*(?=\\))'
  ))
  
  a[[i]] <- list(
    data = list(
      processed_data = pre_complete_data[i],
      processed_month = processed_month,
      processed_month_num = processed_month_num
    ),
    metadata = list(
      data_file_name = sprintf(
        '%s-[0.1.0]-(%02d)%s-processing_result',
        Sys.Date() - 1,
        processed_month_num,
        processed_month
      ),
      processing_code_version = '0.1.0',
      processing_date = Sys.time()-(24*60*60)
    )
  )
  
  saveRDS(
    a[[i]],
    sprintf('data/%s', a[[i]]$metadata$data_file_name)
  )
}

a <- set_names(a, months_nbrs$month)

# Resumen equipo FAST ----------------------------

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

tarifas_fast <- complete_data %>%
  filter(
    store_nbr %in% locs$tiendas,
    whse_nbr2 != 0) %>% 
  mutate(
    whse_nbr_transp = whse_nbr,
    whse_nbr_oper = whse_nbr2,
    esquema = case_when(
      esquema == 'DEDICADO' ~ 'ded',
      esquema == 'SENCILLO' ~ 'pxv',
      TRUE ~ 'otro'),
    key = paste(whse_nbr_oper, store_nbr, sep = '-')
  ) %>% 
  select(id_viaje, key, whse_nbr_oper, whse_nbr_transp, store_nbr, esquema, total_cost_teorico, km_total_viaje, entregas_viaje) %>% 
  group_by(key, whse_nbr_oper, whse_nbr_transp, store_nbr, esquema) %>% 
  summarise(
    entregas = n(),
    total_gasto = sum(total_cost_teorico),
    avg_km = mean(km_total_viaje),
    total_km = sum(km_total_viaje),
    costo_por_entrega = sum(total_gasto)/sum(entregas_viaje),
    costo_por_km = sum(total_gasto)/sum(total_km)
  ) %>% 
  pivot_wider(
    names_from = 'esquema',
    values_from = c('entregas', 'total_gasto', 'avg_km', 'total_km', 'costo_por_entrega', 'costo_por_km')
  ) %>%  
  mutate(
    esquema_incluido = case_when(
      !is.na(total_gasto_ded) & is.na(total_gasto_pxv) ~ 'ded',
      is.na(total_gasto_ded) & !is.na(total_gasto_pxv) ~ 'pxv',
      !is.na(total_gasto_ded) & !is.na(total_gasto_pxv) ~ 'ambos',
      TRUE ~ 'ninguno'
    ),
    entregas = replace_na(entregas_ded,0) + replace_na(entregas_pxv,0),
    total_gasto = replace_na(total_gasto_ded,0) + replace_na(total_gasto_pxv,0),
    total_km = replace_na(total_km_ded,0) + replace_na(total_km_pxv,0),
    costo_por_entrega_calc = total_gasto / entregas,
    costo_por_km_calc = total_gasto / total_km
  ) %>% 
  select(
    key,
    whse_nbr_oper,
    whse_nbr_transp,
    store_nbr,
    entregas_ded,
    total_gasto_ded,
    avg_km_ded,
    total_km_ded,
    costo_por_entrega_ded,
    costo_por_km_ded,
    entregas_pxv,
    total_gasto_pxv,
    avg_km_pxv,
    total_km_pxv,
    costo_por_entrega_pxv,
    costo_por_km_pxv,
    esquema_incluido,
    total_gasto,
    total_km,
    entregas,
    costo_por_entrega_calc,
    costo_por_km_calc
  )

write_csv(tarifas_fast, 'output/tarifas_fast_2020.4.csv', na = '0')


# Resumen Kary ------------------------------------------------------------

zones_base <- read_csv('data/20210113-zone_dic.csv')

zones <- zones_base %>% 
  set_names(tolower(names(.))) %>% 
  mutate(origen = tolower(str_remove_all(str_extract(cedis_origen, '(?<=_).*(?=-)'), '\\s'))) %>% 
  # filter(origen == 'villahermosa') %>% 
  select(cd, zona, tienda)

new_comp_summary <- complete_data %>% 
  # filter(whse_nbr %in% c(7761, 9807)) %>% 
  left_join(zones, by = c('store_nbr' = 'tienda', 'whse_nbr' = 'cd')) %>% 
  group_by(whse_nbr, whse_nbr2, whse_name, whse_name_complete2, store_nbr, month, month_nbr, zona) %>% 
  summarise(
    n_entregas = n(),
    across(c(n_viajes, costo_prov, costo_diesel, costo_total_spt, costo_final_tractor, costo_casetas, trip_remolque_cost_final, trip_hub_cost, costo_final_dolly, gasto_variable_calculado, quintas_distribuido, monitoreo_distribuido, total_reclamos, total_aclaraciones, total_regresos, total_cost_teorico, volumen_vnpk), sum),
    .groups = 'drop'
  ) %>% 
  # left_join(
  #   month_vol, 
  #   by = c(
  #     'whse_nbr2' = 'det_origen', 
  #     'store_nbr' = 'det_destino', 
  #     'month_nbr' = 'month'
  #   )
  # ) %>% 
  mutate(
    zone = str_extract(zona, '\\d+'),
    subtotal_comparable = total_cost_teorico - (costo_final_dolly + total_reclamos),
    costo_por_caja = replace_na(total_cost_teorico / volumen_vnpk, 0)
  )

a <- c(
  'entregas' = 'n_entregas',
  'viajes' = 'n_viajes',
  'prov' = 'costo_prov',
  'diesel' = 'costo_diesel',
  'spt' = 'costo_total_spt',
  'tractores' = 'costo_final_tractor',
  'casetas' = 'costo_casetas',
  'remolques' = 'trip_remolque_cost_final',
  'hubs' = 'trip_hub_cost',
  'dollies' = 'costo_final_dolly',
  'variables' = 'gasto_variable_calculado',
  'quintas' = 'quintas_distribuido',
  'monitoreo' = 'monitoreo_distribuido',
  'reclamos' = 'total_reclamos',
  'aclaraciones' = 'total_aclaraciones',
  'regresos' = 'total_regresos',
  'total' = 'total_cost_teorico'
)

# Cedis-tienda_new
new_comp_summary %>% 
  rename(a) %>% 
  write_csv('output/2021-04-05-comp_summary_new.csv')


# Resumen para el análiis de Felipe para Enrique
a <- readRDS('data/2021-05-10-[0.1.0]-(04)apr-processing_result_2021')

a <- a$data$processed_data

b <- a %>% 
  filter(digits(whse_nbr) == 4 & digits(store_nbr) == 4) %>% 
  mutate(whse_name_cargo = coalesce(whse_name_cargo.x, whse_name_cargo.y)) %>% 
  select(-whse_name_cargo.x, -whse_name_cargo.y) %>% 
  group_by(whse_nbr, whse_name, whse_nbr_cargo, whse_name_cargo, store_nbr, tipo_remolque, capacidad_transporte, equipment_type, esquema) %>% 
  summarise(
    entregas = n(),
    across(
      c(n_viajes, km_total_viaje, km_ida_viaje, km_regreso_viaje, costo_total_spt, costo_final_tractor, final_trip_tractor_cost, total_tractores_regresos, costo_casetas, trip_remolque_cost_final, trip_remolque_cost, remolques_falt, trip_hub_cost, costo_final_dolly, gasto_variable_calculado, quintas_distribuido, monitoreo_distribuido, total_reclamos, total_aclaraciones, total_regresos, total_cost_teorico),
      sum
    ),
    .groups = 'drop'
  ) %>% 
  set_names(c(
    'Det Origen',
    'Nombre Origen',
    'Det Cargo',
    'Nombre Cargo',
    'Tienda',
    'Tipo Remolque (R/S)',
    'Cap. Ft.',
    'Tipo de camión',
    'Esquema',
    'Entregas',
    'Viajes',
    'Km total',
    'Km ida',
    'Km regreso',
    'SPT',
    'Total Tractor',
    'Real Tractor',
    'Prorateado regresos Tractor',
    'Casetas',
    'Total Remolque',
    'Real Remolque',
    'Prorateado Remolque',
    'Hubs',
    'Dollies',
    'Gastos Variables',
    'Quintas',
    'Monitoreo',
    'Reclamos',
    'Aclaraciones',
    'Regresos',
    'Total'
  ))

write_csv(b, 'output/20210510-resumen_tarifas1.csv')

 