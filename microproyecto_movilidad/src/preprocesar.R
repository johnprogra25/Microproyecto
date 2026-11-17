# Limpieza y estandarización de viajes_raw.csv

base_original <- read.csv2("data/raw/viajes_raw.csv",
                           na.strings = c("NA", "N/A", "-", ""),
                           stringsAsFactors = FALSE)

base <- base_original
n_inicial <- nrow(base)

# 1. Duplicados exactos y por id_viaje
n_dup_exactos <- sum(duplicated(base))
base <- base[!duplicated(base), ]

n_dup_id <- sum(duplicated(base$id_viaje))
# Si existen IDs duplicados con distinta información, conservamos la primera ocurrencia
base <- base[!duplicated(base$id_viaje), ]
n_post_dup <- nrow(base)

# 2. Estandarización de categorías (texto, tildes y espacios)
limpiar_texto <- function(x) {
  x <- tolower(trimws(x))
  x <- chartr("áéíóú", "aeiou", x)
  return(x)
}

base$zona_origen <- limpiar_texto(base$zona_origen)
base$zona_destino <- limpiar_texto(base$zona_destino)
base$medio_transporte <- limpiar_texto(base$medio_transporte)
base$evaluo <- limpiar_texto(base$evaluo)

# Revisar valores únicos tras estandarizar
cat("--- Valores únicos categóricos ---\n")
print(table(base$zona_origen, useNA = "ifany"))
print(table(base$zona_destino, useNA = "ifany"))
print(table(base$medio_transporte, useNA = "ifany"))

# Normalizar evaluo
base$evaluo[base$evaluo %in% c("si", "s", "yes", "1", "true")] <- "si"
base$evaluo[base$evaluo %in% c("no", "n", "0", "false")] <- "no"
print(table(base$evaluo, useNA = "ifany"))

# 3. Conversión y validación de fechas
f1 <- as.Date(trimws(base$fecha), format = "%d/%m/%Y")
f2 <- as.Date(trimws(base$fecha), format = "%Y-%m-%d")
f3 <- as.Date(trimws(base$fecha), format = "%d-%m-%Y")
base$fecha_limpia <- as.Date(ifelse(!is.na(f1), f1,
                                    ifelse(!is.na(f2), f2, f3)),
                             origin = "1970-01-01")

n_fecha_na_orig <- sum(is.na(base$fecha))
n_fecha_invalida <- sum(is.na(base$fecha_limpia)) - n_fecha_na_orig
cat("\nFechas NA originales:", n_fecha_na_orig, "| Fechas inválidas no parseadas:", n_fecha_invalida, "\n")
if (n_fecha_invalida > 0) {
  cat("Ejemplos de fechas no parseadas:\n")
  print(unique(base$fecha[is.na(base$fecha_limpia) & !is.na(base$fecha)]))
}

# 4. Inspección y normalización de hora_salida
h_txt <- trimws(base$hora_salida)
# Detectar formatos: HH:MM, HHhMM, H:MM AM/PM
parse_hora <- function(x) {
  res <- rep(NA_real_, length(x))
  # Formato 24h con : o h (ej. 17:10 o 20h55)
  m24 <- grepl("^\\d{1,2}[:hH]\\d{2}$", x)
  if (any(m24)) {
    partes <- strsplit(gsub("[hH]", ":", x[m24]), ":")
    hh <- as.numeric(sapply(partes, `[`, 1))
    mm <- as.numeric(sapply(partes, `[`, 2))
    valid <- hh >= 0 & hh <= 23 & mm >= 0 & mm <= 59
    res[m24][valid] <- hh[valid] + mm[valid] / 60
  }
  # Formato 12h AM/PM (ej. 8:00 PM)
  m12 <- grepl("^\\d{1,2}:\\d{2}\\s*(AM|PM|am|pm)$", x)
  if (any(m12)) {
    ampm <- toupper(gsub(".*(AM|PM|am|pm)$", "\\1", x[m12]))
    hora_str <- trimws(gsub("(AM|PM|am|pm)$", "", x[m12]))
    partes <- strsplit(hora_str, ":")
    hh <- as.numeric(sapply(partes, `[`, 1))
    mm <- as.numeric(sapply(partes, `[`, 2))
    valid <- hh >= 1 & hh <= 12 & mm >= 0 & mm <= 59
    hh_24 <- ifelse(ampm == "PM" & hh != 12, hh + 12,
                    ifelse(ampm == "AM" & hh == 12, 0, hh))
    res[m12][valid] <- hh_24[valid] + mm[valid] / 60
  }
  return(res)
}

base$hora_decimal <- parse_hora(h_txt)
n_hora_invalida <- sum(is.na(base$hora_decimal))
cat("\nHoras inválidas o no parseadas:", n_hora_invalida, "\n")
if (n_hora_invalida > 0) {
  print(unique(base$hora_salida[is.na(base$hora_decimal)]))
}

# 5. Conversión de tiempo_viaje a minutos (variable principal)
txt <- tolower(trimws(gsub(",", ".", base$tiempo_viaje, fixed = TRUE)))
# Detectar explícitamente formato compuesto tipo "1h 30m" o "1:30" si existiera
es_horas <- grepl("hora|h", txt) & !grepl("min", txt)
valor <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", txt)))
base$viaje_min <- ifelse(es_horas, valor * 60, valor)

n_viaje_na_orig <- sum(is.na(base$tiempo_viaje))
n_viaje_texto_inv <- sum(is.na(base$viaje_min)) - n_viaje_na_orig
n_viaje_no_pos <- sum(!is.na(base$viaje_min) & base$viaje_min <= 0)
n_viaje_extremo <- sum(!is.na(base$viaje_min) & base$viaje_min > 180)

cat("\n--- Diagnóstico tiempo_viaje ---\n")
cat("NA originales:", n_viaje_na_orig, "\n")
cat("Texto no numérico (ej. pendiente):", n_viaje_texto_inv, "\n")
cat("Valores <= 0:", n_viaje_no_pos, "\n")
cat("Valores > 180 min (extremos conservados si son plausibles):", n_viaje_extremo, "\n")
if (n_viaje_extremo > 0) {
  print(base[which(base$viaje_min > 180), c("id_viaje", "medio_transporte", "tiempo_viaje", "viaje_min")])
}

# 6. Limpieza y validación de calificación
cal_num <- suppressWarnings(as.numeric(trimws(base$calificacion)))
n_cal_invalida <- sum(!is.na(base$calificacion) & (is.na(cal_num) | !(cal_num %in% 1:5)))
cat("\nCalificaciones inválidas (fuera de 1-5 o texto):", n_cal_invalida, "\n")
if (n_cal_invalida > 0) {
  print(unique(base$calificacion[!is.na(base$calificacion) & (is.na(cal_num) | !(cal_num %in% 1:5))]))
}
base$calificacion <- factor(cal_num, levels = 1:5, ordered = TRUE)

# 7. Guardar copia depurada ANTES de filtrar población analítica
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv2(base, "data/processed/viajes.csv", row.names = FALSE, na = "NA")

# 8. Definir población analítica: viajes con duración positiva y válida
base_analitica <- base[!is.na(base$viaje_min) & base$viaje_min > 0, ]
n_final <- nrow(base_analitica)

cat("\n--- Resumen de filas ---\n")
cat("Filas iniciales:", n_inicial, "\n")
cat("Tras eliminar duplicados:", n_post_dup, "\n")
cat("Población analítica final (viaje_min > 0 y válido):", n_final, "\n")