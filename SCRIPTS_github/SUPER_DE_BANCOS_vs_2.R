rm(list=ls(all.names = TRUE))

library(data.table)
library(openxlsx)
library(dplyr)
library(purrr)
library(lubridate) # Para la función month y year

# FUNCIÓN PARA REMOVER CARACTERES ASCII -----------------------------------
remove_ascii <- function(column_name) {
  column_name <- gsub('Á', 'A', column_name)
  column_name <- gsub('É', 'E', column_name)
  column_name <- gsub('Í', 'I', column_name)
  column_name <- gsub('Ó', 'O', column_name)
  column_name <- gsub('Ú', 'U', column_name)
  column_name <- gsub('á', 'a', column_name)
  column_name <- gsub('é', 'e', column_name)
  column_name <- gsub('í', 'i', column_name)
  column_name <- gsub('ó', 'o', column_name)
  column_name <- gsub('ú', 'u', column_name)
  column_name <- gsub('\\.', '', column_name)  # Eliminación de puntos
  column_name <- gsub(' ', '_', column_name)   # Reemplazo de espacios por guiones bajos
  column_name <- gsub('Ñ', 'NI', column_name)  # Reemplazo de ñ por ni
  return(column_name)
}

# Leer el archivo .txt desde la 4ta fila y aplicar la función a los nombres de las columnas
df_sb <- fread("DB_onedrive/SB/inversion_societaria1719608459513.txt",
               sep = "\t",
               skip = 4,
               quote = "") %>%
  setNames(map_chr(names(.), remove_ascii))

# Proceso de limpieza y transformación:
df_sb <- df_sb %>%
  mutate(FECHA_INGRESO_ACTO_JURIDICO = as.Date(FECHA_INGRESO_ACTO_JURIDICO, format = "%d/%m/%Y"),
         MES = month(FECHA_INGRESO_ACTO_JURIDICO),
         TRIMESTRE = case_when(
           MES %in% 1:3 ~ "Q1",
           MES %in% 4:6 ~ "Q2",
           MES %in% 7:9 ~ "Q3",
           MES %in% 10:12 ~ "Q4"),
         ANIO = year(FECHA_INGRESO_ACTO_JURIDICO),
         TRIMESTRE_ANIO = paste(TRIMESTRE, ANIO, sep = "-")) %>%
  select(-V30, -MES, -TRIMESTRE)

# Creación de reportes anual y trimestral en un solo bloque
reportes <- df_sb %>%
  group_by(ANIO, TRIMESTRE_ANIO) %>%
  summarise(
    TOTAL_QUITO_ANUAL = sum(CAPITAL_POR_ACTO_JURIDICO[TIPO_INVERSION == "EXTRANJERA DIRECTA" & CANTON == "QUITO"], na.rm = TRUE),
    TOTAL_NACIONAL_ANUAL = sum(CAPITAL_POR_ACTO_JURIDICO[TIPO_INVERSION == "NACIONAL"], na.rm = TRUE),
    TOTAL_QUITO_TRIMESTRAL = sum(CAPITAL_POR_ACTO_JURIDICO[TIPO_INVERSION == "EXTRANJERA DIRECTA" & CANTON == "QUITO"], na.rm = TRUE),
    TOTAL_NACIONAL_TRIMESTRAL = sum(CAPITAL_POR_ACTO_JURIDICO[TIPO_INVERSION == "NACIONAL"], na.rm = TRUE)
  ) %>%
  filter(!is.na(TRIMESTRE_ANIO)) # Filtrar para evitar filas nulas

# Separación de reporte anual y trimestral
reporte_anual <- reportes %>%
  filter(!grepl("Q[1-4]", TRIMESTRE_ANIO)) %>%
  select(ANIO, TOTAL_QUITO_ANUAL, TOTAL_NACIONAL_ANUAL) %>%
  formato contiene J tra formato ? degli
