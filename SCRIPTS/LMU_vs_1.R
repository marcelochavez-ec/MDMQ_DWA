
# LIBRERÍAS Y CARGA DE DATOS ----------------------------------------------

rm(list=ls(all.names = T))

library(tidyverse)
library(openxlsx)

lum_2015 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2015.xlsx",
                                sheet = "ListadoTramites6_15 PM",
                                sep.names = "_")

lum_2015[] <- lapply(lum_2015, as.character)

# Función para eliminar tildes
remove_tildes <- function(column_name) {
  column_name <- gsub('Á', 'A', column_name)
  column_name <- gsub('É', 'E', column_name)
  column_name <- gsub('Í', 'I', column_name)
  column_name <- gsub('Ó', 'O', column_name)
  column_name <- gsub('Ú', 'U', column_name)
  column_name <- gsub('\\.', '', column_name) # Eliminación de puntos
  return(column_name)
}

colnames(lum_2015) <- toupper(colnames(lum_2015))

# Aplicar la función a los nombres de las columnas
colnames(lum_2015) <- sapply(colnames(lum_2015), remove_tildes)

lum_2016 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2016.xlsx",
                                sheet = "ListadoTramites8_18 AM",
                                sep.names = "_")

lum_2016[] <- lapply(lum_2016, as.character)

colnames(lum_2016) <- toupper(colnames(lum_2016))

# Aplicar la función a los nombres de las columnas
colnames(lum_2016) <- sapply(colnames(lum_2016), remove_tildes)

lum_2017 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2017.xlsx",
                                sheet = "ListadoTramites9_00 AM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2017[] <- lapply(lum_2017, as.character)

colnames(lum_2017) <- toupper(colnames(lum_2017))

# Aplicar la función a los nombres de las columnas
colnames(lum_2017) <- sapply(colnames(lum_2017), remove_tildes)

lum_2018 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2018.xlsx",
                                sheet = "ListadoTramites9_23 AM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2018[] <- lapply(lum_2018, as.character)

colnames(lum_2018) <- toupper(colnames(lum_2018))

# Aplicar la función a los nombres de las columnas
colnames(lum_2018) <- sapply(colnames(lum_2018), remove_tildes)

lum_2019 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2019.xlsx",
                                sheet = "ListadoTramites11_35 AM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2019[] <- lapply(lum_2019, as.character)

colnames(lum_2019) <- toupper(colnames(lum_2019))

# Aplicar la función a los nombres de las columnas
colnames(lum_2019) <- sapply(colnames(lum_2019), remove_tildes)

lum_2020 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2020.xlsx",
                                sheet = "ListadoTramites9_18 AM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2020[] <- lapply(lum_2020, as.character)

colnames(lum_2020) <- toupper(colnames(lum_2020))

# Aplicar la función a los nombres de las columnas
colnames(lum_2020) <- sapply(colnames(lum_2020), remove_tildes)

lum_2021 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2021.xlsx",
                                sheet = "ListadoTramites9_59 AM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2021[] <- lapply(lum_2021, as.character)

colnames(lum_2021) <- toupper(colnames(lum_2021))

# Aplicar la función a los nombres de las columnas
colnames(lum_2021) <- sapply(colnames(lum_2021), remove_tildes)

lum_2022 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2022.xlsx",
                                sheet = "ListadoTramites4_41 PM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2022[] <- lapply(lum_2022, as.character)

colnames(lum_2022) <- toupper(colnames(lum_2022))

# Aplicar la función a los nombres de las columnas
colnames(lum_2022) <- sapply(colnames(lum_2022), remove_tildes)

lum_2023 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2023.xlsx",
                                sheet = "ListadoTramites5_06 PM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2023[] <- lapply(lum_2023, as.character)

colnames(lum_2023) <- toupper(colnames(lum_2023))

# Aplicar la función a los nombres de las columnas
colnames(lum_2023) <- sapply(colnames(lum_2023), remove_tildes)

lum_2024 <- openxlsx::read.xlsx("D:/MDMQ/BDD/LMU/LMU_2024.xlsx",
                                sheet = "ListadoTramites9 10 AM",
                                colNames = T,
                                detectDates = F,
                                check.names = T,
                                sep.names = "_")

lum_2024[] <- lapply(lum_2024, as.character)

colnames(lum_2024) <- toupper(colnames(lum_2024))

# Aplicar la función a los nombres de las columnas
colnames(lum_2024) <- sapply(colnames(lum_2024), remove_tildes)

db_lmu <- bind_rows(lum_2015,
                    lum_2016,
                    lum_2017,
                    lum_2018,
                    lum_2019,
                    lum_2020,
                    lum_2021,
                    lum_2022,
                    lum_2023,
                    lum_2024)

# Función para convertir números de serie de Excel a fechas en formato "YYYY-MM-DD"
convertir_excel_a_fecha <- function(fecha_excel) {
  as.Date(as.numeric(fecha_excel), 
          origin = "1899-12-30")
}

# Aplicar la función a la columna FECHA_DE_EMISION
db_lmu$FECHA_DE_EMISION_NEW <- convertir_excel_a_fecha(db_lmu$FECHA_DE_EMISION)

# Extraer el año de la columna FECHA_DE_EMISION
db_lmu$ANIO <- format(db_lmu$FECHA_DE_EMISION_NEW, "%Y")

db_lmu <- db_lmu %>% 
  select(ANIO,
         FECHA_DE_EMISION_NEW,
         FECHA_DE_EMISION,
         everything())

colnames(db_lmu) <- tolower(names(db_lmu))

library(RPostgreSQL)
library(skimr)

source("D:/MDMQ/SCRIPTS/exploratorio.R")

con_postgres <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost", 
                          port = 5432,
                          user = "postgres", 
                          password = "marce")

dbWriteTable(
    con_postgres,
    name = DBI::Id(schema = "c_economico",
                   table = "db_lmu"),
    value = db_lmu,
    overwrite = TRUE,
    row.names = FALSE)

# ARREGLO DE FECHAS -------------------------------------------------------

# Función para convertir números de serie de Excel a fechas
# excel_fecha_a_fecha_r <- function(excel_fecha) {
#   as.Date(excel_fecha, origin = "1899-12-30")
# }
# 
# # Función para identificar columnas que contienen "FECHA" y crear nuevas variables con las fechas convertidas
# convertir_fechas <- function(df) {
#   # Identificar las columnas que contienen "FECHA" en su nombre
#   fecha_cols <- grep("FECHA", names(df), ignore.case = TRUE, value = TRUE)
#   
#   # Iterar sobre las columnas identificadas
#   for (col in fecha_cols) {
#     # Crear el nombre de la nueva columna
#     new_col_name <- paste0(col, "_COPY")
#     
#     # Verificar si la columna es numérica o carácter y convertir adecuadamente
#     if (is.numeric(df[[col]])) {
#       df[[new_col_name]] <- excel_fecha_a_fecha_r(df[[col]])
#     } else {
#       df[[new_col_name]] <- as.Date(df[[col]], format = "%m/%d/%Y")
#     }
#   }
#   
#   return(df)
# }
# 
# db_lmu <- convertir_fechas(db_lmu) %>% 
#   select(FECHA_INICIO,
#          FECHA_INICIO_COPY,
#          FECHA_DE_EMISION,
#          FECHA_DE_EMISION_COPY,
#          FECHA_RENOVACION,
#          FECHA_RENOVACION_COPY,
#          everything())

# METADATOS ---------------------------------------------------------------

# meta_lum <- as.data.frame(names(db_lmu)) %>%
#   rename("VARIABLES"="names(db_lmu)") %>%
#   mutate(TIPO_VARIABLE = sapply(db_lmu, class))
# 
# openxlsx::write.xlsx(meta_lum,
#                      "D:/MDMQ/REPORTES/METADATO_LUM.xlsx",
#                      sheetName = "LUM")

# REPETIDOS ---------------------------------------------------------------

repetidos <- as.data.frame(table(db_lmu$NO_LICENCIA)) %>% 
  rename("NUMERO_LICENCIA"="Var1",
         "FRECUENCIA"="Freq") %>% 
  arrange(desc(FRECUENCIA))

# REPORTES POR ANIO -------------------------------------------------------

anual <- db_lmu %>%
  
  # Regla para contar LUM por Año:
  filter(!is.na(FECHA_DE_EMISION_NEW) & # Fechas no vacías
         MACROPROCESO_LICENCIA %in% c("3","4")) %>% # Las LMU 20
  
  select(ANIO,
         NO_LICENCIA,
         FECHA_DE_EMISION_NEW,
         ESTADO_TRAMITE,
         ESTADO_DEL_FLUJO,
         TIPO_PROYECTO) %>% 
  
  distinct() %>% 
  
  group_by(ANIO) %>%
  count(NO_LICENCIA) %>%
  summarize(Total = sum(n))

anual

# openxlsx::write.xlsx(anual,
#                      "D:/MDMQ/REPORTES/LUM_POR_ANIO.xlsx",
#                      rowNames = FALSE,
#                      sheetName = "LicenciasAnioLUM")

# REPORTE DE ENERO A MAYO -------------------------------------------------

# lmu20 <- db_lmu %>%
#   select(MACROPROCESO_LICENCIA,
#          MACROPROCESO)
# 
# db_lmu_filtrado <- subset(lmu20, grepl("LMU 20", MACROPROCESO))

acumulada <- db_lmu %>%
  
  # Regla para contar LUM por mes:
  
  mutate(MES_EMISION = month(FECHA_DE_EMISION_NEW)) %>% 
  
  filter(!is.na(FECHA_DE_EMISION_NEW) & # Las fechas no vacías
           
         lubridate::month(FECHA_DE_EMISION_NEW) %in% 1:4 & # El acumulado de enero a X mes
           
         MACROPROCESO_LICENCIA %in% c("3","4")) %>% # Filtro de las LMU 20
  
  select(ANIO,
         NO_LICENCIA,
         FECHA_DE_EMISION_NEW,
         MES_EMISION) %>% 
  
  distinct() %>% 
  
  group_by(ANIO) %>%
  count(NO_LICENCIA) %>%
  summarize(Total = sum(n))

acumulada

# openxlsx::write.xlsx(acumulada,
#                      "D:/MDMQ/REPORTES/LUM_ENERO_MAYO.xlsx",
#                      rowNames = FALSE,
#                      sheetName = "LUMEneroMayo")
