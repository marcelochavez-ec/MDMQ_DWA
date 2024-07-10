rm(list=ls(all.names = T))

library(tidyverse)
library(openxlsx)
library(RPostgreSQL)
library(skimr)
library(arrow)

# source("SCRIPTS/exploratorio.R")

postgres_conexion <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost", 
                          port = 5432,
                          user = "postgres", 
                          password = "marce")

# Definir la función para leer y apilar múltiples archivos Excel
# read_and_stack_excel <- function(directory_path) {
#     
#     # Obtener la lista de archivos en el directorio
#     excel_files <- list.files(directory_path, 
#                               full.names = TRUE,
#                               pattern = "\\.xlsx$", 
#                               recursive = TRUE)
#     
#     # Inicializar un dataframe vacío para almacenar los resultados
#     df_compilado <- data.frame()
#     
#     # Iterar sobre cada archivo
#     for (file_path in excel_files) {
#         # Leer el archivo Excel
#         tryCatch({
#             df <- readxl::read_excel(file_path, 
#                                      sheet = 1,
#                                      skip = 1)
#             
#             # Verificar si se ha leído correctamente el archivo
#             if (nrow(df) > 0) {
#                 # Convertir las variables que contienen "FECHA" en su nombre a tipo fecha
#                 fecha_vars <- grep("FECHA", 
#                                    names(df), 
#                                    value = TRUE)
#                 
#                 for (var in fecha_vars) {
#                     df[[var]] <- as.Date(format(as.POSIXct(df[[var]], 
#                                                            format = "%d/%m/%Y %H:%M:%S"), 
#                                                 "%Y-%m-%d"))
#                 }
#                 
#                 # Agregar la columna "ANIO_OTORGAMIENTO"
#                 if ("FECHA_OTORGAMIENTO" %in% names(df)) {
#                     df <- df %>% mutate(ANIO_OTORGAMIENTO = year(FECHA_OTORGAMIENTO),
#                                         MES_OTORGAMIENTO = month(FECHA_OTORGAMIENTO))
#                 } else {
#                     warning(paste("La columna FECHA_OTORGAMIENTO no se encontró en el archivo", file_path))
#                 }
#                 
#                 # Apilar el dataframe al dataframe compilado
#                 df_compilado <- dplyr::bind_rows(df_compilado, df)
#             } else {
#                 warning(paste("El archivo", file_path, "no contiene datos."))
#             }
#             
#         }, error = function(e) {
#             warning(paste("Error al leer el archivo Excel", file_path, ":", e$message))
#         })
#     }
#     
#     # Convertir los nombres de las columnas a minúsculas si df_compilado no está vacío
#     if (nrow(df_compilado) > 0) {
#         colnames(df_compilado) <- tolower(colnames(df_compilado))
#     }
#     
#     # Devolver el dataframe compilado
#     return(df_compilado)
# }

# Usar la función para leer y apilar los archivos Excel
# db_luae <- read_and_stack_excel("D:/MDMQ/BDD/LUAE/")

db_luae <- dbGetQuery(postgres_conexion, "SELECT * FROM c_economico.db_luae")

db_luae <- db_luae %>%
  mutate(descripcion_categorica = case_when(
    categoria == "1" ~ "LUAE Simplificada",
    categoria == "2" ~ "LUAE Ordinaria",
    categoria == "3" ~ "LUAE Especial",
    TRUE ~ NA_character_  # Maneja otros casos, si existen
  ))

# write_parquet(db_luae, "DB/LUAE/db_luae.parquet")

# r_exploratorio_luae <- exploratorio(db_luae)
# 
# write.xlsx(r_exploratorio_luae, "DB/LUAE/r_exploratorio_luae.xlsx",
#            sheetName="METADATO_LUAE")

# write.xlsx(r_exploratorio_luae, "DB/LUAE/r_exploratorio_luae.xlsx",
#            sheetName="METADATO_LUAE")

# Verificar el resultado
# str(db_luae)

# REPORTE NÚMERO DE LICENCIAS POR AÑO -------------------------------------

reporte_anual <- db_luae %>%
    
    # Este criterio no creo q sea necesario:
    
    # mutate(id_anio=paste0(anio_otorgamiento,
    #                       "_",
    #                       numero_licencia)) %>% 
    
  filter(estado_emision %in% c("EMITIDA",
                               "EMITIDA PARCIAL",
                               "FINALIZADA",
                               "OTORGADA",
                               "OTORGADA PARCIAL") &
         !estado_bpm %in% c("ABORTADO",
                            "FINALIZADO CON ERROR")) %>% 
    select(numero_tramite,
           anio_otorgamiento,
           mes_otorgamiento) %>%
  
  filter(mes_otorgamiento == 3) %>% 
    
    distinct() %>% 
    
    group_by(anio_otorgamiento,
             mes_otorgamiento) %>%
    
    summarize(total_licencias = n())

reporte_anual


# REPORTE ACUMULADO MENSUAL -----------------------------------------------

reporte_acumulado <- db_luae %>%
    
    # Reglas de cálculo del Indicador Acumulado según los meses que se consideren:  
    
  filter(estado_emision %in% c("EMITIDA",
                               "EMITIDA PARCIAL",
                               "FINALIZADA",
                               "OTORGADA",
                               "OTORGADA PARCIAL") &
           !estado_bpm %in% c("ABORTADO",
                              "FINALIZADO CON ERROR") &
         lubridate::month(fecha_otorgamiento) %in% 1:3) %>%
    
    select(numero_tramite,
           anio_otorgamiento) %>% 
    
    distinct() %>% 
    
    group_by(anio_otorgamiento) %>%
    
    summarize(total_licencias_acumuladas = n())

reporte_acumulado

# FRECUENCIAS: ---------------------------------------------------------

# total_licencias_no_repetidas <- as.data.frame(table(db_luae$numero_licencia))
# cat("EL TOTAL DE LICENCIAS ÚNICAS EN LA SERIE HISTÓRICA: ",dim(total_licencias_no_repetidas)[[1]])

# licencias_repetidas <- as.data.frame(table(db_luae$numero_licencia,
#                                            db_luae$anio_otorgamiento,
#                                            db_luae$movimiento_actual))

# ALMACENAMIENTO EN POSTGRESQL --------------------------------------------

field_types <- c(
    "anio_otorgamiento" = "numeric",
    "descripcion_categorica" = "text",
    "mes_otorgamiento" = "numeric",
    "numero_tramite" = "text",
    "zona_predio" = "text",
    "ruc_licencia" = "text",
    "numero_documento_identidad" = "text",
    "representante_razon_social" = "text",
    "numero_licencia" = "text",
    "patente" = "text",
    "razon_social" = "text",
    "predio" = "text",
    "actividad_economica" = "text",
    "administracion_zonal" = "text",
    "codigo_ciiu" = "text",
    "idskelta" = "text",
    "fecha_inicio" = "date",
    "fecha_otorgamiento" = "date",
    "fecha_fin" = "date",
    "estado_emision" = "text",
    "nombre_comercial" = "text",
    "estado_bpm" = "text",
    "movimiento_actual" = "text",
    "tramite_actual" = "text",
    "establecimiento_numero" = "text",
    "categoria" = "text",
    "mail" = "text",
    "telefono" = "text",
    "direccion" = "text",
    "envio_correo" = "text",
    "informe_icus" = "text",
    "cod" = "text",
    "check_cod" = "text",
    "resultado_icus" = "text",
    "tipologia_icus" = "text",
    "tram_procedimiento" = "text",
    "usuario_ingreso" = "text",
    "fecha_vencimiento" = "date",
    "usuario_otorgamiento" = "text"
)
 
dbWriteTable(
    postgres_conexion,
    name = DBI::Id(schema = "c_economico",
                   table = "db_luae"),
    value = db_luae,
    overwrite = TRUE,
    row.names = FALSE,
    field.types = field_types
)




