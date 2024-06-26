rm(list=ls(all.names = T))

library(tidyverse)
library(RPostgreSQL)

con_postgres <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost", 
                          port = 5432,
                          user = "postgres", 
                          password = "marce")


db_pib <- readxl::read_excel("D:/MDMQ/BDD/DEUDA_PUBLICA_MEF_vs_2/BDD_DEUDA_PUBLICA_2011_2024.xlsx",
                          sheet = "COPY")

db_pib$id_pib <- seq_len(nrow(db_pib))

# db_pib <- db_pib %>% 
#   mutate(fecha_2 = format(as.Date(fecha_2), "%Y-%m")) 

# ALMACENAMIENTO EN POSTGRESQL --------------------------------------------

field_types <- c(
  'id_pib'='numeric',
  'grafico_1'="text",
  'grafico_2'="text",
  'metodologia'="text",
  'fecha_1'="text",
  'fecha_2'="date",
  'anio'="integer",
  'mes'="text",
  'nro_mes'="integer",
  'deuda_externa'="numeric",
  'deuda_externa_pib'="numeric",
  'deuda_interna'="numeric",
  'deuda_interna_pib'="numeric",
  'total_otros_pasivos'="numeric",
  'pib'="numeric",
  'deuda_total'="numeric",
  'deuda_total_pib'="numeric")

dbWriteTable(
    con_postgres,
    name = DBI::Id(schema = "c_economico",
                   table = "db_pib"),
    value = db_pib,
    overwrite = TRUE,
    row.names = FALSE,
    field.types = field_types)

print(dbDisconnect(con_postgres))

