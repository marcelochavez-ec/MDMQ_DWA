# rm(list = ls())

library("shiny")
library("shinythemes")
library("RPostgreSQL")

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})

# Crear una conexión a la base de datos PostgreSQL
con <- dbConnect(PostgreSQL(),
                 dbname = "postgres",
                 host = "localhost",
                 port = 5490,
                 user = "postgres",
                 password = "marce")

dbListTables(con)

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
)

loadUIFiles <- function() {
  ui_files <- c("ui_Inicio.R",
                "ui_Exploratorio.R",
                "ui_Reportes.R",
                "ui_Geovisor.R")
  for (file in ui_files) {
    source(file)
  }
}

loadServerFiles <- function() {
  server_files <- c("server_Inicio.R",
                    # "server_Exploratorio.R",
                    "server_Reportes.R",
                    "server_Geovisor.R")
  for (file in server_files) {
    source(file)
  }
}

# Cargar los archivos de UI y servidor

loadUIFiles()
loadServerFiles()

