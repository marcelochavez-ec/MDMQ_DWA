library(shiny)
library(DT)
library(tidyverse)
library(openxlsx)
library(RPostgreSQL)
library(lubridate)
library(highcharter)
library(shinyjs)

# Configuraciones de Shiny
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Conectar a la base de datos
con_postgres <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost", 
                          port = 5432,
                          user = "postgres", 
                          password = "marce")

# Cargar datos desde la base de datos
db_luae <- dbGetQuery(con_postgres, 
                      "SELECT anio_otorgamiento,
                              numero_licencia,
                              estado_emision,
                              estado_bpm,
                              fecha_otorgamiento,
                              fecha_inicio, 
                              fecha_fin, 
                              fecha_vencimiento,
                              zona_predio, 
                              administracion_zonal, 
                              actividad_economica,
                              movimiento_actual
                       FROM c_economico.db_luae")

# Crear la aplicación Shiny
ui <- fluidPage(
  titlePanel("CUBO - LUAEs"),
  tabsetPanel(
    tabPanel("CUBO",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("dimensions", "Dimensiones:", 
                             choices = c("ADMINISTRACIÓN ZONAL" = "administracion_zonal",
                                         "ESTADO DE EMISIÓN" = "movimiento_actual",
                                         "ACTIVIDAD ECONÓMICA" = "actividad_economica"), 
                             multiple = TRUE),
                 checkboxGroupInput("measures", "Medidas:", 
                                    choices = "TOTAL DE LUAEs", 
                                    selected = "TOTAL DE LUAEs"),
                 actionButton("run_button", "Ejecutar consulta")
               ),
               mainPanel(
                 width = 9,
                 DTOutput("pivot_table")
               )
             )),
    tabPanel("BASE",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 textInput("base_num_licencia", "Número de Licencia:", 
                           placeholder = 'Ingrese uno o más números de licencia separados por comas'),
                 actionButton("base_filter_button", "Filtrar")
               ),
               mainPanel(
                 width = 9,
                 DTOutput("base_table")
               )
             ))
  )
)

server <- function(input, output, session) {
  
  # Asegurarse de cerrar la conexión a la base de datos al detener la aplicación
  onSessionEnded(function() {
    dbDisconnect(con_postgres)
  })
  
  pivot_data <- eventReactive(input$run_button, {
    req(input$dimensions)
    
    # Filtrar los datos con base en las selecciones de las dimensiones
    filtered_data <- db_luae
    
    # Agrupar por las dimensiones seleccionadas
    data <- filtered_data %>%
      group_by(anio_otorgamiento, across(all_of(input$dimensions))) %>%
      summarise(TOTAL_DE_LUAEs = n(), .groups = 'drop')
    
    data
  })
  
  output$pivot_table <- renderDT({
    datatable(pivot_data(), options = list(pageLength = 15, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  filtered_base_data <- eventReactive(input$base_filter_button, {
    # Convertir el input base_num_licencia en un vector
    base_num_licencia_vec <- unlist(strsplit(input$base_num_licencia, ","))
    base_num_licencia_vec <- trimws(base_num_licencia_vec)  # Eliminar espacios en blanco adicionales
    
    db_luae %>%
      filter(if (length(base_num_licencia_vec) == 0) TRUE else numero_licencia %in% base_num_licencia_vec)
  })
  
  output$base_table <- renderDT({
    datatable(filtered_base_data(), options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
