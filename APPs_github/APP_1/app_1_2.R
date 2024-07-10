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
                                         "ESTADO DE EMISIÓN" = "estado_emision",
                                         "ACTIVIDAD ECONÓMICA" = "actividad_economica"), 
                             multiple = TRUE),
                 checkboxGroupInput("measures", "Medidas:", 
                                    choices = "TOTAL DE LUAEs", 
                                    selected = "TOTAL DE LUAEs"),
                 actionButton("generate_report", "Generar Reporte", icon = icon("file-alt")),
                 br(),
                 radioButtons("report_frequency", "Frecuencia del Reporte:", 
                              choices = c("ANUAL", "ACUMULADO MENSUAL"), selected = "ANUAL"),
                 uiOutput("conditional_filters"),
                 br(),
                 downloadButton("download_report", "Descargar Reporte en Excel", icon = icon("download"))
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
  
  # Función reactiva para actualizar los filtros condicionales del reporte
  output$conditional_filters <- renderUI({
    if (input$report_frequency == "ANUAL") {
      return(NULL)
    } else {
      fluidRow(
        column(6,
               selectInput("report_year", "Año:", choices = c("TODOS", unique(db_luae$anio_otorgamiento)), selected = "TODOS")),
        column(6,
               selectInput("report_month", "Mes:", 
                           choices = c("TODOS", 
                                       "Enero" = 1, 
                                       "Febrero" = 2, 
                                       "Marzo" = 3, 
                                       "Abril" = 4, 
                                       "Mayo" = 5, 
                                       "Junio" = 6, 
                                       "Julio" = 7, 
                                       "Agosto" = 8, 
                                       "Septiembre" = 9, 
                                       "Octubre" = 10, 
                                       "Noviembre" = 11, 
                                       "Diciembre" = 12), 
                           selected = "TODOS"))
      )
    }
  })
  
  # Función reactiva para generar los datos del reporte
  report_data <- eventReactive(input$generate_report, {
    if (input$report_frequency == "ANUAL") {
      # Filtrar por año otorgamiento
      report <- db_luae %>%
        group_by(anio_otorgamiento) %>%
        summarise(TOTAL_DE_LUAEs = n(), .groups = 'drop')
      
      # Si no hay datos para el año seleccionado, agregarlo con total 0
      all_years <- unique(db_luae$anio_otorgamiento)
      report <- data.frame(anio_otorgamiento = all_years, TOTAL_DE_LUAEs = 0) %>%
        right_join(report, by = "anio_otorgamiento") %>%
        replace_na(list(TOTAL_DE_LUAEs = 0))
      
    } else if (input$report_frequency == "ACUMULADO MENSUAL") {
      filtered_data <- db_luae %>%
        filter(if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE,
               if (input$report_month != "TODOS") lubridate::month(fecha_otorgamiento) == as.numeric(input$report_month) else TRUE)
      
      report <- filtered_data %>%
        group_by(anio_otorgamiento, lubridate::month(fecha_otorgamiento)) %>%
        summarise(TOTAL_DE_LUAEs = n(), .groups = 'drop')
    }
    
    return(report)
  })
  
  output$pivot_table <- renderDT({
    datatable(report_data(), options = list(pageLength = 15, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  # Descargar reporte en Excel
  output$download_report <- downloadHandler(
    filename = function() {
      paste("reporte_luae_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(report_data(), file)
    }
  )
  
  # Función reactiva para ejecutar la consulta de cubo dinámico
  pivot_data <- eventReactive(input$generate_report, {
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
  
  # Función reactiva para filtrar la tabla base
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

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
