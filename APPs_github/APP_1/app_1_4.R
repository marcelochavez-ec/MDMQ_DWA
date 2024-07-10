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
                              numero_tramite,
                              estado_bpm,
                              estado_emision,
                              fecha_otorgamiento,
                              fecha_vencimiento,
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
                             choices = c("AÑO DE OTORGAMIENTO" = "anio_otorgamiento",
                                         "ADMINISTRACIÓN ZONAL" = "administracion_zonal",
                                         "ESTADO DE EMISIÓN" = "movimiento_actual",
                                         "ACTIVIDAD ECONÓMICA" = "actividad_economica"), 
                             multiple = TRUE),
                 checkboxGroupInput("measures", "Medidas:", 
                                    choices = "TOTAL DE LUAEs", 
                                    selected = "TOTAL DE LUAEs"),
                 actionButton("run_button", "Generar Reporte", icon = icon("file-alt")),
                 br(),
                 radioButtons("report_frequency", "Frecuencia del Reporte:", 
                              choices = c("ANUAL", "ACUMULADO", "MENSUAL"), selected = "ANUAL"),
                 uiOutput("conditional_filters"),
                 br(),
                 radioButtons("pivot_format", "Formato del Reporte:", 
                              choices = c("Vertical (pivot_longer)" = "pivot_longer", 
                                          "Horizontal (pivot_wider)"), selected = "pivot_longer"),
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
    } else if (input$report_frequency == "ACUMULADO") {
      filtered_data <- filtered_data %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO") &
                 (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE) &
                 (if (input$report_month != "TODOS") {
                   if (as.numeric(input$report_month) == 12) lubridate::month(fecha_otorgamiento) <= 12 else lubridate::month(fecha_otorgamiento) <= as.numeric(input$report_month)
                 } else TRUE))
      
      report <- filtered_data %>%
        select(numero_tramite, !!!syms(input$dimensions)) %>%
        distinct() %>%
        group_by(!!!syms(input$dimensions)) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    } else if (input$report_frequency == "MENSUAL") {
      fluidRow(
        column(6,
               selectInput("report_year", "Año:", choices = c("TODOS", unique(db_luae$anio_otorgamiento)), selected = "TODOS")),
        column(6,
               checkboxGroupInput("report_month", "Mes:", 
                                  choices = c("Enero" = 1, 
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
  report_data <- eventReactive(input$run_button, {
    req(input$dimensions)
    
    # Filtrar los datos con base en las selecciones de las dimensiones
    filtered_data <- db_luae
    
    if (input$report_frequency == "ANUAL") {
      report <- filtered_data %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO")) %>%
        select(numero_tramite, !!!syms(input$dimensions)) %>%
        distinct() %>%
        group_by(!!!syms(input$dimensions)) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    } else if (input$report_frequency == "ACUMULADO") {
      filtered_data <- filtered_data %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO") &
                 (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE))
      
      report <- filtered_data %>%
        select(numero_tramite, !!!syms(input$dimensions)) %>%
        distinct() %>%
        group_by(!!!syms(input$dimensions)) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    } else if (input$report_frequency == "MENSUAL") {
      filtered_data <- filtered_data %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO") &
                 (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) %in% as.numeric(input$report_year) else TRUE) &
                 (if (!is.null(input$report_month) && !("TODOS" %in% input$report_month)) lubridate::month(fecha_otorgamiento) %in% as.numeric(input$report_month) else TRUE))
      
      report <- filtered_data %>%
        mutate(mes_otorgamiento = month(fecha_otorgamiento, label = TRUE, abbr = FALSE)) %>%
        select(numero_tramite, !!!syms(input$dimensions), mes_otorgamiento) %>%
        distinct() %>%
        group_by(!!!syms(input$dimensions), mes_otorgamiento) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    }
    
    return(report)
  })
  
  # Función reactiva para generar los datos del reporte en el formato elegido
  formatted_report_data <- reactive({
    req(input$pivot_format)
    
    if (input$pivot_format == "pivot_longer") {
      # Formato vertical (pivot_longer)
      report_long <- report_data() %>%
        pivot_longer(cols = total_licencias, names_to = "Medida", values_to = "Valor") %>%
        select(-Medida)  # Quitar columna Medida
      
      return(report_long)
    } else if (input$pivot_format == "pivot_wider") {
      # Formato horizontal (pivot_wider)
      report_wide <- report_data() %>%
        pivot_wider(names_from = all_of(input$dimensions), values_from = total_licencias, values_fill = list(total_licencias = 0))
      
      return(report_wide)
    }
  })
  
  output$pivot_table <- renderDT({
    datatable(formatted_report_data(), options = list(pageLength = 15, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  # Función reactiva para filtrar la tabla base
  filtered_base_data <- eventReactive(input$base_filter_button, {
    # Convertir el input base_num_licencia en un vector
    base_num_licencia_vec <- unlist(strsplit(input$base_num_licencia, ","))
    base_num_licencia_vec <- trimws(base_num_licencia_vec)  # Eliminar espacios en blanco adicionales
    
    db_luae %>%
      filter(if (length(base_num_licencia_vec) == 0) TRUE else numero_tramite %in% base_num_licencia_vec)
  })
  
  output$base_table <- renderDT({
    datatable(filtered_base_data(), options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  # Descargar reporte en Excel
  output$download_report <- downloadHandler(
    filename = function() {
      paste("reporte_luae_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(formatted_report_data(), file)
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
