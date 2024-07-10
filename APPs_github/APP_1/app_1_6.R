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

# Conectar a la base de datos (ajustar según tu configuración)
# con_postgres <- dbConnect(RPostgres::Postgres(),
#                           dbname = "sidep",
#                           host = "localhost", 
#                           port = 5432,
#                           user = "postgres", 
#                           password = "marce")

# Cargar datos desde la base de datos (ajustar la consulta según tu estructura de datos)
# db_luae <- dbGetQuery(con_postgres, 
#                       "SELECT anio_otorgamiento,
#                               numero_tramite,
#                               estado_bpm,
#                               estado_emision,
#                               fecha_otorgamiento,
#                               fecha_vencimiento,
#                               administracion_zonal, 
#                               actividad_economica,
#                               movimiento_actual
#                        FROM c_economico.db_luae")
# 
# saveRDS(db_luae,file="DB_onedrive/LUAE/db_luae.RDS")

# rm(list = ls(all.names = T))

db_luae <- readRDS("db_luae.RDS")

# Crear la aplicación Shiny
ui <- fluidPage(
  titlePanel("REPORTEADOR"),
  tags$head(
    tags$style(
      HTML("
        .report-container {
          display: flex;
          justify-content: space-between;
          margin-bottom: 20px;
        }
        .report-item {
          flex: 1;
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 10px;
        }
      ")
    )
  ),
  tabsetPanel(
    tabPanel("LUAE",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("dimensions", "Total de LUAEs por:", 
                             choices = c("AÑO DE OTORGAMIENTO" = "anio_otorgamiento",
                                         "ADMINISTRACIÓN ZONAL" = "administracion_zonal",
                                         "ESTADO DE EMISIÓN" = "movimiento_actual",
                                         "ACTIVIDAD ECONÓMICA" = "actividad_economica"), 
                             multiple = TRUE),
                 radioButtons("report_frequency", "Frecuencia del Reporte:", 
                              choices = c("ANUAL", "MENSUAL", "ACUMULADO"), selected = "ANUAL"),
                 uiOutput("conditional_filters"),
                 br(),
                 fluidRow(
                   column(6, 
                          actionButton("run_button",
                                       "Reporte", 
                                       icon = icon("file-alt"), 
                                       width = "100%")
                   ),
                   column(6,
                          downloadButton("download_report",
                                         "Descarga",
                                         icon = icon("download"), 
                                         width = "100%")
                   )
                 )
               ),
               mainPanel(
                 width = 9,  # Usamos toda la anchura disponible
                 uiOutput("report_output")  # Espacio para el resultado del reporte
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
      fluidRow(
        column(6,
               selectInput("report_year", "Año:", choices = c("TODOS", unique(db_luae$anio_otorgamiento)), selected = "TODOS")),
        column(6,
               selectInput("report_month", "Mes:", 
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
    } else if (input$report_frequency == "MENSUAL") {
      fluidRow(
        column(6,
               checkboxGroupInput("report_year", "Año:", 
                                  choices = c("TODOS", unique(db_luae$anio_otorgamiento)), 
                                  selected = "TODOS")),
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
                 (if (!is.null(input$report_year) && !("TODOS" %in% input$report_year)) lubridate::year(fecha_otorgamiento) %in% as.numeric(input$report_year) else TRUE) &
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
  
  # Función reactiva para generar los datos del reporte en formato vertical
  formatted_report_data <- reactive({
    report_data() %>%
      pivot_longer(cols = starts_with("total_licencias"), names_to = "dimensione_a_mostrar", values_to = "total_licencias") %>%
      select(-dimensione_a_mostrar)  # Quitar columna dimensione_a_mostrar
  })
  
  # Renderizar la salida del reporte
  output$report_output <- renderUI({
    req(input$run_button)
    
    fluidRow(
      div(
        class = "report-container",
        div(
          class = "report-item",
          DTOutput("pivot_table")  # Tabla de reporte
        ),
        div(
          class = "report-item",
          highchartOutput("highchart")  # Gráfico de highcharter
        )
      )
    )
  })
  
  # Renderizar la tabla con los datos del reporte
  output$pivot_table <- renderDT({
    datatable(formatted_report_data(), options = list(pageLength = 15, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
  
  # Renderizar el gráfico de highcharter
  output$highchart <- renderHighchart({
    req(formatted_report_data())
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Total de LUAEs por Categoría") %>%
      hc_xAxis(categories = formatted_report_data()$dimensione_a_mostrar) %>%
      hc_yAxis(title = list(text = "Total de LUAEs")) %>%
      hc_series(list(
        name = "Total LUAEs",
        data = formatted_report_data()$total_licencias
      ))
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
