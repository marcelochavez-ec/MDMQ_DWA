library(shiny)
library(DT)
library(tidyverse)
library(openxlsx)
library(RPostgreSQL)
library(lubridate)
library(highcharter)
library(shinyjs)

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

con_postgres <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost", 
                          port = 5432,
                          user = "postgres", 
                          password = "marce")

db_luae <- dbGetQuery(con_postgres, 
                      "SELECT anio_otorgamiento,
                numero_tramite,
                estado_emision,
                estado_bpm,
                fecha_otorgamiento,
                fecha_inicio, 
                fecha_fin, 
                fecha_vencimiento,
                zona_predio, 
                administracion_zonal, 
                actividad_economica 
         FROM
         c_economico.db_luae")

db_luae <- db_luae %>% 
  distinct()

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
            .sidebar .btn {
                width: 100%;
                margin-bottom: 10px;
                display: flex;
                justify-content: center;
                align-items: center;
            }
            .sidebar .form-group {
                text-align: left;
            }
            .sidebar .form-control {
                margin-bottom: 10px;
            }
            .sidebar label, .sidebar .shiny-input-container {
                text-align: left;
            }
            .sidebar .shiny-options-group {
                text-align: left;
            }
            .sidebar .center-label {
                text-align: center;
            }
        "))
  ),
  titlePanel("CUBO - LUAEs"),
  tabsetPanel(
    tabPanel("CUBO",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 class = "sidebar",
                 selectInput("row_vars", "Filas:", choices = names(db_luae), multiple = TRUE),
                 selectInput("col_vars", "Columnas:", choices = names(db_luae), multiple = TRUE),
                 selectInput("value_var", "Operaciones:", choices = c("COUNT" = "count", "SUM" = "sum")),
                 selectInput("target_var", "Variable de Operación:", choices = names(db_luae)),
                 selectInput("estado_emision", "Estado de Emisión:", choices = c("TODOS", unique(db_luae$estado_emision)), multiple = TRUE, selected = "TODOS"),
                 selectInput("movimiento_actual", "Movimiento Actual:", choices = c("TODOS", unique(db_luae$movimiento_actual)), multiple = TRUE, selected = "TODOS"),
                 selectInput("estado_bpm", "Estado BPM:", choices = c("TODOS", unique(db_luae$estado_bpm)), multiple = TRUE, selected = "TODOS"),
                 textInput("num_licencia", "Número de Licencia:", placeholder = 'Ingrese uno o más números de licencia separados por comas'),
                 actionButton("run_button", "Ejecutar consulta", icon = icon("play"))
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
                 class = "sidebar",
                 textInput("base_num_licencia", "Número de Licencia:", placeholder = 'Ingrese uno o más números de licencia separados por comas'),
                 actionButton("base_filter_button", "Filtrar", icon = icon("filter"))
               ),
               mainPanel(
                 width = 9,
                 DTOutput("base_table")
               )
             )),
    tabPanel("REPORTE",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 class = "sidebar",
                 radioButtons("report_frequency", "Frecuencia del Reporte:", choices = c("ANUAL", "ACUMULADO MENSUAL"), selected = c("ANUAL")),
                 uiOutput("conditional_filters"),
                 actionButton("generate_report", "Generar Reporte", icon = icon("file-alt")),
                 br(),
                 downloadButton("download_report", "Descargar Reporte en Excel", icon = icon("download"))
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(6,
                          DTOutput("report_table")
                   ),
                   column(6,
                          highchartOutput(outputId = "chart_licencias", 
                                          height = 500)
                   )
                 )
               )
             ))
  )
)

server <- function(input, output) {
  # Función reactiva para actualizar los filtros condicionales
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
      report <- db_luae %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO")) %>%
        select(numero_tramite,
               anio_otorgamiento) %>% 
        distinct() %>% 
        group_by(anio_otorgamiento) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    } else if (input$report_frequency == "ACUMULADO MENSUAL") {
      filtered_data <- db_luae %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "
FINALIZADO") &
                 (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE) &
                 (if (input$report_month != "TODOS") {
                   if (as.numeric(input$report_month) == 12) lubridate::month(fecha_otorgamiento) <= 12 else lubridate::month(fecha_otorgamiento) <= as.numeric(input$report_month)
                 } else TRUE))
      
      report <- filtered_data %>%
        select(numero_tramite,
               anio_otorgamiento) %>% 
        distinct() %>%
        group_by(anio_otorgamiento) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    }
    
    return(report)
  })

    # Función reactiva para generar los datos del reporte
  report_data_drilldown <- eventReactive(input$generate_report, {
    if (input$report_frequency == "ANUAL") {
      report <- db_luae %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO")) %>%
        select(numero_tramite,
               anio_otorgamiento,
               fecha_otorgamiento) %>% 
        distinct() %>% 
        group_by(anio_otorgamiento,
                 ) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    } else if (input$report_frequency == "ACUMULADO MENSUAL") {
      filtered_data <- db_luae %>%
        filter(estado_emision == "OTORGADA" &
                 (estado_bpm == "EJECUCION" | estado_bpm == "
FINALIZADO") &
                 (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE) &
                 (if (input$report_month != "TODOS") {
                   if (as.numeric(input$report_month) == 12) lubridate::month(fecha_otorgamiento) <= 12 else lubridate::month(fecha_otorgamiento) <= as.numeric(input$report_month)
                 } else TRUE))
      
      report <- filtered_data %>%
        select(numero_tramite,
               anio_otorgamiento) %>% 
        distinct() %>%
        group_by(anio_otorgamiento) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    }
    
    return(report)
  })
  
  output$report_table <- renderDT({
    datatable(report_data(), 
              options = list(pageLength = 10, 
                             scrollX = TRUE,
                             scrollY = TRUE), 
              rownames = FALSE,
              colnames = c("AÑO","TOTAL LICENCIAS"))
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("reporte_luae_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(report_data(), file)
    }
  )
  
  output$chart_licencias <- renderHighchart({
    data <- report_data()
    if (input$report_frequency == "ANUAL") {
      data <- data[-nrow(data), ]  # Eliminar el último año
    }
    
    hchart(data, 
           "column",
           hcaes(x = anio_otorgamiento, 
                 y = total_licencias), 
           color = "#0198f9", 
           name = "Número de LUAEs otorgadas en el DMQ") |>
      hc_title(text = "Serie histórica: 2018-2023",
               align = "left") |>
      hc_xAxis(
        title = list(text = "AÑOS"),
        categories = data$anio_otorgamiento,
        labels = list(rotation = -90)  # Rotar las etiquetas 90 grados
      ) |>
      hc_yAxis(title = list(text = "TOTAL DE LICENCIAS OTORGADAS"))
  })
}

shinyApp(ui = ui, server = server)
