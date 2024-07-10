library(shiny)
library(DT)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(highcharter)
library(RPostgreSQL)

# Configuraciones de Shiny
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

postgres_conexion <- dbConnect(RPostgres::Postgres(),
                               dbname = "sidep",
                               host = "localhost", 
                               port = 5432,
                               user = "postgres", 
                               password = "marce")

# Cargar datos de ejemplo (reemplazar con tus propios datos)
# db_luae <- readRDS("db_luae.RDS")

db_luae <- dbGetQuery(postgres_conexion, "SELECT * FROM c_economico.db_luae")

# Crear la aplicación Shiny
ui <- fluidPage(
  titlePanel("REPORTEADOR"),
  tags$head(
    tags$style(
      HTML("
        .report-container {
          display: flex;
          flex-wrap: wrap;
          justify-content: space-between;
          margin-bottom: 20px;
        }
        .report-item {
          flex: 1;
          min-width: 300px; /* Ancho mínimo para evitar colapsar en pantallas pequeñas */
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 10px;
          margin-bottom: 20px; /* Espaciado inferior entre los elementos */
        }
        .action-button-custom {
          background-color: #1F2F4C;
          color: white;
          width: 100%;
          height: 50px;
          font-size: 16px;
          margin-bottom: 10px; /* Espaciado inferior entre los botones */
          display: flex;
          align-items: center;
          justify-content: center;
          transition: background-color 0.3s ease; /* Transición suave para el color de fondo */
        }
        .action-button-custom:hover {
          background-color: #FFFFFF; /* Color de fondo al pasar el mouse */
        }
        .sidebar-panel-custom {
          background-color: #e5e5e5; /* Color de fondo del sidebarPanel */
          color: black; /* Color del texto */
          padding: 10px; /* Espaciado interno */
          border-radius: 5px; /* Borde redondeado */
          margin-bottom: 10px; /* Margen inferior */
        }
        .footer {
          background-color: #1F2F4C;
          color:white;
          padding: 10px;
          text-align: center;
          margin-top: 20px;
        }
        .footer-text {
          font-size: 12px;
          color: white;
          margin-bottom: 5px; /* Espacio entre los footnotes */
          text-align: left; /* Alinear texto a la izquierda */
          margin-left: 20px; /* Espacio izquierda para viñetas */
        }
      ")
    )
  ),
  tabsetPanel(
    tabPanel("LUAE",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 class = "sidebar-panel-custom",  # Aplicar estilo personalizado al sidebarPanel
                 selectInput("dimensions", "Total de LUAEs por:", 
                             choices = c("AÑO DE OTORGAMIENTO" = "anio_otorgamiento",
                                         "ADMINISTRACIÓN ZONAL" = "administracion_zonal",
                                         "ESTADO DE EMISIÓN" = "movimiento_actual",
                                         "ACTIVIDAD ECONÓMICA" = "actividad_economica"), 
                             multiple = TRUE),
                 radioButtons("report_frequency", "Frecuencia del Reporte:", 
                              choices = c("ANUAL",
                                          "MENSUAL",
                                          "ACUMULADO"),
                              selected = "ANUAL"),
                 uiOutput("conditional_filters"),
                 br(),
                 fluidRow(
                   div(
                     class = "action-button-wrapper",
                     column(6, 
                            actionButton("run_button",
                                         "| Reporte", 
                                         icon = icon("file-alt"), 
                                         class = "custom-button action-button-custom")
                     ),
                     column(6,
                            downloadButton("download_report",
                                           "| Descarga",
                                           icon = icon("download"), 
                                           class = "custom-button action-button-custom")
                     )
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
                 textInput("base_num_licencia",
                           "Número de Licencia:", 
                           placeholder = 'Ingrese uno o más números de licencia separados por comas'),
                 actionButton("base_filter_button", 
                              "Filtrar")
               ),
               mainPanel(
                 width = 9,
                 DTOutput("base_table")
               )
             ))
  ),
  # Footer con dos secciones separadas
  tags$div(
    class = "footer",
    div(
      class = "footer-content",
      div(
        class = "footer-text",
        icon("info-circle", 
             class = "fas", 
             style = "margin-right: 6px; font-size: 21px;"),  # Aumentar el tamaño del icono
        HTML("<b>Importante:</b>"),
        br(),
        HTML("<b>Fuente:</b> Sistema BPM-MDMQ"),
        br(),
        HTML("<b>Fecha de corte:</b> 30 de junio de 2024"),
        br(),
        HTML("<b>Elaborado por:</b> Secretaría de Desarrollo Económico y Productivo")
      )
    )
  ),
  tags$div(
    class = "footer",
    HTML("<b>© Copyright - SIDEPRO 2024</b>")
  )
)

server <- function(input, output, session) {
  
  # Función reactiva para actualizar los filtros condicionales del reporte
  output$conditional_filters <- renderUI({
    if (input$report_frequency == "ANUAL") {
      return(NULL)
    } else if (input$report_frequency == "ACUMULADO") {
      fluidRow(
        column(6,
               selectInput("report_year", 
                           "Año:",
                           choices = c("TODOS",
                                       unique(db_luae$anio_otorgamiento)),
                           selected = "TODOS")),
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
    } else if (input$report_frequency == "MENSUAL") {
      fluidRow(
        column(6,
               checkboxGroupInput("report_year", "Año:", 
                                  choices = c("TODOS",
                                              unique(db_luae$anio_otorgamiento)), 
                                  selected = "TODOS")),
        column(6,
               checkboxGroupInput("report_month", "Mes:", 
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
  report_data <- eventReactive(input$run_button, {
    req(input$dimensions)
    
    # Filtrar los datos con base en las selecciones de las dimensiones
    filtered_data <- db_luae
    
    if (input$report_frequency == "ANUAL") {
      report <- filtered_data %>%
        filter(estado_emision %in% c("EMITIDA",
                                     "EMITIDA PARCIAL",
                                     "FINALIZADA",
                                     "OTORGADA",
                                     "OTORGADA PARCIAL") &
                 (!estado_bpm %in% c("ABORTADO",
                                     "FINALIZADO CON ERROR"))) %>%
        select(numero_tramite, !!!syms(input$dimensions)) %>%
        distinct() %>%
        group_by(!!!syms(input$dimensions)) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    } 
    
    else if (input$report_frequency == "MENSUAL") {
      filtered_data <- filtered_data %>%
        filter(estado_emision %in% c("EMITIDA",
                                     "EMITIDA PARCIAL",
                                     "FINALIZADA",
                                     "OTORGADA",
                                     "OTORGADA PARCIAL") &
                 (!estado_bpm %in% c("ABORTADO",
                                     "FINALIZADO CON ERROR")) &
                 (if (!("TODOS" %in% input$report_year)) 
                   lubridate::year(fecha_otorgamiento) %in% as.numeric(input$report_year) else TRUE) &
                 (if (!("TODOS" %in% input$report_month)) 
                   lubridate::month(fecha_otorgamiento) %in% as.numeric(input$report_month) else TRUE))
      
      report <- filtered_data %>%
        mutate(mes_otorgamiento = month(fecha_otorgamiento, label = TRUE, abbr = FALSE)) %>%
        select(numero_tramite, !!!syms(input$dimensions), mes_otorgamiento) %>%
        distinct() %>%
        group_by(!!!syms(input$dimensions), mes_otorgamiento) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    }
    
    else if (input$report_frequency == "ACUMULADO") {
      report <- filtered_data %>%
        filter(estado_emision %in% c("EMITIDA",
                                     "EMITIDA PARCIAL",
                                     "FINALIZADA",
                                     "OTORGADA",
                                     "OTORGADA PARCIAL") &
                 (!estado_bpm %in% c("ABORTADO",
                                     "FINALIZADO CON ERROR")) &
                 (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE) &
                 (if (input$report_month != "TODOS") lubridate::month(fecha_otorgamiento) <= as.numeric(input$report_month) else TRUE)) %>%
        select(numero_tramite, anio_otorgamiento) %>%
        distinct() %>%
        group_by(anio_otorgamiento) %>%
        summarise(total_licencias = n(), .groups = 'drop')
    }
    
    return(report)
  })
  
  # Renderizar el reporte en una tabla de datos
  output$report_output <- renderUI({
    req(report_data())
    fluidRow(
      column(12,
             div(
               class = "report-container",
               div(
                 class = "report-item",
                 DTOutput("table")  # Espacio para la tabla
               ),
               div(
                 class = "report-item",
                 highchartOutput("chart")  # Espacio para el gráfico
               )
             )
      )
    )
  })
  
  # Generar el gráfico usando Highcharter
  output$chart <- renderHighchart({
    req(report_data())
    
    if (input$report_frequency == "ANUAL") {
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Histórico de Licencias Únicas de Actividades Económicas") %>%
        hc_xAxis(categories = report_data()[[input$dimensions[1]]],
                 title = list(text = "Fecha (Año|Mes)")) %>%
        hc_yAxis(
          title = list(text = ""),  # Quitar el label del eje Y
          opposite = TRUE  # Colocar el eje Y en el lado derecho
        ) %>%
        hc_legend(layout = "horizontal", align = "left", verticalAlign = "top") %>%  # Colocar la leyenda en la parte superior izquierda
        hc_series(
          list(
            name = "Total de LUAEs",
            data = report_data()$total_licencias
          )
        )
    } else if (input$report_frequency == "MENSUAL") {
      # Agrupar los datos por año y mes
      monthly_data <- report_data() %>%
        mutate(anio_mes = paste(anio_otorgamiento, mes_otorgamiento, sep = "-")) %>%
        group_by(anio_mes) %>%
        summarise(total_licencias = sum(total_licencias), .groups = 'drop')
      
      # Preparar el gráfico
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Histórico de Licencias Únicas de Actividades Económicas (Mensual)") %>%
        hc_xAxis(
          categories = monthly_data$anio_mes,
          title = list(text = "Fecha (Año|Mes)")  # Cambiar el label del eje X
        ) %>%
        hc_yAxis(
          title = list(text = ""),  # Quitar el label del eje Y
          opposite = TRUE  # Colocar el eje Y en el lado derecho
        ) %>%
        hc_legend(layout = "horizontal", align = "left", verticalAlign = "top") %>%  # Colocar la leyenda en la parte superior izquierda
        hc_series(
          list(
            name = "Total de LUAEs",
            data = monthly_data$total_licencias
          )
        )
    } else if (input$report_frequency == "ACUMULADO") {
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Histórico de Licencias Únicas de Actividades Económicas (Acumulado)") %>%
        hc_xAxis(categories = report_data()[[input$dimensions[1]]],
                 title = list(text = "Fecha (Año|Mes)")) %>%
        hc_yAxis(
          title = list(text = ""),  # Quitar el label del eje Y
          opposite = TRUE  # Colocar el eje Y en el lado derecho
        ) %>%
        hc_legend(layout = "horizontal", align = "left", verticalAlign = "top") %>%  # Colocar la leyenda en la parte superior izquierda
        hc_series(
          list(
            name = "Total de LUAEs",
            data = report_data()$total_licencias
          )
        )
    }
  })
  
  # Renderizar la tabla de datos
  output$table <- renderDT({
    req(report_data())
    datatable(report_data(), options = list(pageLength = 10))
  })
  
  # Descargar el reporte en Excel
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Reporte_LUAEs_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(report_data(), file)
    }
  )
  
  # Filtrar la tabla de BASE según el número de licencia
  filtered_base_data <- eventReactive(input$base_filter_button, {
    req(input$base_num_licencia)
    licencia_list <- strsplit(input$base_num_licencia, ",")[[1]] %>%
      trimws()
    db_luae %>%
      filter(numero_licencia %in% licencia_list)
  })
  
  output$base_table <- renderDT({
    req(filtered_base_data())
    datatable(filtered_base_data(), options = list(pageLength = 10))
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
