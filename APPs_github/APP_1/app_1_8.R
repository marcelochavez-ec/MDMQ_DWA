library(DT)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(highcharter)
library(shinyjs)
library(RPostgreSQL)

# Configuraciones de Shiny
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Cargar datos de ejemplo (reemplazar con tus propios datos)
con_postgres <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost",
                          port = 5432,
                          user = "postgres",
                          password = "marce")

# Cargar datos desde la base de datos (ajustar la consulta según tu estructura de datos)
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

# Crear la aplicaciÃ³n Shiny
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
          min-width: 300px; /* Ancho mÃ­nimo para evitar colapsar en pantallas pequeÃ±as */
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 10px;
          margin-bottom: 20px; /* Espaciado inferior entre los elementos */
        }
        .action-button-custom {
          background-color: #176087;
          color: white;
          width: 100%;
          height: 50px;
          font-size: 16px;
          margin-bottom: 10px; /* Espaciado inferior entre los botones */
          display: flex;
          align-items: center;
          justify-content: center;
          transition: background-color 0.3s ease; /* TransiciÃ³n suave para el color de fondo */
        }
        
        .action-button-custom:hover {
          background-color: #004e98; /* Color de fondo al pasar el mouse */
        }
        
        .sidebar-panel-custom {
          background-color: #ade8f4; /* Color de fondo del sidebarPanel */
          color: black; /* Color del texto */
          padding: 10px; /* Espaciado interno */
          border-radius: 5px; /* Borde redondeado */
          margin-bottom: 10px; /* Margen inferior */
        }
        .footer {
          background-color: #2C4770;
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
          margin-left: 20px; /* Espacio izquierda para viÃ±etas */
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
                              choices = c("ANUAL", "MENSUAL", "ACUMULADO"), selected = "ANUAL"),
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
             style = "margin-right: 5px; font-size: 20px;"),  # Aumentar el tamaÃ±o del icono
        HTML("<b>Notas Técnicas:</b>"),
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
  
  # FunciÃ³n reactiva para actualizar los filtros condicionales del reporte
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
  
  # FunciÃ³n reactiva para generar los datos del reporte
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
  
  # FunciÃ³n reactiva para generar los datos del reporte en formato vertical
  formatted_report_data <- reactive({
    report_data() %>%
      pivot_longer(cols = starts_with("total_licencias"),
                   names_to = "dimensione_a_mostrar",
                   values_to = "total_licencias") %>%
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
          highchartOutput("highchart")  # GrÃ¡fico de highcharter
        )
      )
    )
  })
  
  # Renderizar la tabla con los datos del reporte
  output$pivot_table <- renderDT({
    datatable(formatted_report_data(), 
              options = list(pageLength = 15, 
                             scrollX = TRUE, 
                             scrollY = TRUE), 
              rownames = FALSE)
  })
  
  # Renderizar el grÃ¡fico de highcharter
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
  
  # FunciÃ³n reactiva para filtrar la tabla base
  filtered_base_data <- eventReactive(input$base_filter_button, {
    # Convertir el input base_num_licencia en un vector
    base_num_licencia_vec <- unlist(strsplit(input$base_num_licencia, ","))
    base_num_licencia_vec <- trimws(base_num_licencia_vec)  # Eliminar espacios en blanco adicionales
    
    db_luae %>%
      filter(if (length(base_num_licencia_vec) == 0) TRUE else numero_tramite %in% base_num_licencia_vec)
  })
  
  output$base_table <- renderDT({
    datatable(filtered_base_data(),
              options = list(pageLength = 10, 
                             scrollX = TRUE, 
                             scrollY = TRUE), 
              rownames = FALSE)
  })
  
  # Descargar reporte en Excel
  output$download_report <- downloadHandler(
    filename = function() {
      paste("reporte_luae_", 
            Sys.Date(),
            ".xlsx", 
            sep = "")
    },
    content = function(file) {
      write.xlsx(formatted_report_data(), file)
    }
  )
}

# Ejecutar la aplicaciÃ³n Shiny
shinyApp(ui = ui, server = server)