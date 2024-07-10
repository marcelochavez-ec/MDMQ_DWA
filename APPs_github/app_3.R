# Librerías y configuraciones
library(shiny) # Para el desarrollo de aplicaciones web con R
library(DT) # Para visualización de tablas en formato Web
library(tidyverse) # Para procesos de tipo ETL
library(openxlsx) # Para generar el reporte en Excel
library(RPostgreSQL) # Para conexión a la BDD
library(lubridate) # Para manejar fechas

# Especificar el puerto de la aplicación
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Cargar el dataframe
con_postgres <- dbConnect(RPostgres::Postgres(),
                          dbname = "sidep",
                          host = "localhost", 
                          port = 5432,
                          user = "postgres", 
                          password = "marce")

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
                actividad_economica 
         FROM
         c_economico.db_luae")

# Filtrar y seleccionar las columnas necesarias
db_luae <- db_luae %>% 
    distinct()

# Crear la aplicación Shiny
ui <- fluidPage(
    titlePanel("CUBO - LUAEs"),
    tabsetPanel(
        tabPanel("CUBO",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,  # Ajusta el ancho del sidebar
                         selectInput("row_vars", "Filas:", choices = names(db_luae), multiple = TRUE),
                         selectInput("col_vars", "Columnas:", choices = names(db_luae), multiple = TRUE),
                         selectInput("value_var", "Operaciones:", choices = c("COUNT" = "count", "SUM" = "sum")),
                         selectInput("target_var", "Variable de Operación:", choices = names(db_luae)),
                         selectInput("estado_emision", "Estado de Emisión:", choices = c("TODOS", unique(db_luae$estado_emision)), multiple = TRUE, selected = "TODOS"),
                         selectInput("movimiento_actual", "Movimiento Actual:", choices = c("TODOS", unique(db_luae$movimiento_actual)), multiple = TRUE, selected = "TODOS"),
                         selectInput("estado_bpm", "Estado BPM:", choices = c("TODOS", unique(db_luae$estado_bpm)), multiple = TRUE, selected = "TODOS"),
                         textInput("num_licencia", "Número de Licencia:", placeholder = 'Ingrese uno o más números de licencia separados por comas'),
                         actionButton("run_button", "Ejecutar consulta")
                     ),
                     mainPanel(
                         width = 9,  # Ajusta el ancho del main panel
                         DTOutput("pivot_table")
                     )
                 )),
        tabPanel("BASE",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,  # Ajusta el ancho del sidebar
                         textInput("base_num_licencia", "Número de Licencia:", placeholder = 'Ingrese uno o más números de licencia separados por comas'),
                         actionButton("base_filter_button", "Filtrar")
                     ),
                     mainPanel(
                         width = 9,  # Ajusta el ancho del main panel
                         DTOutput("base_table")
                     )
                 )),
        tabPanel("REPORTE",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,  # Ajusta el ancho del sidebar
                         radioButtons("report_frequency", "Frecuencia del Reporte:", choices = c("ANUAL", "ACUMULADA MENSUAL"), selected = c("ANUAL")),
                         uiOutput("conditional_filters"),
                         actionButton("generate_report", "Generar Reporte"),
                         downloadButton("download_report", "Descargar Reporte en Excel")
                     ),
                     mainPanel(
                         width = 9,  # Ajusta el ancho del main panel
                         DTOutput("report_table")
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
                       selectInput("report_month", "Mes:", choices = c("TODOS", 1:12), selected = "TODOS"))
            )
        }
    })
    
    # Función reactiva para generar los datos del reporte
    report_data <- eventReactive(input$generate_report, {
        if (input$report_frequency == "ANUAL") {
            report <- db_luae %>%
                filter(estado_emision == "OTORGADA" &
                           (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO")) %>%
                select(numero_licencia,
                       anio_otorgamiento) %>% 
                distinct() %>% 
                group_by(anio_otorgamiento) %>%
                summarise(total_licencias = n(), .groups = 'drop')
        } else if (input$report_frequency == "ACUMULADA MENSUAL") {
            filtered_data <- db_luae %>%
                filter(estado_emision == "OTORGADA" &
                           (estado_bpm == "EJECUCION" | estado_bpm == "FINALIZADO") &
                           (if (input$report_year != "TODOS") lubridate::year(fecha_otorgamiento) == as.numeric(input$report_year) else TRUE) &
                           (if (input$report_month != "TODOS") {
                               if (as.numeric(input$report_month) == 12) lubridate::month(fecha_otorgamiento) <= 12 else lubridate::month(fecha_otorgamiento) <= as.numeric(input$report_month)
                           } else TRUE))
            
            report <- filtered_data %>%
                select(numero_licencia,
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
}

shinyApp(ui = ui, server = server)
