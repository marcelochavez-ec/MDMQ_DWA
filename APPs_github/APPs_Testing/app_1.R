# Librerías y configuraciones
library(shiny) # Para el desarrollo de aplicaciones web con R
library(DT) # Para visualización de tablas en formato Web
library(tidyverse) # Para procesos de tipo ETL
library(openxlsx) # Para generar el reporte en Excel
library(RPostgreSQL) # Para conexión a la BDD

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

db_luae <- datos <- dbGetQuery(con_postgres, 
        "SELECT numero_licencia,
                estado_emision, 
                fecha_otorgamiento,
                anio_otorgamiento,
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
                 radioButtons("report_type", "Tipo de Reporte:", choices = c("ANUAL", "CATEGORIZADO")),
                 selectInput("reporte_zona", "Zona del Predio:", choices = c("TODOS", unique(db_luae$zona_predio)), multiple = TRUE, selected = "TODOS"),
                 selectInput("reporte_admin_zonal", "Administración Zonal:", choices = c("TODOS", unique(db_luae$administracion_zonal)), multiple = TRUE, selected = "TODOS"),
                 selectInput("reporte_actividad", "Actividad Económica:", choices = c("TODOS", unique(db_luae$actividad_economica)), multiple = TRUE, selected = "TODOS"),
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
  pivot_data <- eventReactive(input$run_button, {
    if (is.null(input$row_vars) || is.null(input$col_vars) || is.null(input$value_var) || is.null(input$target_var)) {
      return(NULL)
    }
    
    # Convertir el input num_licencia en un vector
    num_licencia_vec <- unlist(strsplit(input$num_licencia, ","))
    num_licencia_vec <- trimws(num_licencia_vec)  # Eliminar espacios en blanco adicionales
    
    filtered_data <- db_luae %>%
      filter(if ("TODOS" %in% input$estado_emision) TRUE else estado_emision %in% input$estado_emision,
             if ("TODOS" %in% input$movimiento_actual) TRUE else movimiento_actual %in% input$movimiento_actual,
             if ("TODOS" %in% input$estado_bpm) TRUE else estado_bpm %in% input$estado_bpm,
             if (length(num_licencia_vec) == 0) TRUE else numero_licencia %in% num_licencia_vec)
    
    data <- filtered_data %>%
      group_by(across(all_of(c(input$row_vars, input$col_vars)))) %>%
      summarise(Operaciones = if (input$value_var == "count") {
        n()
      } else {
        sum(get(input$target_var), na.rm = TRUE)
      }, .groups = 'drop')
    
    pivot_data <- data %>%
      pivot_wider(names_from = all_of(input$col_vars), values_from = Operaciones)
    
    pivot_data$TOTAL <- rowSums(pivot_data[ , sapply(pivot_data, is.numeric)], na.rm = TRUE)
    pivot_data
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
  
  report_data <- eventReactive(input$generate_report, {
    if (input$report_type == "ANUAL") {
      db_luae %>%
        mutate(anio_otorgamiento = year(fecha_otorgamiento)) %>%
        filter(!estado_emision %in% c("CADUCADA", "NEGADA", "REVOCADA")) %>%
        select(numero_licencia, anio_otorgamiento) %>% 
        distinct() %>%
        group_by(anio_otorgamiento) %>%
        summarise(TOTAL_LICENCIAS = n(), .groups = 'drop')
    } else if (input$report_type == "CATEGORIZADO") {
      db_luae %>%
        mutate(anio_otorgamiento = year(fecha_otorgamiento)) %>%
        filter(!estado_emision %in% c("CADUCADA", "NEGADA", "REVOCADA")) %>%
        select(numero_licencia, anio_otorgamiento, zona_predio, administracion_zonal, actividad_economica) %>%
        filter(if ("TODOS" %in% input$reporte_zona) TRUE else zona_predio %in% input$reporte_zona,
               if ("TODOS" %in% input$reporte_admin_zonal) TRUE else administracion_zonal %in% input$reporte_admin_zonal,
               if ("TODOS" %in% input$reporte_actividad) TRUE else actividad_economica %in% input$reporte_actividad) %>%
        group_by(anio_otorgamiento, zona_predio, administracion_zonal, actividad_economica) %>%
        summarise(TOTAL_LICENCIAS = n(), .groups = 'drop')
    }
  })
  
  output$report_table <- renderDT({
    datatable(report_data(), options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
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
