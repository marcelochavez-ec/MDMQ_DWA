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
                 selectInput("estado_emision", "Estado de Emisión:", choices = c("TODOS", unique(db_luae$ESTADO_EMISION)), multiple = TRUE, selected = "TODOS"),
                 selectInput("movimiento_actual", "Movimiento Actual:", choices = c("TODOS", unique(db_luae$MOVIMIENTO_ACTUAL)), multiple = TRUE, selected = "TODOS"),
                 selectInput("estado_bpm", "Estado BPM:", choices = c("TODOS", unique(db_luae$ESTADO_BPM)), multiple = TRUE, selected = "TODOS"),
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
             ))
  )
)

server <- function(input, output) {
  pivot_data <- eventReactive(input$run_button, {
    req(input$row_vars, input$col_vars, input$value_var, input$target_var)
    
    # Convertir el input num_licencia en un vector
    num_licencia_vec <- unlist(strsplit(input$num_licencia, ","))
    num_licencia_vec <- trimws(num_licencia_vec)  # Eliminar espacios en blanco adicionales
    
    filtered_data <- db_luae %>%
      filter(if ("TODOS" %in% input$estado_emision) TRUE else ESTADO_EMISION %in% input$estado_emision,
             if ("TODOS" %in% input$movimiento_actual) TRUE else MOVIMIENTO_ACTUAL %in% input$movimiento_actual,
             if ("TODOS" %in% input$estado_bpm) TRUE else ESTADO_BPM %in% input$estado_bpm,
             if (length(num_licencia_vec) == 0) TRUE else NUMERO_LICENCIA %in% num_licencia_vec)
    
    data <- filtered_data %>%
      group_by(across(all_of(c(input$row_vars, input$col_vars)))) %>%
      summarise(Operaciones = if (input$value_var == "count") {
        n()
      } else {
        sum(as.numeric(get(input$target_var)), na.rm = TRUE)
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
      filter(if (length(base_num_licencia_vec) == 0) TRUE else NUMERO_LICENCIA %in% base_num_licencia_vec)
  })
  
  output$base_table <- renderDT({
    datatable(filtered_base_data(), options = list(pageLength = 10, scrollX = TRUE, scrollY = TRUE), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)
