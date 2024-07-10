rm(list = ls())

library(shiny)
library(RPostgreSQL)
library(highcharter)

# Crear una conexión a la base de datos PostgreSQL
con <- dbConnect(PostgreSQL(),
                 dbname = "db_stat",
                 host = "localhost",
                 port = 5432,
                 user = "postgres",
                 password = "marce")

# Definir la UI
ui <- fluidPage(
  
  tags$head(rel="stylesheet", type="text/css", href="www/bootstrap.css"),
 
  titlePanel("Selección de Schema, Tabla y Análisis"),
    
    sidebarLayout(
      
        sidebarPanel(
            
          selectInput("id_schema",
                        "Seleccionar Schema:", 
                        choices = "Seleccione un Schema"),
          
          selectInput("id_tabla",
                      "Seleccionar Tabla:", 
                      choices = "Seleccione una Tabla"),
          
          radioButtons("id_analisis", 
                       "Seleccionar Análisis:",
                       choices = c("Análisis Univariado",
                                   "Análisis Bivariado"),
                       selected = "")
        ),
        mainPanel(
            textOutput("mensaje")
        )
    )
)

# Definir el servidor
server <- function(input, output, session) {
    
    # Mensajes por defecto
    output$id_schema <- renderUI({
        if (is.null(input$id_schema)) {
            "Seleccione un Schema"
        } else {
            input$id_schema
        }
    })
    
    output$id_table <- renderUI({
        if (is.null(input$id_tabla)) {
            "Seleccione una Tabla"
        } else {
            input$id_table
        }
    })
    
    output$id_analysis <- renderUI({
        if (is.null(input$id_analisis)) {
            "Seleccione un Análisis"
        } else {
            input$id_analysis
        }
    })
    
    # Obtener la lista de schemas
    schemas <- reactive({
        query <- "SELECT schema_name FROM information_schema.schemata WHERE schema_name NOT LIKE 'pg_%' AND schema_name != 'information_schema'"
        dbGetQuery(con, query)$schema_name
    })
    
    # Actualizar las opciones del selectInput para seleccionar el schema
    observe({
        updateSelectInput(session, "id_schema", choices = c("Seleccione un Schema", schemas()))
    })
    
    # Obtener la lista de tablas para el schema seleccionado
    tables <- reactive({
        id_schema <- input$id_schema
        if (!is.null(id_schema) && id_schema != "Seleccione un Schema") {
            query <- paste("SELECT table_name FROM information_schema.tables WHERE table_schema = $1")
            dbGetQuery(con, query, params = list(id_schema))$table_name
        } else {
            character(0)
        }
    })
    
    # Actualizar las opciones del selectInput para seleccionar la tabla
    observe({
        updateSelectInput(session, "id_tabla", choices = c("Seleccione una Tabla", tables()))
    })
    
    # Mostrar el mensaje de selección del schema, la tabla y el análisis
    output$mensaje <- renderText({
        id_schema <- input$id_schema
        id_tabla <- input$id_tabla
        id_analisis <- input$id_analisis
        
        if (id_schema != "Seleccione un Schema" && id_tabla != "Seleccione una Tabla") {
            if (id_analisis %in% c("Análisis Univariado", "Análisis Relacional")) {
                "Funcionalidad en construcción"
            } else {
                paste("Usted acaba de seleccionar el schema:", id_schema,
                      "la tabla:", id_tabla, "y el análisis:", id_analisis)
            }
        } else {
            "Seleccione un Schema y una Tabla"
        }
    })
    
    # Cerrar la conexión a la base de datos al detener la aplicación
    onStop(function() {
        dbDisconnect(con)
    })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
