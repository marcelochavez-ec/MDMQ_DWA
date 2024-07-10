library(shiny)

# Definir la interfaz de usuario
ui <- fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tags$div(
        style = "position: relative;",
        tags$div(
            style = "padding: 20px; color: white; font-size: 200%; text-align: center; display: fill; border-radius: 5px; background-color: #136f63; overflow: hidden; font-weight: 700;",
            "🛸 Regresión Lineal Simple 🛸"
        ),
        tags$div(
            style = "position: absolute; bottom: 0; right: 0; padding: 10px; color: white; font-size: 100%; font-weight: 700;",
            "® Marcelo Chávez"
        )
    ),
    
    br(),
    
    fluidRow(
        column(4, align = "center", actionButton("boton1", "Nacional", class = "btn-lg")),
        column(4, align = "center", actionButton("boton2", "Provincial", class = "btn-lg")),
        column(4, align = "center", actionButton("boton3", "Región", class = "btn-lg"))
    )
)

# Definir el servidor
server <- function(input, output, session) {
    # Acciones cuando se hace clic en los botones (puedes agregar tus propias acciones aquí)
    observeEvent(input$boton1, {
        print("Se hizo clic en el Botón 1")
    })
    observeEvent(input$boton2, {
        print("Se hizo clic en el Botón 2")
    })
    observeEvent(input$boton3, {
        print("Se hizo clic en el Botón 3")
    })
}

# Crear la aplicación Shiny
shinyApp(ui, server)
