library(shiny)
library(shinythemes)

# source("global.R")

ui <- function() {
  fluidPage(
    theme = shinytheme("flatly"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    div(
      class = "banner",
      img(src = "header.png", class = "img-responsive-header")  # La ruta correcta si la imagen está en www/
    ),
    
    navbarPage(
      title = div(class = "navbar-title", "Sistema de Información de Desarrollo Económico y Productivo del DMQ"),
      tags$head(tags$style(HTML('.navbar-static-top {background-color: #28367F;}',
                                '.navbar-default .navbar-nav>.active>a {background-color: #28367F;}'))),
      tabPanel("Inicio", ui_Inicio()),  # Asegúrate de que estas funciones están definidas
      tabPanel("LUAEs", ui_Exploratorio()),
      tabPanel("LMUs", ui_Reportes()),
      tabPanel("Plusvalía", ui_Geovisor()),
      tabPanel("Modelos de Ensamble", ui_Geovisor())
    )
  )
}

