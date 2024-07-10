ui_Reportes <- function() {
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Selecciona un reporte"),
        selectInput("reporte", "Reportes disponibles",
                    choices = c("Reporte 1", "Reporte 2", "Reporte 3"),
                    selected = "Reporte 1")
      ),
      mainPanel(
        h3("Vista previa del reporte"),
        uiOutput("output_reporte")
      )
    )
  )
}
