server_Reportes <- function(input, output, session) {
  # Código específico para el tabPanel de Reportes
  
  output$output_reporte <- renderUI({
    # Aquí puedes agregar el código para mostrar el reporte seleccionado
    # usando la función renderUI y el archivo R Markdown correspondiente.
    # Por ejemplo:
    if (input$reporte == "Reporte 1") {
      includeMarkdown("reporte1.Rmd")
    } else if (input$reporte == "Reporte 2") {
      includeMarkdown("reporte2.Rmd")
    } else if (input$reporte == "Reporte 3") {
      includeMarkdown("reporte3.Rmd")
    }
  })
  
}
