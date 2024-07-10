# Definir la UI

# source("C:/Users/marcelochavez/Documents/TESIS/APP/ENDI/global.R")

ui_Exploratorio <- function() {
  fluidPage(
    tags$iframe(
      title = "QuitoEnCifras_vs_1",
      width = "100%",
      height = "800px",
      src = "https://app.powerbi.com/view?r=eyJrIjoiMzcxNjdmYzItNGI4NC00OWEzLWFjMTctOGFhZjY2ZDNlNTMzIiwidCI6ImI3YWY4Y2FmLTgzZDgtNDY0NC04NWFlLTMxN2M1NDUyMjNjMSIsImMiOjR9",
      frameborder = "0",
      allowFullScreen = TRUE,
      class = "iframe-custom"
    )
  )
}
