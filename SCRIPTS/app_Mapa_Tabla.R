library(leaflet)
library(shiny)
library(tigris)
library(CDCPLACES)
library(dplyr)
library(htmltools)

ohio <- get_places(state = "OH", measure = "ACCESS2", geometry = TRUE) |>
  filter(datavaluetypeid == "AgeAdjPrv") |>
  select(year, stateabbr, locationname, measure, data_value, geometry) |>
  sf::st_transform(crs = 4326)


ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(".leaflet-container { background: none; } .well { background: none;}"))
  ),
  
  # Application title
  titlePanel("My Demo App"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      leafletOutput("mapfilter", height = 250)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DT::DTOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv <- reactiveValues(selected_counties = NULL,
                       filtered_data = ohio) # Initialize reactive value for selected counties
  
  observeEvent(input$mapfilter_shape_click, { # this is the logic behind the "click" of the map.
    
    click <- input$mapfilter_shape_click
    
    ########## map behavior ################
    # If a county is clicked
    
    if (click$id %in% rv$selected_counties) {
      # If selected, remove it
      rv$selected_counties <- rv$selected_counties[rv$selected_counties != click$id]
    } else if(click$id == "selected"){ # when a county is clicked again it is removed
      
      rv$selected_counties <- rv$selected_counties[rv$selected_counties != tail(rv$selected_counties, n = 1)]
      
    }else {
      # If not selected, add it
      rv$selected_counties <- c(rv$selected_counties, click$id)
    }
    
    leafletProxy("mapfilter", session) |>
      addPolygons(data = ohio,
                  layerId = ~locationname,
                  label = ~locationname,
                  fillColor = "steelblue", # Change fill color based on selection
                  col = "black",
                  weight = 2,
                  fillOpacity = ifelse(ohio$locationname %in% rv$selected_counties, 1, 0.1),
                  highlight = highlightOptions(
                    fillOpacity = 1,
                    bringToFront = TRUE)
      )
    
    
  })
  
  output$mapfilter <- renderLeaflet({ # rendering the filter map
    
    leaflet(ohio,
            options = leafletOptions( # initializing the map
              zoomControl = FALSE,
              dragging = FALSE,
              minZoom = 6,
              maxZoom = 6
            )) %>%
      addPolygons(layerId = ~locationname,
                  label = ~locationname,
                  #   fillColor = "black",
                  col = "black",
                  fillColor = "steelblue",
                  weight = 2,
                  fillOpacity = .1,
                  highlight = highlightOptions(
                    fillOpacity = 1,
                    bringToFront = TRUE
                  ))
    
  })
  
  output$table <- DT::renderDT({
    
    rv$filtered_data |>
      sf::st_set_geometry(NULL) |>
      DT::datatable()
    
  })
  
  observe({ # Update table filtering based on selected counties
    if (!is.null(rv$selected_counties) & length(rv$selected_counties) > 0) { # Check if any counties are selected
      rv$filtered_data <- ohio |>
        filter(locationname %in% rv$selected_counties)
    } else {
      rv$filtered_data <- ohio
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)