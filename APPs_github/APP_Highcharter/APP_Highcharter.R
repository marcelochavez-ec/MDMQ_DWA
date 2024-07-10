library(shiny)
library(dplyr)
library(purrr)
library(gapminder)
library(highcharter)


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  sidebarLayout(
    sidebarPanel(
      titlePanel("R Shiny Highcharts"),
      selectInput(
        inputId = "inContinent",
        label = "Continent:",
        choices = unique(gapminder$continent),
        selected = "Europe"
      ),
      selectInput(
        inputId = "inYearMin",
        label = "Start year:",
        choices = unique(gapminder$year)[1:length(unique(gapminder$year)) - 1],
        selected = min(gapminder$year)
      ),
      selectInput(
        inputId = "inYearMax",
        label = "End year:",
        choices = unique(gapminder$year)[2:length(unique(gapminder$year))],
        selected = max(gapminder$year)
      ),
      width = 3
    ),
    mainPanel(
      tags$h3("Latest stats:"),
      tags$div(
        tags$div(
          tags$p("# Countries:"),
          textOutput(outputId = "outNCountries")
        ) %>% tagAppendAttributes(class = "stat-card"),
        tags$div(
          tags$p("Median life exp:"),
          textOutput(outputId = "outMedLifeExp")
        ) %>% tagAppendAttributes(class = "stat-card"),
        tags$div(
          tags$p("Median population:"),
          textOutput(outputId = "outMedPop")
        ) %>% tagAppendAttributes(class = "stat-card"),
        tags$div(
          tags$p("Median GDP:"),
          textOutput(outputId = "outMedGDP")
        ) %>% tagAppendAttributes(class = "stat-card")
      ) %>% tagAppendAttributes(class = "stat-card-container"),
      tags$div(
        tags$h3("Summary stats:"),
        tags$div(
          tags$div(
            highchartOutput(outputId = "chartLifeExpByYear", height = 500)
          ) %>% tagAppendAttributes(class = "chart-card"),
          tags$div(
            highchartOutput(outputId = "chartGDPByYear", height = 500)
          ) %>% tagAppendAttributes(class = "chart-card"),
        ) %>% tagAppendAttributes(class = "base-charts-container")
      ) %>% tagAppendAttributes(class = "card-container"),
      tags$div(
        tags$h3("Drilldown:"),
        tags$div(
          highchartOutput(outputId = "chartDrilldown", height = 500)
        ) %>% tagAppendAttributes(class = "chart-card chart-card-full")
      ) %>% tagAppendAttributes(class = "card-container"),
      width = 9
    ) %>% tagAppendAttributes(class = "main-container")
  )
)

server <- function(input, output) {
  data_cards <- reactive({
    gapminder %>%
      filter(
        continent == input$inContinent,
        year == max(year)
      ) %>%
      summarise(
        nCountries = n_distinct(country),
        medianLifeExp = median(lifeExp),
        medianPopM = median(pop / 1e6),
        medianGDP = median(gdpPercap)
      )
  })
  
  data_charts <- reactive({
    gapminder %>%
      filter(
        continent == input$inContinent,
        between(year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      ) %>%
      group_by(year) %>%
      summarise(
        medianLifeExp = round(median(lifeExp), 1),
        medianGDP = round(median(gdpPercap), 2)
      )
  })
  
  drilldown_chart_base_data <- reactive({
    gapminder %>%
      filter(
        continent == input$inContinent,
        year == max(year)
      ) %>%
      group_by(country) %>%
      summarise(
        pop = round(pop, 1)
      ) %>%
      arrange(desc(pop))
  })
  
  drilldown_chart_drilldown_data <- reactive({
    gapminder %>%
      filter(
        continent == input$inContinent,
        between(year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      ) %>%
      group_nest(country) %>%
      mutate(
        id = country,
        type = "column",
        data = map(data, mutate, name = year, y = pop),
        data = map(data, list_parse)
      )
  })
  
  
  output$outNCountries <- renderText({
    data_cards()$nCountries
  })
  output$outMedLifeExp <- renderText({
    paste(round(data_cards()$medianLifeExp, 1), "years")
  })
  output$outMedPop <- renderText({
    paste0(round(data_cards()$medianPopM, 2), "M")
  })
  output$outMedGDP <- renderText({
    paste0("$", round(data_cards()$medianGDP, 2))
  })
  
  output$chartLifeExpByYear <- renderHighchart({
    hchart(data_charts(), 
           "column",
           hcaes(x = year, 
                 y = medianLifeExp), 
           color = "#0198f9", 
           name = "Median life expectancy") |>
      hc_title(text = "Median life expectancy by year",
               align = "left") |>
      hc_xAxis(title = list(text = "Year")) |>
      hc_yAxis(title = list(text = "Life expectancy"))
  })
  
  output$chartGDPByYear <- renderHighchart({
    hchart(data_charts(),
           "line", 
           hcaes(x = year,
                 y = medianGDP), 
           color = "#800000", 
           name = "Median GDP") |>
      hc_title(text = "Median GDP by year", 
               align = "left") |>
      hc_xAxis(title = list(text = "Year")) |>
      hc_yAxis(title = list(text = "GDP"))
  })
  
  output$chartDrilldown <- renderHighchart({
    hchart(
      drilldown_chart_base_data(),
      "column",
      hcaes(x = country, y = pop, drilldown = country),
      name = "Population"
    ) %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list_parse(drilldown_chart_drilldown_data())
      ) |>
      hc_colors(c("#004c5f")) |>
      hc_title(text = "Population report", align = "left") |>
      hc_xAxis(title = list(text = "")) |>
      hc_yAxis(title = list(text = "Population"))
  })
}


shinyApp(ui = ui, server = server)