library(shiny)
library(htmlwidgets)
library(dplyr)
library(lubridate)
library(pivottabler)

# START OF DATA PREP

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# to be used when getting station names in joins below
tsorigin = transmute(trainstations, CrsCode=as.character(CrsCode),
                     OriginName=as.character(StationName))
tsdestination = transmute(trainstations, CrsCode=as.character(CrsCode),
                          DestinationName=as.character(StationName))

# recode status and join to get station names from CrsCodes
data <- bhmtrains %>%
  mutate(Status = recode(Status,
                         "A" = "Active", "C" = "Cancelled", "R" = "Reinstated"),
         Origin = as.character(Origin),
         Destination = as.character(Destination)) %>%
  left_join(tsorigin, by = c("Origin" = "CrsCode")) %>%
  left_join(tsdestination, by = c("Destination" = "CrsCode"))

# derive some additional delay data
data <- mutate(data,
               GbttDateTime=if_else(is.na(GbttArrival), GbttDeparture, GbttArrival),
               GbttMonth=make_date(year=year(GbttDateTime), month=month(GbttDateTime), day=1),
               IsArrival=ifelse(is.na(GbttArrival), 0, 1),
               ArrivalDelta=difftime(ActualArrival, GbttArrival, units="mins"),
               ArrivalDelay=ifelse(ArrivalDelta<0, 0, ArrivalDelta),
               DelayedByMoreThan5Minutes=ifelse(ArrivalDelay>5,1,0))

totalTrainCount <- nrow(data)

# END OF DATA PREP

ui <- fluidPage(
  
  titlePanel("Pivottabler Multi-Level Example Shiny App"),
  
  fluidRow(
    column(3,
           selectInput("selectRows1", label = h5("Rows 1"),
                       choices = list("None" = "None",
                                      "Status" = "Status",
                                      "Train Category" = "TrainCategory",
                                      "TOC" = "TOC",
                                      "Power Type" = "PowerType",
                                      "Scheduled Speed" = "SchedSpeedMPH",
                                      "Origin" = "OriginName",
                                      "Destination" = "DestinationName"),
                       selected = "TOC")
    ),
    column(3,
           selectInput("selectRows2", label = h5("Rows 2"),
                       choices = list("None" = "None",
                                      "Status" = "Status",
                                      "Train Category" = "TrainCategory",
                                      "TOC" = "TOC",
                                      "Power Type" = "PowerType",
                                      "Scheduled Speed" = "SchedSpeedMPH",
                                      "Origin" = "OriginName",
                                      "Destination" = "DestinationName"),
                       selected = "None")
    ),
    column(3,
           selectInput("selectCols1", label = h5("Columns 1"),
                       choices = list("None" = "None",
                                      "Status" = "Status",
                                      "Train Category" = "TrainCategory",
                                      "TOC" = "TOC",
                                      "Power Type" = "PowerType",
                                      "Scheduled Speed" = "SchedSpeedMPH",
                                      "Origin" = "OriginName",
                                      "Destination" = "DestinationName"),
                       selected = "TrainCategory")
    ),
    column(3,
           selectInput("selectCols2", label = h5("Columns 2"),
                       choices = list("None" = "None",
                                      "Status" = "Status",
                                      "Train Category" = "TrainCategory",
                                      "TOC" = "TOC",
                                      "Power Type" = "PowerType",
                                      "Scheduled Speed" = "SchedSpeedMPH",
                                      "Origin" = "OriginName",
                                      "Destination" = "DestinationName"),
                       selected = "None")
    )
  ),
  
  fluidRow(
    column(3,
           selectInput("selectMeasure1", label = h5("Measure 1"),
                       choices = list("Train Count" = "Train Count",
                                      "% of Trains" = "% of Trains",
                                      "Total Arrival Delay Minutes" = "Total Arrival Delay Minutes",
                                      "Average Arrival Delay Minutes" = "Average Arrival Delay Minutes",
                                      "Max Arrival Delay Minutes" = "Max Arrival Delay Minutes",
                                      "Trains with Arrival Delay >= 5 Minutes",
                                      "% of Trains with Arrival Delay >= 5 Minutes"),
                       selected = "Train Count")
    ),
    column(3,
           selectInput("selectMeasure2", label = h5("Measure 2"),
                       choices = list("None",
                                      "Train Count" = "Train Count",
                                      "% of Trains" = "% of Trains",
                                      "Total Arrival Delay Minutes" = "Total Arrival Delay Minutes",
                                      "Average Arrival Delay Minutes" = "Average Arrival Delay Minutes",
                                      "Max Arrival Delay Minutes" = "Max Arrival Delay Minutes",
                                      "Trains with Arrival Delay >= 5 Minutes",
                                      "% of Trains with Arrival Delay >= 5 Minutes"),
                       selected = "None")
    ),
    column(6,
           br(),
           helpText("Note:  Selecting Origin and/or Destination in rows/columns will result in thousands or
             tens of thousands of cells being calculated, so the pivot table may take a couple of
             minutes to calculate.")
    )
  ),
  
  hr(),
  
  pivottablerOutput('pvt')
  
)

server <- function(input, output) {
  
  
  
  output$pvt <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(data)
    
    # rows and columns
    if (input$selectCols1 != "None") { pt$addColumnDataGroups(input$selectCols1) }
    if (input$selectCols2 != "None") { pt$addColumnDataGroups(input$selectCols2) }
    if (input$selectRows1 != "None") { pt$addRowDataGroups(input$selectRows1) }
    if (input$selectRows2 != "None") { pt$addRowDataGroups(input$selectRows2) }
    
    # measure 1
    if (input$selectMeasure1 == "Train Count") {
      pt$defineCalculation(calculationName="TotalTrains",
                           summariseExpression="n()",
                           caption="Train Count")
    }
    else if (input$selectMeasure1 == "% of Trains") {
      pt$defineCalculation(calculationName="TotalTrains",
                           summariseExpression="n()",
                           visible=FALSE)
      pt$defineCalculation(calculationName="PercentageOfTrains",
                           caption="% of Trains",
                           type="calculation",
                           basedOn=c("TotalTrains"),
                           format="%.2f %%",
                           calculationExpression=paste0("values$TotalTrains/", totalTrainCount, "*100"))
    }
    else if (input$selectMeasure1 == "Total Arrival Delay Minutes") {
      pt$defineCalculation(calculationName="TotalArrivalDelayMinutes",
                           caption="Total Arrival Delay Minutes",
                           summariseExpression="sum(ArrivalDelay, na.rm = TRUE)")
    }
    else if (input$selectMeasure1 == "Average Arrival Delay Minutes") {
      pt$defineCalculation(calculationName="TotalArrivals",
                           summariseExpression="sum(IsArrival, na.rm=TRUE)", visible=FALSE)
      pt$defineCalculation(calculationName="TotalArrivalDelayMinutes",
                           summariseExpression="sum(ArrivalDelay, na.rm = TRUE)", visible=FALSE)
      pt$defineCalculation(calculationName="AverageArrivalDelayMinutes",
                           caption="Average Arrival Delay Minutes",
                           type="calculation",
                           basedOn=c("TotalArrivals", "TotalArrivalDelayMinutes"),
                           format="%.2f",
                           calculationExpression="values$TotalArrivalDelayMinutes/values$TotalArrivals")
    }
    else if (input$selectMeasure1 == "Max Arrival Delay Minutes") {
      pt$defineCalculation(calculationName="MaxArrivalDelayMinutes",
                           caption="Max Arrival Delay Minutes",
                           summariseExpression="max(ArrivalDelay, na.rm = TRUE)")
    }
    else if (input$selectMeasure1 == "Trains with Arrival Delay >= 5 Minutes") {
      pt$defineCalculation(calculationName="Trains5orMoreMinsLate",
                           caption="Trains with Arrival Delay >= 5 Minutes",
                           summariseExpression="sum(DelayedByMoreThan5Minutes, na.rm = TRUE)")
    }
    else if (input$selectMeasure1 == "% of Trains with Arrival Delay >= 5 Minutes") {
      pt$defineCalculation(calculationName="TotalArrivals",
                           summariseExpression="sum(IsArrival, na.rm=TRUE)", visible=FALSE)
      pt$defineCalculation(calculationName="Trains5orMoreMinsLate",
                           summariseExpression="sum(DelayedByMoreThan5Minutes, na.rm = TRUE)",
                           visible=FALSE)
      pt$defineCalculation(calculationName="PercentageOfTrainsWithArrivalDelay5orMoreMinutesLate",
                           caption="% of Trains with Arrival Delay >= 5 Minutes",
                           type="calculation", basedOn=c("TotalArrivals", "Trains5orMoreMinsLate"),
                           format="%.2f %%",
                           calculationExpression="values$Trains5orMoreMinsLate/values$TotalArrivals*100")
    }
    
    # measure 2
    if (input$selectMeasure2 == "Train Count") {
      pt$defineCalculation(calculationName="TotalTrains2",
                           summariseExpression="n()",
                           caption="Train Count")
    }
    else if (input$selectMeasure2 == "% of Trains") {
      pt$defineCalculation(calculationName="TotalTrains2",
                           summariseExpression="n()",
                           visible=FALSE)
      pt$defineCalculation(calculationName="PercentageOfTrains2",
                           caption="% of Trains",
                           type="calculation",
                           basedOn=c("TotalTrains2"),
                           format="%.2f %%",
                           calculationExpression=paste0("values$TotalTrains2/", totalTrainCount, "*100"))
    }
    else if (input$selectMeasure2 == "Total Arrival Delay Minutes") {
      pt$defineCalculation(calculationName="TotalArrivalDelayMinutes2",
                           caption="Total Arrival Delay Minutes",
                           summariseExpression="sum(ArrivalDelay, na.rm = TRUE)")
    }
    else if (input$selectMeasure2 == "Average Arrival Delay Minutes") {
      pt$defineCalculation(calculationName="TotalArrivals2",
                           summariseExpression="sum(IsArrival, na.rm=TRUE)", visible=FALSE)
      pt$defineCalculation(calculationName="TotalArrivalDelayMinutes2",
                           summariseExpression="sum(ArrivalDelay, na.rm = TRUE)", visible=FALSE)
      pt$defineCalculation(calculationName="AverageArrivalDelayMinutes2",
                           caption="Average Arrival Delay Minutes",
                           type="calculation",
                           basedOn=c("TotalArrivals2", "TotalArrivalDelayMinutes2"),
                           format="%.2f",
                           calculationExpression="values$TotalArrivalDelayMinutes2/values$TotalArrivals2")
    }
    else if (input$selectMeasure2 == "Max Arrival Delay Minutes") {
      pt$defineCalculation(calculationName="MaxArrivalDelayMinutes2",
                           caption="Max Arrival Delay Minutes",
                           summariseExpression="max(ArrivalDelay, na.rm = TRUE)")
    }
    else if (input$selectMeasure2 == "Trains with Arrival Delay >= 5 Minutes") {
      pt$defineCalculation(calculationName="Trains5orMoreMinsLate2",
                           caption="Trains with Arrival Delay >= 5 Minutes",
                           summariseExpression="sum(DelayedByMoreThan5Minutes, na.rm = TRUE)")
    }
    else if (input$selectMeasure2 == "% of Trains with Arrival Delay >= 5 Minutes") {
      pt$defineCalculation(calculationName="TotalArrivals2",
                           summariseExpression="sum(IsArrival, na.rm=TRUE)", visible=FALSE)
      pt$defineCalculation(calculationName="Trains5orMoreMinsLate2",
                           summariseExpression="sum(DelayedByMoreThan5Minutes, na.rm = TRUE)",
                           visible=FALSE)
      pt$defineCalculation(calculationName="PercentageOfTrainsWithArrivalDelay5orMoreMinutesLate2",
                           caption="% of Trains with Arrival Delay >= 5 Minutes",
                           type="calculation", basedOn=c("TotalArrivals2", "Trains5orMoreMinsLate2"),
                           format="%.2f %%",
                           calculationExpression="values$Trains5orMoreMinsLate2/values$TotalArrivals2*100")
    }
    
    # generate pivot tabler
    pt$evaluatePivot()
    pivottabler(pt)
  })
}

shinyApp(ui = ui, server = server)