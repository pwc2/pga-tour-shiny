# Load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(lubridate)
library(stringr)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

# Load data from script
source("loadData.R")

# List variables that can be plotted with more informative names
varChoices = list(`Rounds Played` = "Rounds",
               "Wins" = "Wins",
               `Number of Top 10 Finishes` = "Top.10",
               `Winnings (millions of $)` = "Money",
               `Final Official World Golf Ranking` = "Final.OWGR",
               `Average Official World Golf Ranking` = "Average.OWGR",
               `FedEx Cup Points` = "Points",
               `Average Driving Distance (yards)` = "Average.Driving.Distance",
               `Percentage of Fairways Hit` = "Fairway.Percentage",
               `Percentage of Greens in Regulation Hit` = "GIR",
               `Average Number of Putts` = "Average.Putts",
               `Average Scrambling Percentage` = "Average.Scrambling",
               `Average Score` = "Average.Score",
               `Average Strokes Gained: Total` = "Average.SG.Total",
               `Average Strokes Gained: Off the Tee` = "SG.OTT",
               `Average Strokes Gained: Approach` = "SG.APR",
               `Average Strokes Gained: Around the Green` = "SG.ARG",
               `Average Strokes Gained: Putting` = "Average.SG.Putts")

# Create User Interface object
ui <- navbarPage(
  "Exploring PGA Tour Statistics (2010-2018)",
  theme = shinytheme("yeti"),
  
  tabPanel("About",
           headerPanel(includeMarkdown("example.md")),
           
           mainPanel(
             h4("You can view just a single plot, here is one from the 2018 season:"),
             plotlyOutput("plot2"),
             h4("Or check the box to view all years:"),
             plotlyOutput("plot3"),
             h2("Click on the explore tab at the top of the page to get started!"),
             br()
             )
           ),
  
  tabPanel("Explore",
           pageWithSidebar(
             headerPanel(""),
             
             sidebarPanel(
               selectInput("year", label = "Select a year:", choices = 2018:2010, selected = 2018),
               checkboxInput("allYears", label = "Or check here to view all years", value = FALSE),
               selectInput("xcol", label = "Select a variable for the x-axis:", choices = varChoices, selected = "Rounds"),
               selectInput("ycol", label = "Select a variable for the y-axis:", choices = varChoices, selected = "Wins"),
               helpText("Select which golfers to view based on their final Official World Golf Ranking (OWGR) for the season."),
               helpText("Plot will display golfers with a final OWGR that is between, or equal to, the selected values."),
               sliderInput("selectRank", label = "Final Official World Golf Ranking", min = 0, max = 300, value = c(0,300))
             ),
             
             mainPanel(
               plotlyOutput("plot1")
               )
             )
           )
  )

# Create server object
server <- function(input, output, session) {

  # Update variable selections for on variable based on choice for the other variable
  observe({
    # Make sure selection for y-axis is unique
    updateSelectInput(session, "ycol",
                      label = "Select a variable for the y-axis:",
                      choices = varChoices[varChoices != input$xcol],
                      selected = input$ycol)
    # Make sure selection for x-axis is unique
    updateSelectInput(session, "xcol",
                      label = "Select a variable for the x-axis:",
                      choices = varChoices[varChoices != input$ycol],
                      selected = input$xcol)
  })
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    
    if (input$allYears == TRUE) {
      tourDataCombined %>% 
        select(Player, Year, Final.OWGR, input$xcol, input$ycol) %>% 
        filter(Final.OWGR >= input$selectRank[1] & Final.OWGR <= input$selectRank[2])
    } else {
      tourDataCombined %>% 
        select(Player, Year, Final.OWGR, input$xcol, input$ycol) %>% 
        filter(Final.OWGR >= input$selectRank[1] & Final.OWGR <= input$selectRank[2] & Year %in% input$year)
    }
    
  })
  
  # Get min fox x-axis
  xmin <- reactive({
    
    tourDataCombined %>% 
      filter(Year == input$year) %>% 
      select(input$xcol) %>% 
      min()
    
  })
  # Get max for x-axis
  xmax <- reactive({
    
    tourDataCombined %>% 
      filter(Year == input$year) %>% 
      select(input$xcol) %>% 
      max()
    
  })
  # Get min for y-axis
  ymin <- reactive({
    
    tourDataCombined %>% 
      filter(Year == input$year) %>% 
      select(input$ycol) %>% 
      min()
    
  })
  # Get max for y-axis
  ymax <- reactive({
    
    tourDataCombined %>% 
      filter(Year == input$year) %>% 
      select(input$ycol) %>% 
      max()
    
  })
  
  # Plot to be shown on explore tab
  output$plot1 <- renderPlotly({
    
    if (input$allYears == TRUE) {
      p <- ggplot(selectedData(), aes(color = Final.OWGR, 
                                      text = str_c("Name: ", str_to_title(Player),
                                                   "<br>Final OWGR: ", Final.OWGR,
                                                   "<br>", names(varChoices[which(varChoices == input$xcol)]), ": ", round(selectedData()[[input$xcol]], 2),
                                                   "<br>", names(varChoices[which(varChoices == input$ycol)]), ": ", round(selectedData()[[input$ycol]], 2)))) +
        geom_point(aes_string(x = input$xcol, y = input$ycol)) +
        scale_colour_gradient(high = "#132B43", low = "#56B1F7") +
        facet_wrap(~Year) +
        labs(color = 'Final OWGR',
             x = str_c(" \n", names(varChoices[which(varChoices == input$xcol)])),
             y = names(varChoices[which(varChoices == input$ycol)])) +
        theme(panel.spacing.x=unit(0.5, "lines"),panel.spacing.y=unit(1, "lines"))
      
      ggplotly(p, tooltip = c("text")) %>% 
        layout(margin = list(l = 75, b = 75)) # to fully display y axis labels
      
      } else {
        p <- ggplot(selectedData(), aes(color = Final.OWGR, 
                                        text = str_c("Name: ", str_to_title(Player),
                                                     "<br>Final OWGR: ", Final.OWGR,
                                                     "<br>", names(varChoices[which(varChoices == input$xcol)]), ": ", round(selectedData()[[input$xcol]], 2),
                                                     "<br>", names(varChoices[which(varChoices == input$ycol)]), ": ", round(selectedData()[[input$ycol]], 2)))) +
        geom_point(aes_string(x = input$xcol, y = input$ycol)) +
          scale_x_continuous(limits = c(xmin(), xmax()), breaks = scales::pretty_breaks(n = 5)) +
          scale_y_continuous(limits = c(ymin(), ymax()), breaks = scales::pretty_breaks(n = 5)) +
          scale_colour_gradient(high = "#132B43", low = "#56B1F7") +
          labs(color = 'Final OWGR',
               x = names(varChoices[which(varChoices == input$xcol)]),
               y = names(varChoices[which(varChoices == input$ycol)]))
        
        ggplotly(p, tooltip = c("text"))
      }
    
    })
  
  # Single plot to be shown as example on about tab
  output$plot2 <- renderPlotly({
    
    data2018 <- tourDataCombined %>% filter(Year == 2018)
    
    ggplotly(
      
        ggplot(data2018, aes(color = Final.OWGR,
                             text = str_c("Name: ", str_to_title(Player),
                                          "<br>Final OWGR: ", Final.OWGR,
                                          "<br>", names(varChoices[which(varChoices == "Money")]), ": ", round(data2018$Money, 2),
                                          "<br>", names(varChoices[which(varChoices == "Average.Score")]), ": ", round(data2018$Average.Score, 2)))) +
        geom_point(aes(x = Money, y = Average.Score)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_colour_gradient(high = "#132B43", low = "#56B1F7") +
        labs(color = 'Final OWGR',
             x = names(varChoices[which(varChoices == "Money")]),
             y = names(varChoices[which(varChoices == "Average.Score")])),
      
      tooltip = c("text")
      
    )
    
  })
  
  # Faceted (by year) plot to be shown as example on about tab
  output$plot3 <- renderPlotly({
    
    ggplotly(
      
      ggplot(tourDataCombined, aes(color = Final.OWGR,
                                   text = str_c("Name: ", str_to_title(Player),
                                        "<br>Final OWGR: ", Final.OWGR,
                                        "<br>", names(varChoices[which(varChoices == "Money")]), ": ", round(tourDataCombined$Money, 2),
                                        "<br>", names(varChoices[which(varChoices == "Average.Score")]), ": ", round(tourDataCombined$Average.Score, 2)))) +
        geom_point(aes(x = Money, y = Average.Score)) +
        scale_colour_gradient(high = "#132B43", low = "#56B1F7") +
        facet_wrap(~Year) +
        labs(color = 'Final OWGR',
             x = str_c(" \n", names(varChoices[which(varChoices == "Money")])),
             y = names(varChoices[which(varChoices == "Average.Score")])) +
        theme(panel.spacing.x=unit(0.5, "lines"),panel.spacing.y=unit(1, "lines")),
      
      tooltip = c("text")
      
    ) %>% 
      layout(margin = list(l = 75, b = 75)) # to fully display y axis labels
    
    })
  
}

# Run shiny app
shinyApp(ui, server)
