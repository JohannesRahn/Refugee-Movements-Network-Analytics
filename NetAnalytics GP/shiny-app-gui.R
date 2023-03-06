# install.packages("shinydashboard")
# install.packages("dplyr")

library(shiny)
library(shinydashboard)
library(shinydashboard)
library(ggplot2)
library(dplyr)
source("DataPrep.R")


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Asylum Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Network Graph", tabName = "worldmap", icon = icon("globe")),
      menuItem("About", tabName = "worldmap", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              # Add graphs for descriptive analysis page
              fluidRow(
                column(6, plotOutput("total_decisions_plot")),
                column(6, plotOutput("recognized_decisions_plot"))
              )
              
      ),
      tabItem(tabName = "worldmap",
              # Add network graph for world map page
      )
    )
  )
)

# Define server
server <- function(input, output) {
  dt.full.data <- prepare_data()
  dt.aggregated.asylum <-   dt.full.data %>%
    group_by(Year) %>%
    summarize(Total.decisions = sum(Total.decisions),
              Recognized.decisions = sum(Recognized.decisions))
  

  # Total Asylum Decisions per Year plot
  output$total_decisions_plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Total.decisions)) +
      geom_line(color = "#0072B2") +
      labs(title = "Total Asylum Decisions per Year",
           x = "Year",
           y = "Total Asylum Decisions") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
  
  # Recognized Decisions per Year plot
  output$recognized_decisions_plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Recognized.decisions)) +
      geom_line(color = "#0072B2") +
      labs(title = "Recognized Decisions per Year",
           x = "Year",
           y = "Recognized Decisions") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
}

# Run the app
shinyApp(ui, server)