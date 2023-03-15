# install.packages("shinydashboard")
# install.packages("dplyr")
#install.packages("rsconnect")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rsconnect)


source("server.R")


# Define UI
ui <- dashboardPage(
  
  dashboardHeader(
    title = "Asylum Analysis"
    # Not working
    #, tags$head(tags$link(rel = "shortcut icon", href = "DALL-E-Web-Icon.png"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Analysis", tabName = "analysis", icon = icon("chart-bar"))
      , menuItem("Network Graph", tabName = "worldmap", icon = icon("globe"))
      , menuItem("About", tabName = "worldmap", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analysis",
              # Add graphs for descriptive analysis page
              fluidRow(
                column(6, plotOutput("total.decisions.plot")),
                column(6, plotOutput("recognized.decisions.plot")),
                column(6, plotOutput("rejected.decisions.plot")),
                column(6, plotOutput("otherwise.closed.decisions.plot")),
                column(6, plotOutput("total.closed.decisions.plot"))
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
  #dt.full.data <- prepare_data()
  dt.aggregated.asylum <- aggregate_data()

  # Total Asylum Decisions per Year plot
  output$total.decisions.plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Total_decisions)) +
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
  output$recognized.decisions.plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Recognized_decisions)) +
      geom_line(color = "#0072B2") +
      labs(title = "Recognized Decisions per Year",
           x = "Year",
           y = "Recognized Decisions") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
  
  output$rejected.decisions.plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Rejected_decisions)) +
      geom_line(color = "#0072B2") +
      labs(title = "Rejected Decisions per Year",
           x = "Year",
           y = "Rejected Decisions") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
  
  output$otherwise.closed.decisions.plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Otherwise_closed)) +
      geom_line(color = "#0072B2") +
      labs(title = "Otherwise Closed Decisions per Year",
           x = "Year",
           y = "Oterwise Closed Decisions") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
  
  output$total.closed.decisions.plot <- renderPlot({
    ggplot(dt.aggregated.asylum, aes(x = Year, y = Total_closed)) +
      geom_line(color = "#0072B2") +
      labs(title = " Decisions per Year",
           x = "Year",
           y = "Oterwise Closed Decisions") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
  })
}

# Run the app
shinyApp(ui, server)

library(rsconnect)
rsconnect::setAccountInfo(name='johannesrahn', token='0AEEF974B4F3D3DC382E3E5DC9B30A54', secret='SQ4oEwqbn97TEfhTPlGy0WgorfB74RYL9HxGzdxy')

rsconnect::deployApp()