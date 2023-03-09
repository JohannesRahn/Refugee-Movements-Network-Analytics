source("server.R")

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Asylum Analysis"
    # ,tags$head(
    #   tags$link(list(rel = "stylesheet", type = "text/css", 
    #             href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"))
    # )
    # Not working
    #, tags$head(tags$link(rel = "shortcut icon", href = "DALL-E-Web-Icon.png"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Analysis", tabName = "analysis", 
               icon = icon("chart-bar"))
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
      ),
      tabItem(tabName = "about",
              
      )
    )
  )
)
# Run the app
shinyApp(ui, server)