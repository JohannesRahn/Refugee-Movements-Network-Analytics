source("server.R")
dt.asylum <- prepare_data()
# dt.asylum$Country.of.origin <- as.character(dt.asylum$Country.of.origin)
ui <- dashboardPage(
  dashboardHeader(
    title = "Asylum Analysis"
    #, includeCSS("https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css")
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
      , menuItem("Network Graph", tabName = "network", icon = icon("globe"))
      , menuItem("Network Exploration", tabName = "network_exploration", icon = icon("code"))
      , menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")),
    tabItems(
      tabItem(tabName = "analysis",
              # Add graphs for descriptive analysis page
              # Filter by income group
              fluidRow(
                column(6, plotOutput("total.decisions.plot")),
                column(6, plotOutput("recognized.decisions.plot"))),
              fluidRow(
                column(6, plotOutput("rejected.decisions.plot")),
                column(6, plotOutput("otherwise.closed.decisions.plot")),
                column(6, plotOutput("total.closed.decisions.plot"))
              )
              
      ),
      tabItem(tabName = "network",
              uiOutput("origin.selector"),
              uiOutput("asylum.selector"),
              uiOutput("asylum.income.selector"),
              pickerInput("origin", "Country of origin", 
                          choices=unique(dt.asylum$Country.of.origin),
                          options = list(actions_box = TRUE), 
                          selected=NULL, multiple=TRUE),

      ),
      tabItem(tabName = "network_exploration"
              
      ),
      tabItem(tabName = "about"
              
      )
    )
  )
)


# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1080))