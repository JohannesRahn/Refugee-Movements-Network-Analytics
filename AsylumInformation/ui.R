source("server.R")
dt.asylum <- prepare_data()
dt.asylum.aggregated <- aggregate_data()
# dt.asylum$Country.of.origin <- as.character(dt.asylum$Country.of.origin)
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
      , menuItem("Network Characteristics", tabName = "characteristics", icon = icon("globe"))
      , menuItem("Network Exploration", tabName = "network_exploration", icon = icon("code"))
      , menuItem("Network Prediction", tabName = "network_prediction", icon = icon("list-alt"))
      , menuItem("About", tabName = "about", icon = icon("info-circle"))
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
      tabItem(tabName = "characteristics",
              fluidRow(
                column(9, uiOutput("introduction")),
              ),
              fluidRow(
                column(4, pickerInput("origin", "Country of origin", choices=unique(dt.asylum$Country.of.origin), options = list(actions_box = TRUE), selected=NULL, multiple=FALSE)),
                column(4, pickerInput("Year_input", "Year", choices=unique(dt.asylum$Year)[order(unique(dt.asylum$Year))], options = list(actions_box = TRUE), selected=2017, multiple=FALSE)),
                column(4, pickerInput("income_level", "Income Level", choices=c("All levels", "Low income", "Lower middle income", "Upper middle income", "High income"), options = list(actions_box = TRUE), selected="all", multiple=FALSE))),
                # column(4, uiOutput("asylum.income.selector"))
              
              
              fluidRow(
                column(3, uiOutput("info")),
                column(9,leafletOutput("mymap")),
                ),
          
      ),
      tabItem(tabName = "network_exploration",
              fluidRow(
                column(9, uiOutput("introduction_cir")),
              ),
              fluidRow(
                column(4, selectInput("year", "Select year:", choices = as.character(seq(2000, 2022)))),
                column(3, actionButton("show_graph", "Show Graph")
              )),
              fluidRow(
                column(3, tableOutput("info_circle")),
                column(9, visNetworkOutput("circular_plot")),
              ),
              fluidRow(
                column(3, uiOutput("description_cen")),
                column(9, tableOutput("betweenness")),
              ),
              
              fluidRow(
                column(4, radioButtons("col", "Choose a column:",
                                choices = c("betweenness", "closeness", "eigenvector"), selected = "betweenness")),
                column(4, tableOutput("statistics.circ"))
      )),
      tabItem(tabName = "network_prediction",
              fluidRow(
                column(9, uiOutput("introduction_pred")),
              ),
              fluidRow(
                column(width = 9,
                       leafletOutput("mymap_pred"),
                )
              ),
              
      ),
      tabItem(tabName = "about"
              
      )
    
    )
  )
)


# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1080))