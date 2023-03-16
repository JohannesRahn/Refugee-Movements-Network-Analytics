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
              fluidRow(
                column(12, uiOutput("introduction_descriptives"))
              ),
              fluidRow(
                column(4, plotOutput("total.asylum")),
                column(8, div("Random Title 1", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;"))
              ),
              fluidRow(
                column(4, plotOutput("total.origin")),
                column(8, div("Random Title 2", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 4
              ),
              fluidRow(
                column(4, plotOutput("total.rejection")),
                column(8, div("Random Title 3", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 8
              ),
              fluidRow(
                column(4, plotOutput("total.rejection.rate")),
                column(8, div("Random Title 4", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;"))
              ),
              fluidRow(
                column(4, plotOutput("decisions.by.income")),
                column(8, div("Random Title 5", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 4
              ),
              fluidRow(
                column(12, leafletOutput("rejections.map"), width = 12)
              ),
              fluidRow(
                column(8, plotOutput("total.decisions.plot"), width = 8),
                column(4, div("Random Title 6", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 0
              ),
              fluidRow(
                column(8, plotOutput("recognized.decisions.plot"), width = 8),
                column(4, div("Random Title 7", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 0
              ),
              fluidRow(
                column(8, plotOutput("rejected.decisions.plot"), width = 8),
                column(4, div("Random Title 8", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 0
              ),
              fluidRow(
                column(8, plotOutput("otherwise.closed.decisions.plot"), width = 8),
                column(4, div("Random Title 9", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 0
              ),
              fluidRow(
                column(12, plotOutput("total.closed.decisions.plot"), width = 12),
                column(12, div("Random Title 10", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 0
              )

      ),
      tabItem(tabName = "characteristics",
              fluidRow(
                column(9, uiOutput("introduction")),
              ),
              fluidRow(
                column(4, pickerInput("origin", "Country of origin", choices = unique(dt.asylum$Country.of.origin[dt.asylum$Country.of.origin != "Unknown"]), options = list(actions_box = TRUE), selected="Afghanistan", multiple=FALSE)),
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
      tabItem(tabName = "about",
              uiOutput("about"),
              h6("We are conducting the number of refugees who are seeking for asylum around the world. Our primary purpose is to better protect and help these refugees. Our data is generated from UNHCR (the United High Commissioner for Refugees).UNHCR is a global organization dedicated to saving lives, protecting rights and building a better future for refugees, forcibly displaced communities and stateless people."),
      h2("What We Do"),
      h6("Base on these data we can get a grasp of how many refugees are there around the world? We can track the number of people forced to flee and use data and statistics to inform and optimize our work and the work of our partners to better protect, assist and provide solutions. So when for example a major displacement crisis erupts, we can predict how many people need help, what kind of help they need and how many staff we must deploy."),
      h6("For statistics and operational data that are essential for UNHCR operations, we collect and process data in a number of different systems that are fit for purpose. Our Population Statistics Database, for example, carries information on country of asylum, country of origin and demographics on people of concern to UNHCR – refugees, asylum seekers, returned refugees, internally displaced and stateless people."),
      h6("We work with data and statistics, which are helping us to understand important information to save, protect and improve the lives of refugees, other forcibly displaced and stateless people. Based on good data, we can make informed decisions around our work and better plan for future operations. Data also allows us to demonstrate accountability to beneficiaries, governments, partners and donors in a tangible and comparable way."),
      h6("The collection and use of refugee data are mandated by the 1951 Refugee Convention and by the Statute of the Office of the High Commissioner for Refugees. The confidentiality of refugee data and related information is highly respected by UNHCR and our partners and the processing and protection of personal data are anchored in UNHCR’s Data Protection Policy."),
      h2("History Of UNHCR"),
      h6("The office of the United Nations High Commissioner for Refugees (UNHCR) was created in 1950, during the aftermath of the Second World War, to help millions of Europeans who had fled or lost their homes. We had three years to complete our work and then disband."),
      h6("In 1954, UNHCR won the Nobel Peace Prize for its groundbreaking work in Europe. But it was not long before we faced our next major emergency."),
      h6("In 1956, during the Hungarian Revolution, 200,000 fled to neighbouring Austria. Recognizing the Hungarians as 'prima facie' refugees, UNHCR led efforts to resettle them. This uprising and its aftermath shaped the way humanitarian organizations would deal with refugee crises in the future."),
      h6("During the 1960s, the decolonization of Africa produced the first of that continent’s numerous refugee crises. We also helped uprooted people in Asia and Latin America over the following two decades. In 1981, we received a second Nobel Peace Prize for what had become worldwide assistance to refugees."),
      h6("The start of the 21st century has seen UNHCR help with major refugee crises in Africa, the Middle East and Asia. We have also been asked to use our expertise to help many internally displaced by conflict and expanded our role in helping stateless people. In some parts of the world, such as Africa and Latin America, the 1951 Refugee Convention has been strengthened by additional regional legal instruments."),
      h6("UNHCR now has more than 18,879 personnel working in 137 countries. Our budget, which in its first year was US$300,000, grew to US$8.6 billion in 2019. In 2020, we marked our 70th anniversary. During our lifetime, we have helped well over 50 million refugees to successfully restart their lives."), h2("Who Do We Help"),
      h6("Our primary purpose is to safeguard the rights and well-being of people who have been forced to flee. Together with partners and communities, we work to ensure that everybody has the right to seek asylum and find safe refuge in another country. We also strive to secure lasting solutions."),
      h6("They include refugees, returnees, stateless people, the internally displaced and asylum-seekers. Our protection, shelter, health and education has been crucial, healing broken pasts and building brighter futures."),
      h2("Descriptive Analysis"),
      h6("Descriptive analysis is a statistical method used to summarize and describe the main features of a dataset. It involves analyzing the data using various statistical measures, such as mean, median, mode, range, standard deviation, and variance, to provide a comprehensive picture of the dataset."),
      h6("Descriptive analysis is often the first step in data analysis and is used to explore and understand the characteristics of the data. It helps to identify patterns, trends, and relationships between variables, as well as any outliers or unusual observations."),
      h6("Descriptive analysis can be used in many fields, including finance, marketing, healthcare, and social sciences, among others. It can be used to summarize data on sales, customer behavior, patient outcomes, and more, to aid decision-making and inform policy."),
      h6("Overall, descriptive analysis is a powerful tool for understanding and summarizing complex datasets, providing valuable insights that can inform a wide range of applications.")
              
      )
    
    )
  )
)


# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1080))
