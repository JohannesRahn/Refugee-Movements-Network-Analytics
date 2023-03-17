source("server.R")
dt.asylum <- prepare_data()
#dt.asylum.aggregated <- aggregate_data(dt.asylum)
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
      , menuItem("Regional Analysis", tabName = "region_analysis", 
                       icon = icon("map-pin"))
      , menuItem("Network Characteristics", tabName = "characteristics", icon = icon("globe"))
      , menuItem("Network Exploration", tabName = "network_exploration", icon = icon("code"))
      , menuItem("Network Prediction", tabName = "network_prediction", icon = icon("list-alt"))
      , menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "region_analysis",
              # Creating styling for the plots
              fluidPage(
                tags$style(HTML("
    .well {
      background-color: #f5f5f5;
      border-radius: 3px;
      padding: 15px;
      margin-bottom: 20px;
    }
    .kpi {
      font-size: 24px;
      font-weight: bold;
      text-align: center;
    }
    .kpi-head {
      text-align: center;
      font-weight: bold;
      margin-bottom: 20px;
    }
  ")),
                
                fluidRow(
                  column(4,
                         wellPanel(
                           h3("Total Recognized Decisions") %>% 
                             tagAppendAttributes(class = 'kpi-head'),
                           textOutput("totalRecognized") %>% 
                             tagAppendAttributes(class = 'kpi')
                         )),
                  column(4,
                         wellPanel(
                           h3("Total Rejected Decisions") %>% 
                             tagAppendAttributes(class = 'kpi-head'),
                           span(textOutput("totalRejected")) %>% 
                             tagAppendAttributes(class = 'kpi')
                         )),
                  column(4,
                         wellPanel(
                           h3("Percentage of Rejected Decisions") %>% 
                             tagAppendAttributes(class = 'kpi-head'),
                           span(textOutput("rejectedPercent")) %>% 
                             tagAppendAttributes(class = 'kpi')
                         ))
                ),
                fluidRow(
                  column(3,
                         selectInput("asylumIncomeGroupFilter",
                                     "Filter by Aslyum Income Group:",
                                     choices = c("No Filter", "Low income", "Lower middle income", "Upper middle income", "High income"),
                                     selected = "No Filter")),
                  column(3,
                         selectInput("originIncomeGroupFilter",
                                     "Filter by Origin Income Group:",
                                     choices = c("No Filter", "Low income", "Lower middle income", "Upper middle income", "High income"),
                                     selected = "No Filter")),
                  column(3,
                         selectInput("originRegionFilter",
                                     "Filter by Origin Region:",
                                     choices = c("No Filter", unique(na.omit(dt.asylum$Origin_Region))),
                                     selected = "No Filter")),
                  column(3,
                         selectInput("asylumRegionFilter",
                                     "Filter by Asylum Region:",
                                     choices = c("No Filter", unique(na.omit(dt.asylum$Asylum_Region))),
                                     selected = "No Filter"))
                ),
                fluidRow(
                  column(6, wellPanel(plotOutput("yearly.decisions.plot"))),
                  column(6, wellPanel(plotOutput("authority.decisions.plot")))
                  #column(6, wellPanel(plotOutput("recognized.decisions.plot")))
                ),
                fluidRow(
                  column(12, wellPanel(plotOutput("combined.decisions.plot")))
                )
                
                
              )
      ),
      tabItem(tabName = "analysis",
              fluidPage(
                tags$style(HTML("
    .well {
      background-color: #f5f5f5;
      border-radius: 3px;
      padding: 15px;
      margin-bottom: 20px;
    }
    .kpi {
      font-size: 24px;
      font-weight: bold;
      text-align: center;
    }
    .kpi-head {
      text-align: center;
      font-weight: bold;
      margin-bottom: 20px;
    }
  ")),
              fluidRow(
                column(12, uiOutput("introduction_descriptives"))
              ),
              fluidRow(
                column(4, plotOutput("total.asylum")),
                column(8, div("In this Graph, we showing the Top 5 countries of Asylum by the total decisions in 2022. The leading country is Germany with 4.3M, almost double compare to France with 2.4M, which is 3rd in the standing. A minor increase with 2.6M is from United State of America standing in Top 2. Following up are United Kindom with 1.8M and South Africa 1.5M. In conclude, the majority people who seeking asylum are most likely to flee to Europe or the US. The chance of seeking asylum is not high in Asia.", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;"))
              ),
              fluidRow(
                column(4, plotOutput("total.origin")),
                column(8, div("You can looking for the Top 5 countries of origin by this graph. There are Afghanistan (2M), Syrian Arab Republic (1.8M), Iraq (1.5M), Serbia & Kosovo (1M). Most of these countries are from Middle East region, they are likely to share the similarity. However there are 2M people with Unknown origins, this might be the result of some mistake while collecting data or they wanted to hide their country of origin to avoid troubles.", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 4
              ),
              fluidRow(
                column(4, plotOutput("total.rejection")),
                column(8, div("This graph showing the Top 5 countries with highest number of rejections. For example: France with 2.4M decisions but reject 1.7M, almost 70% will be reject, standing at the highest number of rejections. Similar to Germany with 1.6M rejections in 2022. This graph sharing the similarity with the first graph, the total rejection is positive proportion with the total number of decisions.", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 8
              ),
              fluidRow(
                column(4, plotOutput("total.rejection.rate")),
                column(8, div("This graph will show you the top 5 hardest countries to seek asylum. Standing at number 1 will be Aruba with 100% rejection rate, this is a very small country which is might be the reason. Similar to Micronesia (0.9), and Caymen Islands (0.8). These countries share the similar geographic characteristics, which is very small, and possibly with very low number of asylum as well. Same story with Japan with the rejection rate of 0.8", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;"))
              ),
              fluidRow(
                column(4, plotOutput("decisions.by.income")),
                column(8, div("For this Pie Graph, we are listing the total decisions by the income level. With 72% of the people seeking for high income countries, almost 3/4 of the total decisions. While only 17% seeking Upper Middle Income countries, and the rest equally divided between Lower Middle and Lower Income countries. This results confirmed that people are most likely to seeking asylum with a better, higher/advanced countries compare to poor countries, which is understandable.", style = "background-color: #f7f7f7; padding: 20px; border-radius: 5px;")), offset = 4
              ),
              fluidRow(
                column(12, leafletOutput("rejections.map"), width = 12)
              )
              )
      ),
      tabItem(tabName = "characteristics",
              fluidRow(
                column(9, uiOutput("introduction")),
              ),
              fluidRow(
                column(4, pickerInput("origin", "Country of origin", choices = unique(dt.asylum$Country.of.origin[dt.asylum$Country.of.origin != "Unknown"]), options = list(actions_box = TRUE), selected="Afghanistan", multiple=FALSE)),
                column(4, pickerInput("Year_input", "Year", choices=unique(dt.asylum$Year)[order(unique(dt.asylum$Year))], options = list(actions_box = TRUE), selected=2017, multiple=FALSE)),
                column(4, pickerInput("income_level", "Income Level", choices=c("All levels", "Low income", "Lower middle income", "Upper middle income", "High income"), options = list(actions_box = TRUE), selected="all", multiple=FALSE))
                ),
              # column(4, uiOutput("asylum.income.selector"))
              
              
              fluidRow(
                column(3, uiOutput("info")),
                column(9,leafletOutput("mymap")),
              ),
              h3("Description for Statistic Values"),
              h4("Number of Vertices"),
              h5("The number of vertices refers to the total number of nodes or entities in a network. These nodes can represent a variety of different entities, such as people, organizations, countries, depending on the context of the analysis.The number of vertices is an important characteristic of a network because it can affect many of its properties and behaviors. For example: As the number of vertices increases, the degree distribution tends to become more normally distributed, the clustering coefficient tends to decrease, indicating a less tightly knit network, and the diameter tends to increase as well, although the rate of increase may depend on the specific network structure."),
              h4("Number of Edges"),
              h5("The number of edges refers to the total number of connections between nodes in a network. In a network diagram, edges are represented as lines or arcs that connect pairs of nodes. It can provide important information about the network's structure, connectivity, and behavior. Some key property include:"),
              h5("Degree distribution: As the number of edges increases, the degree distribution may become more homogeneous, with fewer nodes having very high or very low degrees."),
              h5("Density: The number of edges increases, the density of the network tends to increase as well."),
              h5("Clustering coefficient: the number of edges increases, the clustering coefficient may increase as well, indicating a more tightly knit network"),
              h5("Path length: In general, as the number of edges increases, the average path length may decrease, indicating a more connected network."),
              h4("Average Path Length"),
              h5("The average path length is a measure of the average number of edges that must be traversed in order to travel from one node to another node in a network. Specifically, it is the average shortest path length between all pairs of nodes in the network. In general, nodes that are closer to each other are more likely to be connected or to influence each other in some way. The average path length can be influenced by a number of factors, including the number of nodes in the network, the number of edges, and the specific topology or structure of the network. The average path length is often used as a key measure of network efficiency or connectivity, as it provides insights into how easily information or influence can spread through the network. In conclude, networks with shorter average path lengths tend to be more efficient and more connected, and may be more robust to failures or disruptions."),
              h4("Average Clustering Coefficient"),
              h5("The average clustering coefficient is a measure of the degree to which nodes in a network tend to cluster together into tightly-knit local groups or neighborhoods. It is the average of the clustering coefficients of all nodes in the network. The clustering coefficient of a node measures the proportion of its neighbors that are also neighbors of each other. In other words, it quantifies how likely it is that a pair of nodes that are connected to a common node are also directly connected to each other. Nodes with high clustering coefficients tend to be part of dense local clusters or neighborhoods, while nodes with low clustering coefficients tend to be more isolated. In general, networks with higher average clustering coefficients tend to be more tightly-knit and more resistant to disruptions, as information or influence can be more easily propagated through local clusters. Conversely, networks with lower average clustering coefficients may be more vulnerable to failures or disruptions, as they lack strong local connections and rely more heavily on long-range connections."),
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
              #Add overview about our project
              h2("About Us"),
              h5("We are a group of students at NOVA SBE and we conducting the number of refugees who are seeking for asylum around the world. Our primary purpose is to better understanding what the data can provide therefore we can protect and help these refugees. Our data is generated from UNHCR (the United High Commissioner for Refugees).UNHCR is a global organization dedicated to saving lives, protecting rights and building a better future for refugees, forcibly displaced communities and stateless people."),
              h2("What We Do"),
              h5("Base on these data we can get a grasp of how many refugees are there around the world? We can track the number of people forced to flee and use data and statistics to inform and optimize our work and the work of our partners to better protect, assist and provide solutions. So when for example a major displacement crisis erupts, we can predict how many people need help, what kind of help they need and how many staff we must deploy."),
              h5("For statistics and operational data that are essential for UNHCR operations, we collect and process data in a number of different systems that are fit for purpose. Our Population Statistics Database, for example, carries information on country of asylum, country of origin and demographics on people of concern to UNHCR – refugees, asylum seekers, returned refugees, internally displaced and stateless people. We work with data and statistics, which are helping us to understand important information to save, protect and improve the lives of refugees, other forcibly displaced and stateless people. Based on good data, we can make informed decisions around our work and better plan for future operations. Data also allows us to demonstrate accountability to beneficiaries, governments, partners and donors in a tangible and comparable way."),
              h5("The collection and use of refugee data are mandated by the 1951 Refugee Convention and by the Statute of the Office of the High Commissioner for Refugees. The confidentiality of refugee data and related information is highly respected by UNHCR and our partners and the processing and protection of personal data are anchored in UNHCR’s Data Protection Policy."),
              h2("History Of UNHCR"),
              h5("The office of the United Nations High Commissioner for Refugees (UNHCR) was created in 1950, during the aftermath of the Second World War, to help millions of Europeans who had fled or lost their homes. We had three years to complete our work and then disband. In 1954, UNHCR won the Nobel Peace Prize for its groundbreaking work in Europe. But it was not long before we faced our next major emergency. In 1956, during the Hungarian Revolution, 200,000 fled to neighbouring Austria. Recognizing the Hungarians as 'prima facie' refugees, UNHCR led efforts to resettle them. This uprising and its aftermath shaped the way humanitarian organizations would deal with refugee crises in the future. During the 1960s, the decolonization of Africa produced the first of that continent’s numerous refugee crises. We also helped uprooted people in Asia and Latin America over the following two decades. In 1981, we received a second Nobel Peace Prize for what had become worldwide assistance to refugees."),
              h5("The start of the 21st century has seen UNHCR help with major refugee crises in Africa, the Middle East and Asia. We have also been asked to use our expertise to help many internally displaced by conflict and expanded our role in helping stateless people. In some parts of the world, such as Africa and Latin America, the 1951 Refugee Convention has been strengthened by additional regional legal instruments. UNHCR now has more than 18,879 personnel working in 137 countries. Our budget, which in its first year was US$300,000, grew to US$8.6 billion in 2019. In 2020, we marked our 70th anniversary. During our lifetime, we have helped well over 50 million refugees to successfully restart their lives."),
              h2("Who Do We Help"),
              h5("Our primary purpose is to safeguard the rights and well-being of people who have been forced to flee. Together with partners and communities, we work to ensure that everybody has the right to seek asylum and find safe refuge in another country. We also strive to secure lasting solutions. They include refugees, returnees, stateless people, the internally displaced and asylum-seekers. Our protection, shelter, health and education has been crucial, healing broken pasts and building brighter futures."),
              h2("Descriptive Analysis"),
              h5("Descriptive analysis is a statistical method used to summarize and describe the main features of a dataset. It involves analyzing the data using various statistical measures, such as mean, median, mode, range, standard deviation, and variance, to provide a comprehensive picture of the dataset. It is often the first step in data analysis and is used to explore and understand the characteristics of the data. It helps to identify patterns, trends, and relationships between variables, as well as any outliers or unusual observations. Descriptive analysis can be used in many fields, including finance, marketing, healthcare, and social sciences, among others. It can be used to summarize data on sales, customer behavior, patient outcomes, and more, to aid decision-making and inform policy."),
              h5("Overall, descriptive analysis is a powerful tool for understanding and summarizing complex datasets, providing valuable insights that can inform a wide range of applications."),
              h2("Network Exploration"),
              h5("Basically sum up the information that you can filter by year and select by group, when you zoom in of the graph, you can see at the out side ring are countries and how they connect to other countries. You can explore the betweeness, closeness, Eigenvector and lastly with the top 10 countries as well. You can see number of vertices, edges, average path length, average degree, and clustering coefficient. The goal of network exploration is to gain insights into the network, such as its topology, connectivity, centralities, communities, and dynamics. This information can be useful in a variety of applications, such as network design, optimization, prediction, and control."),
              h5("Some common tasks in network exploration include identifying important nodes and links, detecting communities or clusters, predicting network evolution or behavior, and visualizing the network structure and properties. By exploring and understanding networks, we can gain insights into the underlying systems they represent, and use this knowledge to inform decision-making and problem-solving in a variety of fields."),
              h2("Network Characteristics"),
              h5("On the network page you can find a network graph showing the connections between country of origin and the country of asylum. You can select the country of origin, the year and the income level for the country of asylum. You can also find the statistic values, such as number of vertices, edges, Diameter, average path lenghts, average degree, and clustering coefficient."),
              h2("Network Predicting"),
              h5("On this page you can find/predict countries with similar patterns on the Map. For example Germany sharing similarity with 8 other countries."),
              h2("Disclaimers"),
              h5("Because the data is base on Government reports, there are some of the data is Unknown/Error. Thus, some of the function for the filter will not show properly results!!!"),
              h5("Secondary, these data is base on the number of Decision by the Government, so in reality there are higher number of asylum seeking!!!")
      )
      
    )
  )
)


# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1080))