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
      , menuItem("Network Exploration", tabName = "network_exploration", icon = icon("code"))
      , menuItem("Network Characteristics", tabName = "characteristics", icon = icon("globe"),
                 menuSubItem("Origin", tabName = "origin_graph", icon = icon("angle-right")),
                 menuSubItem("Asylum", tabName = "asylum_graph", icon = icon("angle-right")))
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
      .tab-content > .tab-pane {
        display: none;
      }
      .tab-content > .active {
        display: block;
      }
    ")),
                fluidRow(
                  column(12, uiOutput("introduction_descriptives"))
                ),
                fluidRow(
                  column(12, HTML("<p> &nbsp; &nbsp; &nbsp; This overview includes valuable statistical analysis from 2000 to 2022.</p>"))
                ),
                tags$div(
                  class = "container-fluid",
                  tags$div(
                    class = "row",
                    tags$div(
                      class = "col-sm-12",
                      tabsetPanel(
                        tabPanel("Total Asylum",
                                 fluidRow(
                                   column(6, plotOutput("total.asylum", width = "100%")),
                                   column(6, HTML("<h3><strong>Description of Total Asylum</strong></h3><p style='text-align: justify;'>This graph shows the top 5 countries in terms of asylum decisions, categorised by the variable '<strong><em>country of asylum</em></strong>', which refers to the host or asylum country.</p><p style='text-align: justify;'>Surprisingly, despite having a much larger population, the US is only in second place, with Germany having the most asylum decisions at 4.3 million. However, it's important to note that since 2015, the Syrian war and other conflicts in the Middle East have led to a significant influx of refugees into the EU, particularly France and the UK. The remarkable capacity to host refugees and its impact on society as a whole should therefore be highlighted.</p>"))
                                 )
                        ),
                        tabPanel("Total Origin",
                                 fluidRow(
                                   column(6, plotOutput("total.origin", width = "100%")),
                                   column(6, 
                                          HTML('
        <h3><strong>Description of Total Origin</strong></h3>
        <p>This graph shows the top 5 countries of origin <em><strong>"Country.of.origin"</strong></em>. As logically indicated in the first graph <strong>(Total Asylum)</strong>, we have most refugees from countries in the Middle East. <strong>Afghanistan</strong> tops the list, followed by <strong>Syria</strong> and <strong>Iraq</strong>. This is mainly due to the conflict in Afghanistan, which has been going on since 2001. Syria has been ravaged by war since 2011, which is reflected in the data and leads to a flow of refugees into Central European countries. The war in Iraq since 2003 and its massive refugee flows are also reflected in the data. The war in <strong>Serbia and Kosovo</strong>, which is the only European country in this Top 5 list, is particularly noteworthy. Finally, it is important to talk about the <strong>"Unknown"</strong> countries. These are stateless persons or persons whose country of origin has not been registered.</p>
        <p style="text-align: justify;">&nbsp;</p>
      ')
                                   )
                                 )
                                 
                        ),
                        tabPanel("Total Rejection",
                                 fluidRow(
                                   column(6, plotOutput("total.rejection", width = "100%")),
                                   column(6, 
                                          HTML('<h3><strong>Description of Total Rejections</strong></h3>
      <p>From the graph we can observe the <strong>Top 5 asylum countries with the most rejected applications</strong> in total numbers. It is striking that <strong>France has significantly more rejections than Germany</strong>, even though France has accepted significantly fewer refugees in total <strong>(2.4M vs. 4.3M)</strong>. This suggests that <strong>Germany has a much lower rejection rate and a much more liberal refugee policy</strong>. The exact rejection rate for each country can be obtained from the rejection map underneath.&nbsp;</p>
      <p style="text-align: justify;">&nbsp;</p>')
                                   )
                                 )
                                 
                        ),
                        tabPanel("Total Rejection Rate",
                                 fluidRow(
                                   column(6, plotOutput("total.rejection.rate", width = "100%")),
                                   column(6, 
                                          HTML('<h3><strong>Description of Total Rejection Rate</strong></h3>
            <p>From the graph of the top 5 total rejections, we can determine the relative rejection of applications. <strong>Aruba</strong> tops the list with a <strong>rejection rate of 100%</strong>, but other island states also have what at first glance appears to be an extremely high rejection rate. However, this is due to the fact that these island states have a very low total number of decisions <strong>(e.g. Micronesia with only 54 total decisions)</strong> and tend to be very remote and far from potential trouble spots. However, <strong>Japan\'s</strong> extremely restrictive refugee policy is worth noting, with a <strong>rejection rate of 77.7%</strong>. This restrictive policy has been known for some time, but it is now <strong>facing increasing pressure from human rights organisations</strong> to offer refuge to more asylum seekers and refugees - but instead the Japanese parliament is considering new legislation to make its strict policy even stricter.</p>
            <p style="text-align: justify;">&nbsp;</p>')
                                   )
                                 )
                                 
                        ),
                        tabPanel("Decisions by Income",
                                 fluidRow(
                                   column(6, plotOutput("decisions.by.income", width = "100%")),
                                   column(6, 
                                          HTML('<h3><strong>Description of Decisions by Income</strong></h3>
                  <p>This pie chart shows which country categories (according to UN income classification) shoulder the <strong>greatest burden of refugee flows</strong>. With an overwhelming majority of <strong>72%, the highest income countries bear the most burden</strong>. However, it is important to keep in mind that often poor neighboring countries of conflict countries bear most of the weight, but no official asylum application is received, or an application is not recorded and therefore does not appear in this dataset.</p>
                  <p><strong>The income levels are defined as follows:</strong></p>
                  <table style="border-collapse: collapse; width: 100%; height: 90px;" border="1">
                  <tbody>
                  <tr style="height: 18px;">
                  <td style="width: 50%; height: 18px;"><strong>Group</strong></td>
                  <td style="width: 50%; height: 18px;"><strong>GNI/capita for FY2023</strong></td>
                  </tr>
                  <tr style="height: 18px;">
                  <td style="width: 50%; height: 18px;">Low Income</td>
                  <td style="width: 50%; height: 18px;">&lt; 1,085</td>
                  </tr>
                  <tr style="height: 18px;">
                  <td style="width: 50%; height: 18px;">Lower-middle income</td>
                  <td style="width: 50%; height: 18px;">1,086 - 4,255</td>
                  </tr>
                  <tr style="height: 18px;">
                  <td style="width: 50%; height: 18px;">Upper-middle income</td>
                  <td style="width: 50%; height: 18px;">4,256 - 13,205</td>
                  </tr>
                  <tr style="height: 18px;">
                  <td style="width: 50%; height: 18px;">High income</td>
                  <td style="width: 50%; height: 18px;">&gt; 13,205</td>
                  </tr>
                  </tbody>
                  </table>
                  <p style="text-align: justify;">&nbsp;</p>'))
                                 )
                                 
                        ),
                        selected = "Total Asylum"
                      )
                    )
                  ),
                  tags$div(
                    class = "row",
                    tags$div(
                      class = "col-sm-12",
                      fluidRow(
                        column(12, HTML("<h3><strong>Description of Rejections Map</strong></h3>")),
                        column(8, HTML("<p style='text-align: justify;'>The <strong>interactive map of rejections</strong> shows the <strong>rejection rate, the total number of decisions and the total number of rejections for each country</strong> included in the dataset.</p>")),
                        column(12, leafletOutput("rejections.map"))
                      )
                    )
                  )
                )
              )
              
              
      ),
      
      tabItem(tabName = "network_exploration",
              fluidRow(
                column(8, uiOutput("header.cir")),
              ),
              fluidRow(
                column(12, tableOutput("info.circle"))
              ),
              fluidRow(
                column(2, selectInput("year", "Select year:", choices = as.character(seq(2000, 2022)))),
              ),
              fluidRow(
                column(3, uiOutput("introduction.cir")),
                column(9, visNetworkOutput("circular.plot")),
              ),
              fluidRow(
                column(6, HTML("<br><h2><strong>Details into Centrality Measures</strong></h2><br>"))
              ),
              fluidRow(
                column(12, tableOutput("betweenness")),
              ),
              
              fluidRow(
                column(4, uiOutput("description.between")),
                column(4, uiOutput("description.eigen")),
                column(4, uiOutput("description.close")),
              ),
              fluidRow(
                column(3, radioButtons("col", "Choose a column:",
                              choices = c("betweenness", "closeness", "eigenvector"), selected = "betweenness")),
                column(3, tableOutput("statistics.circ"))),
              fluidRow(
                column(6, uiOutput("groups_circ_graph"))
              )
      ),
      
      tabItem(tabName = "network_prediction",
              fluidRow(
                column(9, uiOutput("introduction_pred")),
              ),
              fluidRow(
                column(3, pickerInput("asylum", "Country of asylum", choices = unique(dt.asylum$Country.of.asylum[dt.asylum$Country.of.asylum != "Unknown"]), options = list(actions_box = TRUE), selected="Germany", multiple=FALSE)),
                column(width = 9,
                       leafletOutput("mymap_pred"))
              ),
      ),
      tabItem(tabName = "origin_graph",
              fluidRow(
                column(9, uiOutput("introduction")),
              ),
              fluidRow(
                column(4, pickerInput("origin", "Country of origin", choices = unique(dt.asylum$Country.of.origin[dt.asylum$Country.of.origin != "Unknown"]), options = list(actions_box = TRUE), selected="Afghanistan", multiple=FALSE)),
                column(4, pickerInput("Year_input", "Year", choices=unique(dt.asylum$Year)[order(unique(dt.asylum$Year))], options = list(actions_box = TRUE), selected=2000, multiple=FALSE)),
                column(4, pickerInput("income_level", "Income Level", choices=c("All levels", "Low income", "Lower middle income", "Upper middle income", "High income"), options = list(actions_box = TRUE), selected="all", multiple=FALSE))),
              fluidRow(
                column(3, uiOutput("statistics.origin")),
                column(9,leafletOutput("mymap")),
              ),
      ),
      tabItem(tabName = "asylum_graph",
              fluidRow(
                column(9, uiOutput("introduction.asylum")),
              ),
              fluidRow(
                column(4, pickerInput("asylum_1", "Country of asylum", sorted_choices <- sort(unique(dt.asylum$Country.of.asylum[dt.asylum$Country.of.asylum != "Unknown"]))
                        , options = list(actions_box = TRUE), selected="Germany", multiple=FALSE)),
                column(4, pickerInput("Year_input_asyl", "Year", choices=unique(dt.asylum$Year)[order(unique(dt.asylum$Year))], options = list(actions_box = TRUE), selected=2000, multiple=FALSE)),
                column(4, pickerInput("income_level_asyl", "Income Level", choices=c("All levels", "Low income", "Lower middle income", "Upper middle income", "High income"), options = list(actions_box = TRUE), selected="all", multiple=FALSE))),
              fluidRow(
                column(3, uiOutput("statistics.asylum")),
                column(9,leafletOutput("mymap.asylum")),
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
              h6("Overall, descriptive analysis is a powerful tool for understanding and summarizing complex datasets, providing valuable insights that can inform a wide range of applications."),
              h4("Credits"),
              h6("For the complete network graph in the menu tab \"network exploration\" we based our code on the idea of Philip Ohlsson, find his Git Hub here: https://github.com/philipohlsson/refugee_data_UNHCR")
              
      )
      
    )
  )
)


# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1080))