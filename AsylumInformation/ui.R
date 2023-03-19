source("server.R")

dt.asylum <- prepare.data()
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

      menuItem("Descriptive Analysis", tabName = "analysis", icon = icon("chart-bar"),
               menuSubItem("Overview Analysis", tabName = "analysis"),
               menuSubItem("Regional Analysis", tabName = "region_analysis"))
      , menuItem("Network Exploration", tabName = "network_exploration", icon = icon("code"))
      , menuItem("Network Characteristics", tabName = "characteristics", icon = icon("globe"),
                 menuSubItem("Origin", tabName = "origin_graph", icon = icon("angle-right")),
                 menuSubItem("Asylum", tabName = "asylum_graph", icon = icon("angle-right")))
      , menuItem("Network Prediction", tabName = "network_prediction", icon = icon("list-alt"))
      , menuItem("About", tabName = "about", icon = icon("info-circle")
    )
  )),
  
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
                  column(12, uiOutput("introduction_regional"))
                ),
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
                           textOutput("totalRejected") %>% 
                             tagAppendAttributes(class = 'kpi')
                         )),
                  column(4,
                         wellPanel(
                           h3("Percentage of Rejected Decisions") %>% 
                             tagAppendAttributes(class = 'kpi-head'),
                           textOutput("rejectedPercent") %>% 
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
                  column(12, HTML(
                    "<p> &nbsp; &nbsp; &nbsp; This overview includes valuable 
                    statistical analysis from 2000 to 2022.</p>"))
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
                                   column(6, plotOutput("total.asylum", 
                                                        width = "100%")),
                                   column(6, HTML("
        <h3><strong>Description of Total Asylum</strong></h3><p 
        style='text-align: justify;'>This graph shows the top 5 
        countries in terms of asylum decisions, categorised by 
        the variable '<strong><em>country of asylum</em></strong>', 
        which refers to the host or asylum country.</p><p style='text-align: 
        justify;'>Surprisingly, despite having a much larger population, the US
        is only in second place, with Germany having the most asylum decisions 
        at 4.3 million. However, it's important to note that since 2015, the 
        Syrian war and other conflicts in the Middle East have led to a 
        significant influx of refugees into the EU, particularly France 
        and the UK. The remarkable capacity to host refugees and its 
        impact on society as a whole should therefore be highlighted.</p>"))
                                 )
                        ),
                        tabPanel("Total Origin",
                                 fluidRow(
                                   column(6, plotOutput("total.origin", 
                                                        width = "100%")),
                                   column(6, HTML('
  <h3><strong>Description of Total Origin</strong></h3>
  <p>This graph shows the top 5 countries of origin 
  <em><strong>"Country.of.origin"</strong></em>. 
  As logically indicated in the first graph <strong>(Total Asylum)</strong>, 
  we have most refugees from countries in the Middle East. 
  <strong>Afghanistan</strong> tops the list, followed by 
  <strong>Syria</strong> and <strong>Iraq</strong>. 
  This is mainly due to the conflict in Afghanistan, 
  which has been going on since 2001. Syria has been ravaged by war since 2011, 
  which is reflected in the data and leads to a flow of refugees into Central 
  European countries. The war in Iraq since 2003 and its massive refugee flows 
  are also reflected in the data. The war in <strong>Serbia and Kosovo</strong>, 
  which is the only European country in this Top 5 list, is particularly 
  noteworthy. Finally, it is important to talk about the 
  <strong>"Unknown"</strong> countries. These are stateless persons or persons 
  whose country of origin has not been registered.</p>
  <p style="text-align: justify;">&nbsp;</p>
')
                                          
                                   )
                                 )

                        ),
                        tabPanel("Total Rejection",
                                 fluidRow(
                                   column(6, plotOutput("total.rejection", 
                                                        width = "100%")),
                                   column(6, HTML(
      '<h3><strong>Description of Total Rejections</strong></h3>
      <p>From the graph we can observe the <strong>Top 5 asylum countries 
      with the most rejected applications</strong> in total numbers. 
      It is striking that <strong>France has significantly more rejections than 
      Germany</strong>, even though France has accepted significantly fewer 
      refugees in total <strong>(2.4M vs. 4.3M)</strong>. 
      This suggests that <strong>Germany has a much lower rejection rate 
      and a much more liberal refugee policy</strong>. 
      The exact rejection rate for each 
      country can be obtained from the rejection map underneath.&nbsp;</p>
      <p style="text-align: justify;">&nbsp;</p>')
                                          
                                   )
                                 )

                        ),
                        tabPanel("Total Rejection Rate",
                                 fluidRow(
                                   column(6, plotOutput("total.rejection.rate", 
                                                        width = "100%")),
                                   column(6,
      HTML('<h3><strong>Description of Total Rejection Rate</strong></h3>
      <p>From the graph of the top 5 total rejections, we can determine 
      the relative rejection of applications. <strong>Aruba</strong> tops the 
      list with a <strong>rejection rate of 100%</strong>, but other island 
      states also have what at first glance appears to be an extremely high 
      rejection rate. However, this is due to the fact that these island 
      states have a very low total number of decisions <strong>(e.g. Micronesia 
      with only 54 total decisions)</strong> and tend to be very remote and far 
      from potential trouble spots. However, <strong>Japan\'s</strong> extremely 
      restrictive refugee policy is worth noting, with a <strong>rejection rate 
      of 77.7%</strong>. This restrictive policy has been known for some time, 
      but it is now <strong>facing increasing pressure from human rights 
      organisations</strong> to offer refuge to more asylum seekers and 
      refugees - but instead the Japanese parliament is 
      considering new legislation to make its strict policy even stricter.</p>
      <p style="text-align: justify;">&nbsp;</p>')
                                          
                                   )
                                 )

                        ),
                        tabPanel("Decisions by Income",
                                 fluidRow(
                                   column(6, plotOutput("decisions.by.income", 
                                                        width = "100%")),
                                   column(6,
                                          HTML('
                  <h3><strong>Description of Decisions by Income</strong></h3>
                  <p>This pie chart shows which country categories 
                  (according to UN income classification) shoulder the 
                  <strong>greatest burden of refugee flows</strong>. With an 
                  overwhelming majority of <strong>72%, the highest income 
                  countries bear the most burden</strong>. However, it is 
                  important to keep in mind that often poor neighboring 
                  countries of conflict countries bear most of the weight, 
                  but no official asylum application is received, or an 
                  application is not recorded and therefore does not appear 
                  in this dataset.</p>
                  <p><strong>The income levels are defined as 
                  follows:</strong></p>
                  <table style="border-collapse: collapse; width: 100%; 
                  height: 90px;" border="1">
                  <tbody>
                  <tr style="height: 18px;">
                  <td style="width: 50%; 
                  height: 18px;"><strong>Group</strong></td>
                  <td style="width: 50%; 
                  height: 18px;"><strong>GNI/capita for FY2023</strong></td>
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
                        column(12, HTML("
                  <h3><strong>Description of Rejections Map</strong></h3>")),
                        column(8, HTML(
                  "<p style='text-align: justify;'>The <strong>interactive map 
                  of rejections</strong> shows the <strong>rejection rate, the 
                  total number of decisions and the total number of rejections 
                  for each country</strong> included in the dataset.</p>")),
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
                column(12, DT::dataTableOutput("betweenness")),
              ),
              fluidRow(

                column(4, HTML("<br>", "<br>"))

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
      tabItem(tabName = "origin_graph",
              fluidRow(
                column(9, uiOutput("introduction")),
              ),
              fluidRow(
                column(4, pickerInput("origin", "Country of origin", choices = unique(dt.asylum$Country.of.origin[dt.asylum$Country.of.origin != "Unknown"]), options = list(actions_box = TRUE), selected="Afghanistan", multiple=FALSE)),
                column(4, pickerInput("Year_input", "Year", choices=unique(dt.asylum$Year)[order(unique(dt.asylum$Year))], options = list(actions_box = TRUE), selected=2000, multiple=FALSE)),
                column(4, pickerInput("income.level", "Income Level", choices=c("All levels", "Low income", "Lower middle income", "Upper middle income", "High income"), options = list(actions_box = TRUE), selected="all", multiple=FALSE))),
              fluidRow(
                column(3, uiOutput("statistics.origin")),
                column(9,leafletOutput("map.origin")),
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
                column(4, pickerInput("income.level_asyl", "Income Level", choices=c("All levels", "Low income", "Lower middle income", "Upper middle income", "High income"), options = list(actions_box = TRUE), selected="all", multiple=FALSE))),
              fluidRow(
                column(3, uiOutput("statistics.asylum")),
                column(9,leafletOutput("map.asylum")),
              ),
      ),
      
      
      tabItem(tabName = "network_prediction",
              fluidRow(
                column(12, uiOutput("introduction_pred"))
              ),
              fluidRow(
                column(4, pickerInput("country", "Country", choices = unique(dt.asylum$Country.of.origin[dt.asylum$Country.of.origin != "Unknown"]), options = list(actions_box = TRUE), selected="Afghanistan", multiple=FALSE)),
                column(4, pickerInput("in_out", "Asylum or origin", choices=c("Asylum", "Origin"), options = list(actions_box = TRUE), selected="Asylum", multiple=FALSE)),
              ),
              fluidRow(
                column(9, leafletOutput("map.origin_pred")),
                column(3, uiOutput("pred_info"))
              ),
              fluidRow(
                column(12, uiOutput("description_pred"))
              )
      ),
      tabItem(
        tabName = "about",
        tags$div(
          class = "about-page",
          tags$style("
          .about-text p {
            margin-bottom: 10px;
            font-size: 18px;
            padding: 5px 0;
          }
          
          .about-text {
            border: 1px solid #ddd;
            border-radius: 5px;
            padding: 10px;
            background-color: #fff;
            line-height: 1.8;
          }
          
          .about-page {
            padding: 50px 0;
            display: flex;
            flex-direction: column;
            align-items: center;
            background-color: #f5f5f5;
            margin: 0 20px;
          }
          
          h2 {
            color: #333;
            padding: 10px 0;
            margin-top: 30px;
            margin-bottom: 10px;
          }
          div {
          font-size: 16px;
          line-height: 1.4;
          }


    "),
          tags$div(
            class = "about-text",
            fluidRow(
              column(12, HTML('
                  <p><img src=
                  "https://www.unhcr.org/innovation/wp-content/uploads/2019/07/unhcr-logo-horizontal.svg" 
                  alt="" width="254" height="61" /></p>
                  <h2><strong>Introduction to the UNHCR Refugee Data ShinyApp</strong></h2>')),
              column(8, HTML('
<p>Welcome to our UNHCR Refugee Data ShinyApp, a sophisticated
and comprehensive tool designed to provide valuable insight and analysis into
global refugee data. This ShinyApp has been carefully developed using R, a 
versatile and powerful statistical programming language, and is based on 
<a href="https://www.unhcr.org/refugee-statistics/download/?url=K6fWq2" 
target="_blank">data provided by the United Nations High Commissioner for 
Refugees (UNHCR) from 2000 to 2022</a>. Our primary goal is to facilitate 
a deeper understanding of the intricate web of relationships between 
countries of origin and countries of asylum. We aim to identify trends and 
patterns in refugee migration and provide essential information that can guide 
policy decisions and enhance efforts to support refugees worldwide. Within this 
ShinyApp, we have carefully organised the data into different sections, each 
focusing on a different aspect of refugee migration.</p>
<h2><strong>Descriptive Analysis</strong></h2>
<p>The <strong>Overview Analysis</strong> section provides a thorough overview 
of the data, highlighting key findings in several areas such as total asylum, 
total origin, total rejections, total rejection rate and decisions by income. 
These analyses highlight the important role played by countries such as Germany 
and the United States in hosting refugees, the impact of conflicts in the Middle 
East on refugee migration patterns, and the distribution of refugee hosting 
responsibility across income groups as defined by the United Nations.</p>
<p>In addition, the <strong>Regional Analysis</strong> section allows users 
to examine trends and patterns in decision-making by applying filters based on 
various factors such as asylum seeker income group, origin income group, origin 
region and asylum region. This section also provides key performance indicators, 
including the total number of decisions granted, the total number of decisions 
refused and the percentage of decisions refused.</p>
<h2><strong>Network Exploration</strong></h2>
<p>In the <strong>Network Exploration</strong> section, users are presented 
with an extensive network graph showing patterns of asylum claims around the world. 
This interactive graph can be filtered by year, country and group to observe changes 
in patterns over time. To facilitate a better understanding of the role of specific 
countries within the network, this section presents several network analysis measures, 
such as betweenness, eigenvector and closeness centrality.</p>
<h2><strong>Network Characteristics</strong></h2>
<p>In addition, the <strong>Network Characteristics</strong> sections for both 
countries of origin and countries of asylum provide users with detailed network graphs 
showing the connections between specific countries of origin and their countries of asylum,
or vice versa. These visualisations allow users to gain insight into the relationships between 
specific countries, help identify popular asylum countries and uncover potential reasons behind 
these patterns.</p>
<h2><strong>Network Prediction</strong></h2>
<p>Finally, the <strong>Network Prediction</strong> section includes a Jaccard Index calculation 
to show the similarity between nations based on their refugee flows. This measure can help 
identify areas where international cooperation and collaboration may be beneficial in 
addressing refugee-related issues.</p>
<h2><strong>Gain a deeper insight!</strong></h2>
<p>We invite you to explore the various tabs and features of this ShinyApp to gain a 
deeper understanding of global refugee migration patterns and the complex relationships 
between countries. By providing this insight, we hope to contribute to ongoing efforts to 
address the challenges faced by refugees and the countries involved in this humanitarian crisis.</p>
<p><strong>Credits</strong></p>
<p>For the complete network graph in the menu tab <strong>Network Exploration</strong> we 
based our code on the idea of Philip Ohlsson, find his Git Hub 
<a href="https://github.com/philipohlsson/refugee_data_UNHCR" target="_blank">here</a></p>')),
      )
    )
  )
)
)
)
)


# Run the app
shinyApp(ui = ui, server = server, options = list(height = 1440))