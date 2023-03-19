source("global.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dt.asylum <- prepare_data()
  #dt.aggregated.asylum <- aggregate_data(dt.asylum)
  g = graph.data.frame(dt.asylum, directed=TRUE)
  descriptives.reac <- descriptives()
  
  
  # Reactive element to change origin and asylum column as character
  dt.asylum.st <- reactive({
    dt <- prepare_data()
    dt$Country.of.origin <- as.character(dt$Country.of.origin)
    dt$Country.of.asylum <- as.character(dt$Country.of.asylum)
    dt
  })
    
  # Reactive element for circular graph
  graph_data <- reactive({
    # Calculate the graph based on the data
    circular_plot <- circular.graph(input$year)
    # Return the graph
    return(circular_plot)
  })  
  
  # Reactive element for descriptive filter:
  descriptive_data <- reactive({
    
    dt.descriptive <- filter_region_analysis(dt.asylum, 
                                             input$asylumIncomeGroupFilter, 
                                             input$originIncomeGroupFilter, 
                                             input$originRegionFilter, 
                                             input$asylumRegionFilter)
    
    # The descriptive analysis only needs aggregated data
    dt.aggregated.descriptive <- na.omit(aggregate_data(dt.descriptive))
    return(dt.aggregated.descriptive)
  })
  # Introduction for Descriptive section
  output$introduction_regional <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Regional Analysis", "</h1>", "<br>", 
    "<div>", "The tab presents various visualizations which allow you to 
               examine the trends and patterns in decision making. 
               You can apply filters to the data based on asylum income group, 
               origin income group, origin region, and asylum region. 
               Additionally, the tab provides key performance indicators 
               such as total recognized decisions, total rejected decisions, 
               and the percentage of rejected decisions.", "</div>", "<br>"))
    
  })
  # Reactive element for descriptive filter:
  authority_data <- reactive({
    
    dt.descriptive <- filter_region_analysis(dt.asylum, 
                                             input$asylumIncomeGroupFilter, 
                                             input$originIncomeGroupFilter, 
                                             input$originRegionFilter, 
                                             input$asylumRegionFilter)
    
    dt.grouped.authority <- dt.descriptive %>%
      group_by(Authority) %>%
      summarise(Total_decisions = sum(Total.decisions))
    # The descriptive analysis only needs aggregated data
    return(dt.grouped.authority)
  })
  
  check_dt_size_alert_alert <- function(dt) {
    
    if (nrow(na.omit(dt)) == 0) {
      shinyalert(title = "No Data Available!", 
                 text = "There is no data matching your criteria.", 
                 type = "error")
    }
  }
  
  filter_region_analysis <- function(dt.descriptive, asylumIncomeGroupFilter,
                                     originIncomeGroupFilter, 
                                     originRegionFilter, asylumRegionFilter){
    # Method to use the filter of the region analysis on a data table.
    if (input$asylumIncomeGroupFilter != "No Filter") {
      dt.descriptive <- dt.descriptive[dt.descriptive$Asylum_Income == asylumIncomeGroupFilter, ]
    }
    
    if (input$originIncomeGroupFilter != "No Filter") {
      dt.descriptive <- dt.descriptive[dt.descriptive$Origin_Income == originIncomeGroupFilter, ]
    }
    if (input$originRegionFilter != "No Filter") {
      dt.descriptive <- dt.descriptive[dt.descriptive$Origin_Region == originRegionFilter, ]
    }
    if (input$asylumRegionFilter != "No Filter") {
      dt.descriptive <- dt.descriptive[dt.descriptive$Asylum_Region == asylumRegionFilter, ]
    }
    return(dt.descriptive)
  }
  
  # Introduction for Descriptive section
  output$introduction_descriptives <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Descriptive Statistics", 
               "</h1>", "<br>"))
  })
  
  # Bar chart top 5 asylum countries
  output$total.asylum <- renderPlot({
    plot.bar1 <- descriptives.reac[[1]]
    plot.bar1
  })
  
  
  # Bar chart top 5 origin countries
  output$total.origin <- renderPlot({
    plot.bar2 <- descriptives.reac[[2]]
    plot.bar2
  })
  
  # Bar chart top 5 countries by rejection
  output$total.rejection <- renderPlot({
    plot.bar3 <- descriptives.reac[[3]]
    plot.bar3
  })
  

  # Bar chart with top 5 countries rejection rate
  output$total.rejection.rate <- renderPlot({
    plot.bar4 <- descriptives.reac[[4]]
    plot.bar4
  })
  
  
  # Pie chart with total decisions by income level
  output$decisions.by.income <- renderPlot({
    pie1 <- descriptives.reac[[5]]
    pie1
  })
  
  # World map with rejection rate
  output$rejections.map <- renderLeaflet({
    world.map <- descriptives.reac[[6]]
    world.map
  })
  
  
  custom_theme <- function() {
    theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 16),
        panel.grid.minor = element_line(color = "gray85"),
        panel.grid.major = element_line(color = "gray75"),
        panel.background = element_rect(fill = "#f5f5f5"),
        plot.background = element_rect(fill = "#f5f5f5")
      )
  }
  
  output$yearly.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Total_decisions)) +
      geom_line(color = "#1F78B4", size = 1.5, linetype = "solid") +
      labs(title = "Total Decisions per Year",
           x = "Year",
           y = "Total Decisions") + custom_theme()
  })
  
  output$recognized.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Recognized_decisions)) +
      geom_line(color = "#1F78B4", size = 1.5, linetype = "solid") +
      labs(title = "Recognized Decisions per Year",
           x = "Year",
           y = "Recognized Decisions") + custom_theme()
  })
  
  output$rejected.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Rejected_decisions)) +
      geom_line(color = "#E31A1C", size = 1.5, linetype = "solid") +
      labs(title = "Rejected Decisions per Year",
           x = "Year",
           y = "Rejected Decisions") + custom_theme()
  })
  
  output$otherwise.closed.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Otherwise_closed)) +
      geom_line(color = "#33A02C", size = 1.5, linetype = "solid") +
      labs(title = "Otherwise Closed Decisions per Year",
           x = "Year",
           y = "Otherwise Closed Decisions") + custom_theme()
  })
  
  output$total.closed.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Total_closed)) +
      geom_line(color = "#6A3D9A", size = 1.5, linetype = "solid") +
      labs(title = "Total Closed Decisions per Year",
           x = "Year",
           y = "Total Closed Decisions") + custom_theme()
  })
  
  output$combined.decisions.plot <- renderPlot({
    ggplot(descriptive_data()) +
      geom_line(aes(x = Year, y = Rejected_decisions, 
                    color = "Rejected Decisions"), 
                size = 1.5, linetype = "solid") +
      geom_line(aes(x = Year, 
                    y = Otherwise_closed, color = "Otherwise Closed Decisions"), 
                size = 1.5, linetype = "solid") +
      geom_line(aes(x = Year, 
                    y = Total_closed, color = "Total Closed Decisions"), 
                size = 1.5, linetype = "solid") +
      labs(title = "Closed Decisions per Year",
           x = "Year",
           y = "Decisions") +
      scale_color_manual(values = c("Rejected Decisions" = "#E31A1C", 
                                    "Otherwise Closed Decisions" = "#33A02C", 
                                    "Total Closed Decisions" = "#6A3D9A")) + custom_theme()
  })
  
  output$authority.decisions.plot <- renderPlot({
    ggplot(authority_data(), aes(x = Authority, y = Total_decisions)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Total Decisions by Asylum Authority",
         x = "Asylum Authority",
         y = "Total Decisions") +
    custom_theme()
  })
  
  output$totalRecognized <- renderText({
    totalRecognized <- sum(descriptive_data()$Recognized_decisions)
    formatC(totalRecognized, format = "d", big.mark = ",")
  })
  
  output$totalRejected <- renderText({
    totalRejected <- sum(descriptive_data()$Rejected_decisions)
    formatC(totalRejected, format = "d", big.mark = ",")
  })
  
  output$rejectedPercent <- renderText({
    rejected <- sum(descriptive_data()$Rejected_decisions)
    total <- sum(descriptive_data()$Total_decisions)
    
    percentage <- (rejected / total) * 100
    paste0(formatC(percentage, format = "f", digits = 2), "%")
  })

  # Introduction to network characteristics origin
  output$introduction <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Network Characteristics - Country of Origin", "</h1>", 
               "<br>", "On this page you can find a network graph showing the connections between a specific country of origin and its country of asylums in order to find out more information about how many people got asylum in which country in which year and what the income level of that country is. That helps to understand if people seek asylum in close countries or further away. You can select the country of origin, the year and the income level for the country of asylum", "<br>", "<br>"))
  })
  
  # World map with country of origin network
  output$mymap <- renderLeaflet({
    graph <- create.origin.graph(dt.asylum, input$origin, input$Year_input, input$income_level)
    vert <- graph$vert
    edges <- graph$edges
    g <- graph$g
    edges_lines <- graph$edges_lines
    
    edges_df <- SpatialLinesDataFrame(edges_lines, edges)
    
    pal <- colorNumeric(palette = "YlGn", domain = unique(edges$weight), n = 10)

    # Calculate quantiles and create labels
    quantiles <- quantile(edges$weight, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
    
    leaflet(vert) %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addCircleMarkers() %>% 
      addPolylines(data = edges_df, weight = 2, color = ~pal(weight)) %>%
      addLegend(pal = pal, values = edges$weight, title = "Total Decisions", position = "bottomright")
  })
  
  # Statistics to country of origin network
  output$statistics.origin <- renderTable({
    # access the igraph return of the graph_data function
    g <- create.origin.graph(dt.asylum, input$origin, input$Year_input, input$income_level)$graph    
    data.frame(
      Statistic = c("Number of vertices", "Number of edges"),
      Value = c(vcount(g), ecount(g))
    )
  })
  
  # Introduction to network characteristics asylum
  output$introduction.asylum <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Network Characteristics - Country of Asylum", "</h1>", 
               "<br>", "On this page you can find a network graph showing the connections between a specific country of asylum and its country of origins in order to find out more information where people come from that seek asylum in the specific country. Furthermore, it can be seen if the country has a lot of connections to country of origins and is therefore a popular country of asylum. That helps to identify popular asylum countries and allows for further analysis, why that could be the case. You can select the country of asylum, the year and the income level for the country of asylum", "<br>", "<br>"))
  })
  
  # World map with country of asylum network
  output$mymap.asylum <- renderLeaflet({
    graph_asylum <- create.asylum.graph(dt.asylum, input$asylum_1, input$Year_input_asyl, input$income_level_asyl)
    vert <- graph_asylum$vert
    edges <- graph_asylum$edges
    g <- graph_asylum$g
    edges_lines <- graph_asylum$edges_lines
    
    edges_df <- SpatialLinesDataFrame(edges_lines, edges)
    
    pal <- colorNumeric(palette = "YlOrRd", domain = unique(edges$weight), n = 10)

    leaflet(vert) %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addCircleMarkers() %>% 
      addPolylines(data = edges_df, weight = 2, color = ~pal(weight)) %>%
      addLegend(pal = pal, values = edges$weight, title = "Total Decisions", position = "bottomright")
  })
  
  # Statistics to country of asylum network
  output$statistics.asylum <- renderTable({
    # access the igraph return of the graph_data function
    g_asyl <- create.asylum.graph(dt.asylum, input$asylum_1, input$Year_input, input$income_level)$graph    
    data.frame(
      Statistic = c("Number of vertices", "Number of edges"),
      Value = c(vcount(g_asyl), ecount(g_asyl))
    )
  })
  
  # Introduction to circle network 

  output$header.cir <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Network Exploration", "</h1>", "<br>"))
  }) 
  
  # Introduction to circle network 
  output$introduction.cir <- renderText({
    HTML(paste("<h4>", "<strong>", "What you find here", "</strong>", "</h4>", "On this webpage, you can view a comprehensive network graph that displays the asylum application patterns across the world. Each country of origin is linked to the countries where people have applied for asylum. Hovering over the lines on the graph shows you the country of origin, the country of asylum, and the total number of asylum applications. You can filter the graph based on the year to observe changes in patterns over time. Additionally, we have categorized countries into five groups, which are explained further below. The graph can be filtered based on country and group, and the filtered connections will be highlighted in different colors to make it easier to analyze. The graph allows to find patterns.", "<br>", "<br>"))

  }) 
  
  # Shows circle network
  output$circular.plot <- renderVisNetwork({
      # Create the visNetwork object using the reactive expression
      graph_data()[[1]]
  })
  
  # Creates a table with statistical information about circle network
  output$info.circle <- renderTable({
    # access the igraph return of the graph_data function
    g <- graph_data()[[2]]
    data.frame(
      `Number of vertices` = vcount(g),
      `Number of edges` = ecount(g),
      `Diameter` = diameter(g),
      `Average path length` = mean(shortest.paths(g)),
      `Average clustering coefficient` = transitivity(g, type = "average")
    ) %>%
      rename_all(~gsub("\\.", " ", .)) # remove periods in column names
  }, row.names = FALSE)
  

 
  # Description of centrality measures for circle network
  output$description.between <- renderText({
    HTML(paste("<b>", "Betweenness", "</b>", "<br>", "Betweenness is a centrality measure in network analysis that quantifies the extent to which a node lies on the shortest paths between other pairs of nodes in the network. Specifically, it measures the number of times a node appears on the shortest path between any two other nodes in the network. In our context, betweenness centrality could indicate which countries act as intermediaries or \"bridges\" between other countries in the network. A country with high betweenness would be located on many of the shortest paths between other countries, making it an important mediator for the flow of asylum seekers or information between countries. In contrast, a country with low betweenness may have relatively little influence over the flow of information or asylum seekers between other countries in the network.", "<br>", "<br>", "<b>"))
  })  
  
  output$description.eigen <- renderText({
    HTML(paste("<b>", "Eigenvector", "</b>", "<br>", "Eigenvector is a centrality measure that takes into account both the number and importance of a node's direct neighbors in the network. Specifically, it assigns a higher score to a node that is connected to other high-scoring nodes in the network. In our context, eigenvector centrality could indicate which countries are connected to other highly influential countries in the network. A country with high eigenvector centrality would be connected to other countries that are themselves important within the network, potentially giving it a greater degree of influence or authority within the network as a whole. In contrast, a country with low eigenvector centrality may be more peripheral or disconnected from other influential countries in the network.

", "<br>"))

  })  
  
  output$description.close <- renderText({
    HTML(paste("<b>", "Closeness", "</b>", "<br>", "Closeness is a centrality measure that quantifies how quickly and efficiently information can be transmitted from one node to all other nodes in the network. Specifically, closeness measures the reciprocal of the sum of the shortest path distances between a node and all other nodes in the network. In our case closeness indicates which countries are most likely to receive asylum seekers from other countries in the network. A country with high closeness would be able to receive and transmit information more easily to other countries in the network, while a country with low closeness may be more isolated or disconnected from other countries in the network.", "<br>"))
  })
  
  # Creates a table with statistical information about circle network
  output$betweenness <- DT::renderDataTable({
    # access the igraph return of the graph_data function
    df <- graph_data()[[3]]
    DT::datatable(df, rownames = FALSE, options = list(dom = 't'))
  })
  
  
  # Show Table with min/max/median values 
  output$statistics.circ <- renderTable({
    df <- graph_data()[[4]]
    col <- switch(input$col,
                  "betweenness" = df$betweenness,
                  "closeness" = format(round(df$closeness, 4)),
                  "eigenvector" = df$eigenvector)
    min.val <- min(col)
    max.val <- max(col)
    mean.val <- mean(col, na.rm = TRUE)
    median.val <- median(col, na.rm = TRUE)
    sd.val <- round(sd(col), 6)
    statistics <- data.frame(
      Statistic = c("Minimum", "Mean", "Median", "Maximum", "Standard Deviation"),
      Value = c(min.val, mean.val, median.val, max.val, sd.val)
    )
    statistics
  })
  
  # Explanation of groups in circular network graph
  output$groups_circ_graph <- renderText({
    HTML(paste("<h4>", "Explanation of created groups for the circular network graph", "</h4>", "<br>", "<strong>", "Asylum Country:", "</strong>", "An asylum country is defined as a country that has a total number of refugees of zero and an amount of asylum seekers bigger than zero.", "<br>", "<br>", "<strong>", "Refugee Country:", "</strong>", "A refugee country has an amount of refugees bigger than zero and no asylum seekers that come into the country.", "<br>", "<br>", "<strong>", "Mainly Refugee Country:", "</strong>", "The number of asylum seekers from this country and towards that country is bigger than zero. Furthermore, the ratio of refugees and asylum seekers in that country is greater than 100.", "<br>", "<br>", "<strong>", "Dual Flow Country:", "</strong>", "The number of asylum seekers from this country and towards that country is bigger than zero. Furthermore, the ratio of refugees and asylum seekers in that country is greater than 10.", "<br>", "<br>", "<strong>", "Mainly Asylum Country:", "</strong>", "The number of asylum seekers from this country and towards that country is bigger than zero. The ratio of asylum seekers in that country and refugees from that country is bigger than 100."))
  })
  
  # Introduction to network prediction
  output$introduction_pred <- renderText({

    HTML(paste("<h1 style='color:green;'>", "Network Prediction", "</h1>", "<br>",
               "On this page we included a Jaccard Index calculation that reveals how 
               similar nations are based on their refugee flows. If two nations are showing similar behaviors, 
               they may also have similar migration patterns and encounter comparable difficulties 
               in aiding and assisting refugees.
               With the filter, you can enter a country name and see a list of the top countries that are 
               most likely to have similar refugee behavior based on our Jaccard Index calculation. Also it is 
               possible to distinguish if a country has similar patterns in terms of being the country of origin 
               and being the country of asylum.", "<br>", "<br>",
               
               "<b>", "Jaccard Index", "</b>", "<br>",
               "In general, the measure 'Jaccard Index' is a similarity measure in a network,
               which is considering two vertices and their number of common neighbors divided by 
               the total number of neighbors these both vertices have. It ranges from 0 to 1, 
               with 0 indicating no similarity between the sets and 1 indicating complete similarity.
               If necessary, it can also differentiate in directed network graphs between incoming and outgoing edges.", "<br>", "<br>"))
  })


  # Create a prediction
  output$mymap_pred <- renderLeaflet({
    graph_pred <- create_prediction_graph(input$country, input$in_out)
    vert <- graph_pred$vert
    edges <- graph_pred$edges
    g <- graph_pred$graph
    edges_lines <- graph_pred$edges_lines
    
    leaflet(vert) %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addCircleMarkers(color = "green") %>% 
      addPolylines(data = edges_lines, weight = 2, color = "green", dashArray = "5,10")
  })
  
  
  #describe prediction
  output$description_pred <- renderText({
    HTML(paste("<br>",
               "Analyzing refugee flows is an important task for understanding the complex network 
               of relationships that exist between countries of origin and countries of asylum.
               Using the Jaccard Index, we can identify which countries are similar regarding their refugee 
               behavior. In this case, we are using it to measure the similarity between the refugee flows of two countries.
               If two countries have similar refugee flows, it suggests that they may have similar migration patterns and
               face similar challenges in providing aid and support to refugees. It is possible to identify areas where 
               international cooperation and collaboration may be beneficial in addressing refugee-related issues.", "<br>",
               "Differentiating between country of origin and country of asylum is crucial in this analysis. The country 
               of origin refers to the country where the refugee is from, while the country of asylum is the country where 
               the refugee is seeking asylum or refuge. Both countries are in very different situations, which is why it is
               important to differentiate between them regarding our analysis.
               Overall, this analysis is helpful for understanding the complex network of relationships surrounding refugee
               migration and identifying patterns that can inform policy decisions and support efforts for refugees."))
    })
  
  # Statistics to country of origin network
  output$pred_info <- renderTable({
    # access the igraph return of the graph_data function
    graph <- create_prediction_graph(input$country, input$in_out)
    g <- graph$g_old
    data.frame(
      Country = graph$vert$name[!(graph$vert$name == input$country)],
      "Jaccard Index" = graph$edges$weight
    )
  })
  
  # Introduction for About section
  output$about <- renderText({
    HTML(paste("<h1 style='color:green;'>", "About what we do", "</h1>", "<br>"))
  })
  
 
}

