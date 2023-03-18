

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(digits = 4)
  dt.aggregated.asylum <- aggregate_data()
  dt.asylum <- prepare_data()
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
    circular_plot <- circular_graph(input$year)
    # Return the graph
    return(circular_plot)
  })  
  
  
  # Introduction for Descriptive section
  output$introduction_descriptives <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Descriptive Statistics", "</h1>", "<br>"))
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

  # Introduction to network characteristics
  output$introduction <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Network Characteristics", "</h1>", "<br>", "On the network page you can find a network graph showing the connections between country of origin and the country of asylum. You can select the country of origin, the year and the income level for the country of asylum.", "<br>", "<br>"))
  })
  
  # World map with country of origin network
  output$mymap <- renderLeaflet({
    graph <- create_asylum_graph(dt.asylum, input$origin, input$Year_input, input$income_level)
    vert <- graph$vert
    edges <- graph$edges
    g <- graph$g
    edges_lines <- graph$edges_lines
    
    edges_df <- SpatialLinesDataFrame(edges_lines, edges)
    
    pal <- colorNumeric(palette = "YlGn", domain = unique(edges$weight), n = 10)

    leaflet(vert) %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addCircleMarkers() %>% 
      addPolylines(data = edges_df, weight = 2, color = ~pal(weight)) %>%
      addLegend(pal = pal, values = edges$weight, title = "Total Decisions", position = "bottomright")
  })
  
  # Statistics to country of origin network
  output$info <- renderTable({
    # access the igraph return of the graph_data function
    g <- create_asylum_graph(dt.asylum, input$origin, input$Year_input, input$income_level)$graph    
    data.frame(
      Statistic = c("Number of vertices", "Number of edges", "Diameter",
                    "Average Path Length", "Average clustering coefficient", "Average degree"),
      Value = c(vcount(g), ecount(g), diameter(g),
                mean(shortest.paths(g)), transitivity(g, type = "average"), mean(degree(g)))
    )
  })
  
  # Introduction to circle network 
  output$introduction_cir <- renderText({
    HTML(paste("<h1 style='color:blue;'>", "Network Descriptives", "</h1>", "<br>", "On this page you can find a complete network graph for the asylum applications worldwide. The country of origin is connected with the country of asylum. The network data is displayed per year, so you can try to find changes in the pattern between the different years. Furthermore, we divided the countries in five different groups. Describe the different groups", "<br>", "<br>"))
  }) 
  
  # Shows circle_graph
  output$circular_plot <- renderVisNetwork({
    # Check if the button has been clicked
    if (input$show_graph > 0) {
      # Create the visNetwork object using the reactive expression
      graph_data()[[1]]
    }
  })
  
  # Creates a table with statistical information about circle_graph
  output$info_circle <- renderTable({
    # access the igraph return of the graph_data function
    g <- graph_data()[[2]]
    data.frame(
      Statistic = c("Number of vertices", "Number of edges", "Diameter",
                    "Average Path Length", "Average clustering coefficient", "Average degree"),
      Value = c(vcount(g), ecount(g), diameter(g),
                mean(shortest.paths(g)), transitivity(g, type = "average"), mean(degree(g)))
    )
  })
  
  # Description of centrality measures
  output$description_cen <- renderText({
    HTML(paste("<b>", "Betweenness", "</b>", "<br>", "This measures the extent to which a node lies on the shortest path between other nodes. In the context of your refugee network, this could be interpreted as the extent to which a country plays a critical role in facilitating the movement of refugees between other countries.", "<br>", "<br>", "<b>", "Eigenvector", "</b>", "<br>", "This measures the importance of a node based on the importance of its neighbors. In the context of your refugee network, this could be interpreted as the importance of a country based on the importance of the other countries it is connected to.", "<br>"))
  })  
  
  # Creates a table with statistical information about circle_graph
  output$betweenness <- renderTable({
      # access the igraph return of the graph_data function
      df <- graph_data()[[3]]
      df
  })
  
  # Show Table with min/max/median values 
  output$statistics.circ <- renderTable({
    df <- graph_data()[[4]]
    col <- switch(input$col,
                  "betweenness" = df$betweenness,
                  "closeness" = df$closeness,
                  "eigenvector" = df$eigenvector)
    min_val <- min(col)
    max_val <- max(col)
    mean_val <- mean(col)
    median_val <- median(col)
    sd_val <- sd(col)
    statistics <- data.frame(
      Statistic = c("Minimum", "Mean", "Median", "Maximum", "Standard Deviation"),
      Value = c(min_val, mean_val, median_val, max_val, sd_val)
    )
    statistics
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
    HTML(paste("<h1 style='color:blue;'>", "About what we do", "</h1>", "<br>"))
  })
  
 
}

