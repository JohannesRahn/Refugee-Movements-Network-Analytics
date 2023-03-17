source("global.R")
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(digits = 4)
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
    circular_plot <- circular_graph(input$year)
    # Return the graph
    return(circular_plot)
  })  
  
  # Reactive element for descriptive filter:
  descriptive_data <- reactive({
    
    dt.descriptive <- filter_region_analysis(dt.asylum, input$asylumIncomeGroupFilter, input$originIncomeGroupFilter, input$originRegionFilter, input$asylumRegionFilter)
    
    # The descriptive analysis only needs aggregated data
    dt.aggregated.descriptive <- na.omit(aggregate_data(dt.descriptive))
    return(dt.aggregated.descriptive)
  })
  
  # Reactive element for descriptive filter:
  authority_data <- reactive({
    
    dt.descriptive <- filter_region_analysis(dt.asylum, input$asylumIncomeGroupFilter, input$originIncomeGroupFilter, input$originRegionFilter, input$asylumRegionFilter)
    dt.grouped.authority <- dt.descriptive %>%
      group_by(Authority) %>%
      summarise(Total_decisions = sum(Total.decisions))
    # The descriptive analysis only needs aggregated data
    return(dt.grouped.authority)
  })
  
  filter_region_analysis <- function(dt.descriptive, asylumIncomeGroupFilter, originIncomeGroupFilter ,
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
  
  
  custom_theme <- function() {
    theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12),
            panel.grid.minor = element_line(color = "gray85"),
            panel.grid.major = element_line(color = "gray75"))
  }
  output$total.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Total_decisions)) +
      geom_line(color = "#1F78B4", size = 1.5, linetype = "solid") +
      labs(title = "Total Decisions per Year",
           x = "Year",
           y = "Total Decisions") +
      custom_theme()
  })
  output$recognized.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Recognized_decisions)) +
      geom_line(color = "#1F78B4", size = 1.5, linetype = "solid") +
      labs(title = "Recognized Decisions per Year",
           x = "Year",
           y = "Recognized Decisions") +
      custom_theme()
  })
  
  output$rejected.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Rejected_decisions)) +
      geom_line(color = "#E31A1C", size = 1.5, linetype = "solid") +
      labs(title = "Rejected Decisions per Year",
           x = "Year",
           y = "Rejected Decisions") +
      custom_theme()
  })
  
  output$otherwise.closed.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Otherwise_closed)) +
      geom_line(color = "#33A02C", size = 1.5, linetype = "solid") +
      labs(title = "Otherwise Closed Decisions per Year",
           x = "Year",
           y = "Otherwise Closed Decisions") +
      custom_theme()
  })
  
  output$total.closed.decisions.plot <- renderPlot({
    ggplot(descriptive_data(), aes(x = Year, y = Total_closed)) +
      geom_line(color = "#6A3D9A", size = 1.5, linetype = "solid") +
      labs(title = "Total Closed Decisions per Year",
           x = "Year",
           y = "Total Closed Decisions") +
      custom_theme()
  })
  output$combined.decisions.plot <- renderPlot({
    ggplot(descriptive_data()) +
      geom_line(aes(x = Year, y = Rejected_decisions, color = "Rejected Decisions"), size = 1.5, linetype = "solid") +
      geom_line(aes(x = Year, y = Otherwise_closed, color = "Otherwise Closed Decisions"), size = 1.5, linetype = "solid") +
      geom_line(aes(x = Year, y = Total_closed, color = "Total Closed Decisions"), size = 1.5, linetype = "solid") +
      labs(title = "Closed Decisions per Year",
           x = "Year",
           y = "Decisions") +
      scale_color_manual(values = c("Rejected Decisions" = "#E31A1C", "Otherwise Closed Decisions" = "#33A02C", "Total Closed Decisions" = "#6A3D9A")) +
      custom_theme()
  })
  
  output$authority.decisions.plot <- renderPlot({
    ggplot(authority_data(), aes(x = Authority, y = Total_decisions)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Total Decisions by Asylum Authority",
         x = "Asylum Authority",
         y = "Total Decisions") +
    custom_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

  # Introduction to network characteristics
  output$introduction <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Network Characteristics", "</h1>", 
               "<br>", "On the network page you can find a network graph showing the connections between country of origin and the country of asylum. You can select the country of origin, the year and the income level for the country of asylum.", "<br>", "<br>"))
  })
  
  # World map with country of origin network
  output$mymap <- renderLeaflet({
    graph <- create_asylum_graph(dt.asylum, input$origin, input$Year_input, input$income_level)
    vert <- graph$vert
    edges <- graph$edges
    g <- graph$g
    edges_lines <- graph$edges_lines
    
    edges_df <- SpatialLinesDataFrame(edges_lines, edges)
    
    pal <- colorQuantile(palette = "YlOrRd", domain = unique(edges$weight), n = 10)
    # Calculate quantiles and create labels
    quantiles <- quantile(edges$weight, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
    
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
    HTML(paste("<h1 style='color:blue;'>", "Network Descriptives", "</h1>", "<br>", 
               "On this page you can find a complete network graph for the asylum applications worldwide. The country of origin is connected with the country of asylum. The network data is displayed per year, so you can try to find changes in the pattern between the different years. Furthermore, we divided the countries in five different groups. Describe the different groups", "<br>", "<br>"))
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
    HTML(paste("<h1 style='color:blue;'>", "Network Prediction", "</h1>", "<br>", "On this page you can find countries with similar patterns.", "<br>", "<br>"))
  })
  
  # Create a prediction
  output$mymap_pred <- renderLeaflet({
    graph_pred <- create_prediction_graph()
    vert <- graph_pred$vert
    edges <- graph_pred$edges
    g <- graph_pred$graph
    edges_lines <- graph_pred$edges_lines
    
    leaflet(vert) %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addCircleMarkers() %>% 
      addPolylines(data = edges_lines, weight = 1)
  })
  
  # Introduction for About section
  output$about <- renderText({
    HTML(paste("<h1 style='color:blue;'>", "About what we do", "</h1>", "<br>"))
  })
  
 
}

