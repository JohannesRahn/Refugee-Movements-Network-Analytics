

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dt.aggregated.asylum <- aggregate_data()
  dt.asylum <- prepare_data()
  g = graph.data.frame(dt.asylum, directed=TRUE)
  
  
  # this is a reactive element to change origin and asylum column as character
  dt.asylum.st <- reactive({
    source("global.R")
    dt <- prepare_data()
    dt$Country.of.origin <- as.character(dt$Country.of.origin)
    dt$Country.of.asylum <- as.character(dt$Country.of.asylum)
    dt
  })
    
  graph_data <- reactive({
    # Calculate the graph based on the data
    circular_plot <- circular_graph(input$year)
    # Return the graph
    return(circular_plot)
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

  output$mymap <- renderLeaflet({
    graph <- create_asylum_graph(dt.asylum, input$origin, input$Year_input, input$income_level)
    vert <- graph$vert
    edges <- graph$edges
    g <- graph$g
    edges_lines <- graph$edges_lines
    
    leaflet(vert) %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addCircleMarkers() %>% 
      addPolylines(data = edges_lines, weight = 1, color = edges$weights)})
  
  output$info <- renderText({
    g <- create_asylum_graph(dt.asylum, input$origin, input$Year_input, input$income_level)$graph
    HTML(paste("<br>", "<b>", "Number of vertices: ", "</b>", vcount(g), "<br>", "<br>",
               "<b>", "Number of edges: ", "</b>", ecount(g), "<br>", "<br>",
               "<b>", "Diameter: ", "</b>", diameter(g), "<br>", "<br>",
               "<b>", "Average Path Length: ", "</b>", mean(shortest.paths(g)), "<br>", "<br>",
               "<b>", "Average clustering coefficient: ", "</b>", transitivity(g, type = "average"), "<br>", "<br>",
               "<b>", "Average degree: ", "</b>", mean(degree(g))))
  })
  
  output$introduction <- renderText({
    HTML(paste("<h1 style='color:green;'>", "Network Characteristics", "</h1>", "<br>", "On the network page you can find a network graph showing the connections between country of origin and the country of asylum. You can select the country of origin, the year and the income level for the country of asylum.", "<br>", "<br>"))
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
    
  output$introduction_cir <- renderText({
    HTML(paste("<h1 style='color:blue;'>", "Network Descriptives", "</h1>", "<br>", "On this page you can find a complete network graph for the asylum applications worldwide. The country of origin is connected with the country of asylum. The network data is displayed per year, so you can try to find changes in the pattern between the different years. Furthermore, we divided the countries in five different groups. Describe the different groups", "<br>", "<br>"))
  })  
  
  output$description_cen <- renderText({
    HTML(paste("<b>", "Betweenness", "</b>", "<br>", "This measures the extent to which a node lies on the shortest path between other nodes. In the context of your refugee network, this could be interpreted as the extent to which a country plays a critical role in facilitating the movement of refugees between other countries.", "<br>", "<br>", "<b>", "Eigenvector", "</b>", "<br>", "This measures the importance of a node based on the importance of its neighbors. In the context of your refugee network, this could be interpreted as the importance of a country based on the importance of the other countries it is connected to.", "<br>"))
  })  
    # Creates a table with statistical information about circle_graph
    output$betweenness <- renderTable({
      # access the igraph return of the graph_data function
      g <- graph_data()[[2]]
      country.data <- graph_data()[[3]]
      country.data$btw <- betweenness(g)
      # df <- data.frame(vertex = V(g)$name, betweenness(g))
      country.data$ev_cen <- eigen_centrality(g)$vector
      country.data$close <- closeness(g)
      country.data$page <- page_rank(g)
      df.between <- country.data[order(-country.data$btw), c("Country", "btw")]
      df.between <- df.between[1:10, ]
      df.between 
      df.between
  })
}

