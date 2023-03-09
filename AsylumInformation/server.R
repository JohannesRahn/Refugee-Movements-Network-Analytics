source("global.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  dt.aggregated.asylum <- aggregate_data()
  
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

}

