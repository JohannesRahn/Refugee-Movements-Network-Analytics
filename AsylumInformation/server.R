source("global.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dt.aggregated.asylum <- aggregate_data()
  dt.asylum <- prepare_data()
  observe(print(input$origin))
  # observe(print(colnames(dt.asylum)))
  
  mydata <- reactive({
    if (is.null(input$origin)) {dt.asylum.input <- dt.asylum
    } else dt.asylum.input <- dt.asylum[dt.asylum$Country.of.origin %in% input$origin, ]
    dt.asylum.input
  })
  
  
  # this is a reactive element to change origin and asylum column as character
  dt.asylum.st <- reactive({
    dt <- prepare_data()
    dt$Country.of.origin <- as.character(dt$Country.of.origin)
    dt$Country.of.asylum <- as.character(dt$Country.of.asylum)
    dt
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
  
  output$origin.selector <- renderUI({
    selectInput("origin_1",
                label = "Choose country of origin",
                choices = unique(dt.asylum.st()$Country.of.origin),
                selected = dt.asylum.st()$Country.of.origin[1])
  })
  
  output$asylum.selector <- renderUI({
    selectInput("asylum",
                label = "Choose country of asylum",
                choices = unique(dt.asylum.st()$Country.of.asylum),
                selected = dt.asylum.st()$Country.of.asylum[1])
  })
  
  output$asylum.income.selector <- renderUI({
    selectInput("asylum.income",
                label = "Choose income level of asylum country",
                choices = unique(dt.asylum.st()$Asylum_Income),
                selected = dt.asylum.st()$Asylum_Income[1])
  })


}

