# Required Libraries
library(shiny)
library(ggplot2)
library(readxl)
library(DT)  # For displaying tables
library(shinydashboard)


# UI Components
ui <- dashboardPage(
  dashboardHeader(title = "Data Visualizer"),
  
  dashboardSidebar(
    fileInput("file1", "Choose CSV or Excel File", 
              multiple = FALSE, 
              accept = c("text/csv", ".csv", ".xlsx")),
    
    selectInput("visType", "Select Visualization Type:", 
                c("None", "Table", "Histogram", "Boxplot - Single Category", 
                  "Boxplot - Multiple Categories", "Line Graph", "Bar Chart")),
    
    selectInput("statFeatures", "Select Statistical Feature:", 
                c("None", "Standard Deviation", "Spearman's Rank", 
                  "Chi-squared Goodness of Fit Test", "Chi-squared Test for Independence", 
                  "Chi-squared Test for Homogeneity")),
    
    conditionalPanel(
      condition = "input.visType == 'Histogram'",
      numericInput("binWidth", "Histogram Bin Width:", value = 1, min = 0.1, step = 0.1)
    )
  ),
  
  dashboardBody(
    fluidRow(
      column(9,
             DTOutput("dataTable"),
             plotOutput("visPlot"),
             conditionalPanel(
               condition = "input.visType == 'Boxplot - Single Category' || input.visType == 'Boxplot - Multiple Categories'",
               box(title = "Summary Statistics", verbatimTextOutput("boxplotStats"))
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Standard Deviation'",
               box(
                 title = "Standard Deviation",
                 div(style = "height: 150px;", verbatimTextOutput("stdDeviation")),
                 width = 12
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Spearman\\'s Rank'",
               box(
                 title = "Spearman's Rank",
                 div(style = "height: 150px;", verbatimTextOutput("spearmanRank")),
                 width = 12
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Chi-squared Goodness of Fit Test'",
               box(
                 title = "Chi-squared Goodness of Fit Test",
                 div(style = "height: 150px;", textOutput("chiSquaredGOF")),
                 width = 12
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Chi-squared Test for Independence'",
               box(
                 title = "Chi-squared Test for Independence",
                 div(style = "height: 150px;", textOutput("chiSquaredInd")),
                 width = 12
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Chi-squared Test for Homogeneity'",
               box(
                 title = "Chi-squared Test for Homogeneity",
                 div(style = "height: 150px;", textOutput("chiSquaredHom")),
                 width = 12
               )
             )
      )
    )
  )
)



server <- function(input, output, session) {
  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    
    switch(tools::file_ext(inFile$datapath),
           csv = read.csv(inFile$datapath),
           xlsx = read_excel(inFile$datapath))
  })
  
  output$dataTable <- renderDT({
    if (input$visType == "Table") {
      return(data())
    } else {
      return(NULL)
    }
  })
  
  output$stdDeviation <- renderText({
    if (is.null(data()) || input$statFeatures != "Standard Deviation") {
      return(NULL)
    }
    
    stdDevs <- sapply(data(), function(col) {
      if (is.numeric(col)) {
        round(sd(col, na.rm = TRUE), 2)
      } else {
        NA
      }
    }, simplify = TRUE)
    
    stdDevText <- paste(names(stdDevs), stdDevs, sep = ": ", collapse = "\n")
    return(stdDevText)
  })
  
  output$spearmanRank <- renderText({
    if (is.null(data()) || input$statFeatures != "Spearman's Rank" || ncol(data()) != 2) {
      return(NULL)
    }
    
    if (is.numeric(data()[[1]]) && is.numeric(data()[[2]])) {
      corValue <- cor(data()[[1]], data()[[2]], method="spearman", use="pairwise.complete.obs")
      return(paste("Spearman's Rank Correlation:", round(corValue, 2)))
    } else {
      return("Both columns should be numeric for Spearman's Rank calculation.")
    }
  })
  
  output$chiSquaredGOF <- renderText({
    if (is.null(data()) || input$statFeatures != "Chi-squared Goodness of Fit Test") {
      return(NULL)
    }
    
    observed_frequencies <- table(data()[[1]])  # For simplicity, using the first column
    chisq <- chisq.test(observed_frequencies)
    
    return(paste("Chi-squared Goodness of Fit Test:",
                 "\nX-squared =", round(chisq$statistic, 2),
                 "\np-value =", round(chisq$p.value, 2)))
  })
  
  output$chiSquaredInd <- renderText({
    if (is.null(data()) || input$statFeatures != "Chi-squared Test for Independence") {
      return(NULL)
    }
    
    contingency_table <- table(data()[[1]], data()[[2]])  # For simplicity, using the first two columns
    chisq <- chisq.test(contingency_table)
    
    return(paste("Chi-squared Test for Independence:",
                 "\nX-squared =", round(chisq$statistic, 2),
                 "\np-value =", round(chisq$p.value, 2)))
  })
  
  output$chiSquaredHom <- renderText({
    if (is.null(data()) || input$statFeatures != "Chi-squared Test for Homogeneity") {
      return(NULL)
    }
    
    contingency_table <- table(data()[[1]], data()[[2]])  # For simplicity, using the first two columns
    chisq <- chisq.test(contingency_table)
    
    return(paste("Chi-squared Test for Homogeneity:",
                 "\nX-squared =", round(chisq$statistic, 2),
                 "\np-value =", round(chisq$p.value, 2)))
  })
  
  output$visPlot <- renderPlot({
    if (is.null(data())) {
      return(NULL)
    }
    
    switch(input$visType,
           "None" = return(NULL),
           "Table" = return(NULL),
           "Histogram" = {
             binWidth <- ifelse(is.null(input$binWidth) || input$binWidth == 0, 1, input$binWidth)
             breaks <- seq(min(data()[[1]], na.rm = TRUE) - 0.5, max(data()[[1]], na.rm = TRUE) + 0.5, binWidth)
             ggplot(data(), aes(x=data()[[1]])) + 
               geom_histogram(breaks=breaks, closed="left") + 
               labs(x = names(data())[1])
           },
           "Boxplot - Single Category" = {
             if (ncol(data()) == 1 && is.numeric(data()[[1]])) {
               ggplot(data(), aes(y=data()[[1]])) + 
                 geom_boxplot() + 
                 labs(y = names(data())[1])
             } else {
               return(NULL)
             }
           },
           "Boxplot - Multiple Categories" = {
             if (ncol(data()) == 2 && is.numeric(data()[[2]])) {
               catCol <- names(data())[1]
               numCol <- names(data())[2]
               p <- ggplot(data(), aes_string(x=catCol, y=numCol)) + 
                 geom_boxplot() + 
                 labs(x = catCol, y = numCol)
               return(p)
             } else {
               return(NULL)
             }
           },
           "Line Graph" = {
             if (ncol(data()) == 1 && is.numeric(data()[[1]])) {
               ggplot(data(), aes(x=1:nrow(data()), y=data()[[1]])) + 
                 geom_line() + 
                 labs(x = "Index", y = names(data())[1])
             } else if (ncol(data()) == 2 && is.numeric(data()[[2]])) {
               local_data <- data()
               timeCol <- names(local_data)[1]
               valueCol <- names(local_data)[2]
               ggplot(local_data, aes_string(x=timeCol, y=valueCol)) + 
                 geom_line() + 
                 labs(x = timeCol, y = valueCol)
             } else {
               return(NULL)
             }
           },
           "Bar Chart" = {
             if (ncol(data()) == 2) {
               local_data <- data()
               catCol <- names(local_data)[1]
               numCol <- names(local_data)[2]
               if (is.numeric(local_data[[numCol]]) && !is.numeric(local_data[[catCol]])) {
                 ggplot(local_data, aes_string(x=catCol, y=numCol)) +
                   geom_bar(stat = "identity") +
                   labs(x = catCol, y = numCol)
               } else {
                 return(NULL)
               }
             } else {
               return(NULL)
             }
           })
  })

  
  
  # Output the boxplot statistics
  output$boxplotStats <- renderText({
    if (is.null(data()) || (input$visType != "Boxplot - Single Category" && input$visType != "Boxplot - Multiple Categories")) {
      return(NULL)
    }
    
    if (input$visType == "Boxplot - Single Category") {
      numCol <- names(data())[1]
      stats <- boxplot.stats(data()[[numCol]])
      return(paste("Statistics for the entire dataset:\n",
                   "Lowest Value: ", stats$stats[1], "\n",
                   "Lower Quartile (Q1): ", stats$stats[2], "\n",
                   "Median: ", stats$stats[3], "\n",
                   "Upper Quartile (Q3): ", stats$stats[4], "\n",
                   "Highest Value: ", stats$stats[5], "\n"))
    }
    
    if (input$visType == "Boxplot - Multiple Categories") {
      catCol <- names(data())[1]
      numCol <- names(data())[2]
      stats_text <- ""
      for (cat in unique(data()[[catCol]])) {
        cat_data <- data()[data()[[catCol]] == cat, numCol, drop=TRUE]  # Ensure it's a vector, not a dataframe
        stats <- boxplot.stats(cat_data)
        stats_text <- paste(stats_text,
                            "Statistics for ", cat, ":\n",
                            "Lowest Value: ", stats$stats[1], "\n",
                            "Lower Quartile (Q1): ", stats$stats[2], "\n",
                            "Median: ", stats$stats[3], "\n",
                            "Upper Quartile (Q3): ", stats$stats[4], "\n",
                            "Highest Value: ", stats$stats[5], "\n\n")
      }
      return(stats_text)
    }
  })
}

# Run the Application
shinyApp(ui = ui, server = server)