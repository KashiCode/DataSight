# Required Libraries
library(shiny)
library(ggplot2)
library(readxl)
library(DT)  # For displaying tables
library(shinydashboard)
library(reshape2)
library(forecast)


# UI Components
ui <- dashboardPage(
  dashboardHeader(title = "Data Visualizer"),
  
  dashboardSidebar(
    fileInput("file1", "Choose CSV or Excel File", 
              multiple = FALSE, 
              accept = c("text/csv", ".csv", ".xlsx")),
    
    selectInput("visType", "Select Visualization Type:", 
                c("None", "Table", "Histogram", "Boxplot - Single Category", 
                  "Boxplot - Multiple Categories", "Line Graph", "Bar Chart", "Scatterplot", "Heatmap", "Pie Chart", "Correlation Matrix", "Density Plot", "Violin Plot", "Faceted Plot", "Time Series Graph", "Q-Q Plot")),
    
    selectInput("statFeatures", "Select Statistical Feature:", 
                c("None", "Standard Deviation", "Spearman's Rank", 
                  "Chi-squared Goodness of Fit Test", "Chi-squared Test for Independence/Homogeneity", "Basic Descriptive Statistics", "Pearson's Correlation Coefficient", "Confidence Intervals for Proportions", "Confidence Intervals for Means", "T-test","ANOVA", "Outlier Detection", "Time Series Analysis","Shapiro-Wilk Test")),
    
    conditionalPanel(
      condition = "input.statFeatures == 'Confidence Intervals for Proportions' || input.statFeatures == 'Confidence Intervals for Means'",
      numericInput("confLevel", "Confidence Level:", value = 95, min = 0, max = 100, step = 1)
    ),
    
    conditionalPanel(
      condition = "input.statFeatures == 'Outlier Detection'",
      selectInput("outlierColumns", "Select Columns for Outlier Detection:", 
                  choices = NULL, selected = NULL, multiple = TRUE)
    ),
    
    conditionalPanel(
      condition = "input.statFeatures == 'Time Series Analysis'",
      selectInput("timeSeriesColumn", "Select Time Series Column:", choices = NULL),
      numericInput("frequencyInput", "Frequency of Time Series:", value = 1),
      numericInput("windowInput", "STL Decomposition Window:", value = 7)
    ),
    
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
               box(
                 title = "Summary Statistics",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("boxplotStats")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Standard Deviation'",
               box(
                 title = "Standard Deviation",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("stdDeviation")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Spearman\\'s Rank'",
               box(
                 title = "Spearman's Rank",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("spearmanRank")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Chi-squared Goodness of Fit Test'",
               box(
                 title = "Chi-squared Goodness of Fit Test",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("chiSquaredGOF")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Chi-squared Test for Independence/Homogeneity'",
               box(
                 title = "Chi-squared Test for Independence/Homogeneity",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("chiSquaredInd")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Pearson\\'s Correlation Coefficient'",
               box(
                 title = "Pearson's Correlation Coefficient",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("pearsonCorrelation")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Confidence Intervals for Proportions'",
               box(
                 title = "Confidence Intervals for Proportions",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("confidenceIntervals")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Confidence Intervals for Means'",
               box(
                 title = "Confidence Intervals for Means",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("confidenceIntervals2")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'T-test'",
               box(
                 title = "T-test Result",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("tTestResult")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'ANOVA'",
               box(
                 title = "ANOVA Result",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("anovaResult")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Outlier Detection'",
               box(
                 title = "Outlier Detection Results",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("outlierDetection")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Time Series Analysis'",
               box(
                 title = "Time Series Analysis Results",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("timeSeriesAnalysisResults")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Shapiro-Wilk Test'",
               box(
                 title = "Shapiro-Wilk Test Result",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("shapiroWilk")
               )
             ),
             conditionalPanel(
               condition = "input.statFeatures == 'Basic Descriptive Statistics'",
               box(
                 title = "Basic Descriptive Statistics",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 width = 13,
                 verbatimTextOutput("basicStats")
               )
             )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Define the detect_outliers function to return actual outlier values
  detect_outliers <- function(x) {
    if (!is.numeric(x)) {
      return(NULL)
    }
    
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    outliers <- x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)]
    return(outliers)
  }
  
  
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
  
  # Observe any changes in the data and update the dropdown choices
  observe({
    df <- data()  # Fetch the reactive data
    if (!is.null(df)) {
      updateSelectInput(session, "outlierColumns", choices = names(df))
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
  
  
  #Time series analysis.
  # Observe any changes in the data and update the dropdown choices for time series column
  observe({
    df <- data()  # Fetch the reactive data
    if (!is.null(df)) {
      updateSelectInput(session, "timeSeriesColumn", choices = names(df))
    }
  })
  
  # Render the text output for time series analysis
  output$timeSeriesAnalysisResults <- renderText({
    # Make sure a column is selected and the 'Time Series Analysis' feature is selected
    req(input$timeSeriesColumn, input$statFeatures == "Time Series Analysis")
    
    # Ensure the data is not null, the feature is selected, and the appropriate number of columns exists
    if (is.null(data()) || ncol(data()) < 2) {
      return("Data not available or not suitable for Time Series Analysis")
    }
    
    # Access the selected column's data and convert it to a ts object with the specified frequency
    ts_data <- ts(data()[[input$timeSeriesColumn]], frequency = input$frequencyInput)
    
    # Perform STL decomposition with a periodic window
    decomposition <- stl(ts_data, s.window = input$windowInput)
    
    # Calculate autocorrelation
    acf_result <- acf(ts_data, plot = FALSE)
    
    # Extract the components of the decomposition
    seasonal_component <- decomposition$time.series[, "seasonal"]
    trend_component <- decomposition$time.series[, "trend"]
    remainder_component <- decomposition$time.series[, "remainder"]
    
    # Format and return the results
    paste("Seasonal Component: ", round(head(seasonal_component, 5), 2), "\n",
          "Trend Component: ", round(head(trend_component, 5), 2), "\n",
          "Remainder Component: ", round(head(remainder_component, 5), 2), "\n",
          "Autocorrelation: ", round(head(acf_result$acf, 5), 2), "\n")
  })
    
  
  # Revised code for the output$outlierDetection renderText
  output$outlierDetection <- renderText({
    req(data())  # Ensure that 'data' is not NULL before proceeding
    
    selectedCols <- input$outlierColumns
    if (is.null(selectedCols) || length(selectedCols) == 0) {
      return("No columns selected for outlier detection.")
    }
    
    # Apply the detect_outliers function to the selected columns and collect results
    outlierResults <- sapply(data()[, selectedCols, drop=FALSE], detect_outliers, simplify = FALSE)
    
    # Prepare the text to display the results
    outlierText <- ""
    for (colName in selectedCols) {
      outliers <- outlierResults[[colName]]
      if (length(outliers) > 0) {
        outlierText <- paste(outlierText, colName, "Outliers:", paste(outliers, collapse=", "), "\n")
      } else {
        outlierText <- paste(outlierText, colName, "No outliers detected.\n")
      }
    }
    
    return(outlierText)
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
  
  output$shapiroWilk <- renderText({
    # Make sure a numeric column is selected
    req(input$statFeatures == "Shapiro-Wilk Test")
    
    # Check if the data is numeric and suitable for the Shapiro-Wilk test
    if (is.null(data()) || !is.numeric(data()[[1]])) {
      return("Please select a numeric column to perform the Shapiro-Wilk test.")
    }
    
    # Perform the Shapiro-Wilk test
    shapiro_test <- shapiro.test(data()[[1]])
    
    # Create the result text
    result_text <- paste("Shapiro-Wilk normality test:",
                         "W value:", round(shapiro_test$statistic, 4),
                         "P-value:", round(shapiro_test$p.value, 4),
                         sep = "\n")
    
    return(result_text)
  })
  
  output$confidenceIntervals <- renderText({  # Corrected output ID
    if (is.null(data()) || input$statFeatures != "Confidence Intervals for Proportions") {
      return(NULL)
    }
    
    if (all(data()[[1]] %in% c(0, 1))) {  # Checks if the first column is binary
      conf_level <- input$confLevel / 100  # Convert percentage to proportion
      alpha <- 1 - conf_level
      p_hat <- mean(data()[[1]])
      n <- length(data()[[1]])
      z_score <- qnorm(1 - alpha / 2)
      std_error <- sqrt(p_hat * (1 - p_hat) / n)
      lower_bound <- p_hat - z_score * std_error
      upper_bound <- p_hat + z_score * std_error
      
      return(paste(conf_level * 100, "% Confidence Interval for the proportion of", names(data())[1], ":\n",
                   "Lower Bound: ", round(lower_bound, 2), "\n",
                   "Upper Bound: ", round(upper_bound, 2)))
    } else {
      return("The first column should be binary (0/1) to calculate the confidence interval for a proportion.")
    }
  })
  
  output$confidenceIntervals2 <- renderText({
    if (is.null(data()) || input$statFeatures != "Confidence Intervals for Means" || ncol(data()) < 2) {
      return(NULL)
    }
    
    if (is.numeric(data()[[1]]) && is.numeric(data()[[2]])) {
      conf_level <- input$confLevel / 100  # Convert percentage to proportion
      alpha <- 1 - conf_level
      x_bar_1 <- mean(data()[[1]], na.rm = TRUE)
      x_bar_2 <- mean(data()[[2]], na.rm = TRUE)
      s1 <- sd(data()[[1]], na.rm = TRUE)
      s2 <- sd(data()[[2]], na.rm = TRUE)
      n1 <- sum(!is.na(data()[[1]]))
      n2 <- sum(!is.na(data()[[2]]))
      pooled_variance <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
      standard_error <- sqrt(pooled_variance * (1/n1 + 1/n2))
      t_score <- qt(1 - alpha / 2, df = n1 + n2 - 2)
      lower_bound <- (x_bar_1 - x_bar_2) - t_score * standard_error
      upper_bound <- (x_bar_1 - x_bar_2) + t_score * standard_error
      
      return(paste(input$confLevel, "% Confidence Interval for the difference between means of", names(data())[1], "and", names(data())[2], ":\n",
                   "Lower Bound: ", round(lower_bound, 2), "\n",
                   "Upper Bound: ", round(upper_bound, 2)))
    } else {
      return("The first two columns should be numeric to calculate the confidence interval for the difference between means.")
    }
  })
  
  output$tTestResult <- renderText({
    req(input$file1)  # Make sure a file is uploaded
    
    # Get the data from the uploaded file
    df <- data()
    
    # Make sure there are at least two columns to compare
    if (is.null(df) || ncol(df) < 2) {
      return("Please upload a file with at least two numeric columns for the T-test.")
    }
    
    # Make sure the first two columns are numeric
    if (!is.numeric(df[[1]]) || !is.numeric(df[[2]])) {
      return("Please ensure the first two columns are numeric for the T-test.")
    }
    
    # Perform the T-test
    t_test <- t.test(df[[1]], df[[2]])
    
    # Create the result text
    result_text <- paste("T-test results:",
                         "T-value:", round(t_test$statistic, 2),
                         "P-value:", round(t_test$p.value, 4),
                         "Degrees of Freedom:", t_test$parameter,
                         "Confidence Interval:", paste(round(t_test$conf.int, 2), collapse = " to "),
                         sep = "\n")
    
    return(result_text)
  })
  
  output$anovaResult <- renderText({
    req(input$file1) # Ensure that the file input is provided before proceeding
    
    # Get the data frame from reactive data object
    df <- data()
    
    # Check if the data frame exists and has the expected structure
    if (is.null(df) || ncol(df) < 2 || !is.numeric(df[[2]])) {
      return("Ensure the first column is a factor (group) and the second column is numeric for ANOVA.")
    }
    
    # Convert the first column to a factor if it's not already
    df[[1]] <- factor(df[[1]])
    
    # Run ANOVA
    anova_test <- aov(df[[2]] ~ df[[1]], data = df)
    anova_summary <- summary(anova_test)
    
    # Get the p-value, ensuring it's the first and only the first
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    # Ensure p_value is a single logical value
    if (length(p_value) != 1) {
      return("Error: p-value calculation resulted in an unexpected format.")
    }
    
    # Create a dynamic interpretation based on the p-value
    significance_statement <- if (p_value < 0.001) {
      "There is a highly statistically significant difference in the means of the response variable across the groups (p < 0.001)."
    } else if (p_value < 0.01) {
      "There is a statistically significant difference in the means of the response variable across the groups (p < 0.01)."
    } else if (p_value < 0.05) {
      "There is a statistically significant difference in the means of the response variable across the groups (p < 0.05)."
    } else {
      "There is not enough evidence to suggest a statistically significant difference in the means of the response variable across the groups (p >= 0.05)."
    }
    
    # Interpret the results
    interpretation <- paste(
      "ANOVA Interpretation:",
      "Degrees of Freedom (Df) for treatment groups:", anova_summary[[1]]$Df[1],
      "Degrees of Freedom (Df) for residuals:", anova_summary[[1]]$Df[2],
      "Sum of Squares (Sum Sq) for treatment:", anova_summary[[1]]$`Sum Sq`[1],
      "Sum of Squares (Sum Sq) for residuals:", anova_summary[[1]]$`Sum Sq`[2],
      "Mean Square (Mean Sq) for treatment:", anova_summary[[1]]$`Mean Sq`[1],
      "Mean Square (Mean Sq) for residuals:", anova_summary[[1]]$`Mean Sq`[2],
      "F value:", anova_summary[[1]]$`F value`[1],
      "P-value (Pr(>F)):", format.pval(p_value, digits=5),
      significance_statement,
      sep = "\n"
    )
    
    return(interpretation)
  })
  
  
  
  output$pearsonCorrelation <- renderText({
    if (is.null(data()) || input$statFeatures != "Pearson's Correlation Coefficient" || ncol(data()) != 2) {
      return(NULL)
    }
    
    if (is.numeric(data()[[1]]) && is.numeric(data()[[2]])) {
      corValue <- cor(data()[[1]], data()[[2]], method="pearson", use="complete.obs")
      return(paste("Pearson's Correlation Coefficient:", round(corValue, 2)))
    } else {
      return("Both columns should be numeric for Pearson's Correlation Coefficient.")
    }
  })
  
  output$chiSquaredInd <- renderText({
    if (is.null(data()) || input$statFeatures != "Chi-squared Test for Independence/Homogeneity") {
      return(NULL)
    }
    
    contingency_table <- table(data()[[1]], data()[[2]])  # For simplicity, using the first two columns
    chisq <- chisq.test(contingency_table)
    
    return(paste("Chi-squared Test for Independence:",
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
           },
           "Heatmap" = {
             if (is.null(data()) || ncol(data()) < 3) {
               return("Data not suitable for heatmap. Please ensure your data has at least three columns.")
             }
             #Columns
             ggplot(data(), aes(x = data()[[1]], y = data()[[2]], fill = data()[[3]])) + 
               geom_tile() + 
               scale_fill_gradient(low = "blue", high = "red") +
               labs(fill = "Intensity", x = "Region", y = "Month", title = "Heatmap") +
               theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels if needed
           },
           "Pie Chart" = {
             if (is.null(data()) || ncol(data()) < 2) {
               return("Data not suitable for pie chart. Please ensure your data has at least two columns.")
             }
             
             # Assuming first column is categories and second column contains the values
             pie_data <- data()[,1:2]
             colnames(pie_data) <- c("category", "value")
             
             ggplot(pie_data, aes(x = "", y = value, fill = category)) +
               geom_bar(width = 1, stat = "identity") +
               coord_polar(theta = "y") +
               theme_void() +
               theme(legend.title = element_blank()) +
               labs(fill = "Category")
           },
           "Correlation Matrix" = {
             # Filter only numeric data for the correlation matrix
             numeric_data <- data()[sapply(data(), is.numeric)]
             if (is.null(numeric_data) || ncol(numeric_data) < 2) {
               return("Data not suitable for correlation matrix. Please ensure your data has at least two numeric columns.")
             }
             
             # Calculate correlation matrix
             correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
             
             # Melt the correlation matrix for ggplot2
             correlation_data <- reshape2::melt(correlation_matrix)
             
             # Create a ggplot2 heatmap of the correlation matrix
             ggplot(correlation_data, aes(x = Var1, y = Var2, fill = value)) +
               geom_tile(color = "white") +
               scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               labs(fill = "Correlation", title = "Correlation Matrix")
           },
           "Density Plot" = {
             if (is.null(data()) || !is.numeric(data()[[1]])) {
               return("Data not suitable for density plot. Please ensure your data is numeric.")
             }
             ggplot(data(), aes(x = data()[[1]])) + 
               geom_density(fill = "blue", alpha = 0.5) +
               labs(x = names(data())[1], y = "Density", title = "Density Plot")
           },
           "Violin Plot" = {
             # Check if data is null or not suitable
             if (is.null(data()) || !is.numeric(data()[[1]])) {
               return("Data not suitable for violin plot. Please ensure your first column is numeric.")
             }
             
             # Handling Single Category
             ggplot(data(), aes(y = data()[[1]], x = as.factor(data()[[2]]))) +
               geom_violin(trim = FALSE) +
               labs(x = names(data())[2], y = names(data())[1], title = "Violin Plot")
           },
           "Faceted Plot" = {
             # Check for the appropriate number of columns and types
             if (ncol(data()) >= 3 && is.numeric(data()[[3]])) {
               # Assuming the third column contains numeric values and the second column contains the facet variable
               ggplot(data(), aes(x = data()[[1]], y = data()[[3]])) +
                 geom_point() +
                 facet_wrap(~ data()[[2]]) + # Facet by the second column
                 labs(title = "Faceted Plot", x = names(data())[1], y = names(data())[3])
             } else {
               return("Data not suitable for faceted plot. Ensure you have at least three columns with the third column being numeric.")
             }
           },
           "Time Series Graph" = {
             if (ncol(data()) >= 2 && is.numeric(data()[[2]])) {
               ts_plot <- ggplot(data(), aes_string(x = names(data())[1], y = names(data())[2])) +
                 geom_line() +
                 labs(x = "Time", y = "Value", title = "Time Series Graph")
               return(ts_plot)
             } else {
               return("Data not suitable for time series graph. Ensure the first column is time and the second column is numeric.")
             }
           },
           "Q-Q Plot" = {
             if (is.null(data()) || !is.numeric(data()[[1]])) {
               return("Data not suitable for Q-Q plot. Please ensure your data is numeric.")
             }
             qqplot <- ggplot(data(), aes(sample = data()[[1]])) +
               stat_qq() +
               stat_qq_line() +
               labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")
             print(qqplot)
           },
           "Scatterplot" = {
             if (ncol(data()) == 2 && is.numeric(data()[[1]]) && is.numeric(data()[[2]])) {
               xCol <- names(data())[1]
               yCol <- names(data())[2]
               ggplot(data(), aes_string(x=xCol, y=yCol)) + 
                 geom_point() + 
                 labs(x = xCol, y = yCol, title = "Scatterplot") +
                 geom_smooth(method = 'lm', se = FALSE, color = 'red')  # Linear regression line
             } else {
               return("Both columns should be numeric for a scatterplot.")
             }
           })
  })

  # Additional code for Basic Descriptive Statistics
  output$basicStats <- renderText({
    if (is.null(data()) || input$statFeatures != "Basic Descriptive Statistics") {
      return(NULL)
    }
    
    statsText <- ""
    for (colName in names(data())) {
      colData <- data()[[colName]]
      if (is.numeric(colData)) {
        meanVal <- mean(colData, na.rm = TRUE)
        modeVal <- as.numeric(names(sort(table(colData), decreasing=TRUE)[1]))
        medianVal <- median(colData, na.rm = TRUE)
        rangeVal <- range(colData, na.rm = TRUE)
        varianceVal <- var(colData, na.rm = TRUE)
        
        statsText <- paste(statsText, 
                           "Column: ", colName, "\n",
                           "Mean: ", round(meanVal, 2), "\n",
                           "Mode: ", round(modeVal, 2), "\n",
                           "Median: ", round(medianVal, 2), "\n",
                           "Range: ", round(rangeVal[1], 2), " to ", round(rangeVal[2], 2), "\n",
                           "Variance: ", round(varianceVal, 2), "\n\n")
      }
    }
    return(statsText)
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
