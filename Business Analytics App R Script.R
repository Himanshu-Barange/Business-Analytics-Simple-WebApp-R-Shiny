library(shiny)
library(DT)
library(ggplot2)

ui <- navbarPage("",
                 
                 #### MAIN SECTION 1
                 
                 tabPanel("Home",
                          
                          sidebarPanel(
                            fileInput("dataset",
                                      "Upload File",
                                      accept = c(".csv", ".xlsx", ".xlsm"),
                                      placeholder = "accepts csv, xlsx, xlsm",
                                      width = "100%",
                                      
                            ),
                            radioButtons("head_or_tail", "View Type", c("Head", "Tail"), selected = "Head", width = "100%"),
                            selectInput("head_tail_rows", "Choose Number Of Rows", c(5,10,15,20), width = "100%"),
                            width = 3
                          ),
                          mainPanel(
                            tableOutput("head_tail_table")
                          )
                 ),
                 
                 #### MAIN SECTION 2
                 
                 tabPanel("Insights",
                          
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput("selected_columns", "Select Columns", multiple = TRUE, choices = NULL),
                              radioButtons("column_type", "Column Type", choices = c("Numeric", "Categorical"), selected = "Numeric"),
                              width = 3
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                id = "insights_tabs",
                                tabPanel("Numeric Summary", tableOutput("data_summary")),
                                tabPanel("Visual Summary", plotOutput("histogram_plot"))
                              )
                            )
                          ),
                          
                 ),
                 
                 
                 #### MAIN SECTION 3'
                 
                 tabPanel("Table",
                          uiOutput("table_message"),
                          dataTableOutput("main_table")
                 ),
                 
                 #### MAIN SECTION 4
                 
                 navbarMenu("More",
                            
                            tabPanel("Help",
                                     selectInput("tab_name", "Select Tab Name For Detail",
                                                 c("Home", "Insights", "Table"),
                                                 selected = "Home"),
                                     mainPanel(
                                       textOutput("help_text")
                                     )
                            ),
                            
                            
                            tabPanel("About",
                                     fluidPage(
                                       mainPanel(
                                         textOutput("about_text")
                                       )
                                     )
                            )
                 )
)

server <- function(input, output, session){
  
  # Initializing dataset_val as a reactiveVal with NULL
  dataset_val <- reactiveVal(NULL)
  
  # Observing file input and updating dataset_val when a file is uploaded
  observe({
    inFile <- input$dataset
    if (!is.null(inFile)) {
      ext <- tools::file_ext(inFile$datapath)
      data <- switch(ext,
                     csv = read.csv(inFile$datapath, stringsAsFactors = FALSE),
                     xlsx = readxl::read_xlsx(inFile$datapath),
                     xlsm = readxl::read_xlsx(inFile$datapath)
      )
      dataset_val(data)  # Update the reactiveVal with the loaded data
    }
  })
  
  output$head_tail_table <- renderTable({
    data <- dataset_val()
    if (is.null(data)) {
      return(NULL)
    }
    if(input$head_or_tail == "Head"){
      head(data, n = as.numeric(input$head_tail_rows))
    } else if(input$head_or_tail == "Tail"){
      tail(data, n = as.numeric(input$head_tail_rows))
    }
  }, rownames = TRUE)
  
  
  # ---------------------------------------------------------------
  # ---------------------------------------------------------------
  
  observe({
    if (!is.null(dataset_val())) {
      updateSelectInput(session, "selected_columns", choices = names(dataset_val()))
    }
  })
  
  observeEvent(c(input$column_type, dataset_val()), {
    data <- dataset_val()
    if (!is.null(data)) {
      if (input$column_type == "Numeric") {
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        updateSelectInput(session, "selected_columns", choices = numeric_cols)
      } else if (input$column_type == "Categorical") {
        categorical_cols <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
        updateSelectInput(session, "selected_columns", choices = categorical_cols)
      }
    }
  }, ignoreNULL = FALSE)
  
  output$data_summary <- renderTable({
    data <- dataset_val()
    if (is.null(data)) {
      return(NULL)
    }
    
    if (input$column_type == "Numeric") {
      data <- data[, sapply(data, is.numeric)]
    } else if (input$column_type == "Categorical") {
      data <- data[, sapply(data, function(col) is.factor(col) || is.character(col))]
    } 
    
    
    if (!is.null(input$selected_columns) && length(input$selected_columns) > 0) {
      data <- data[, input$selected_columns, drop = FALSE]
    }
    
    summarize_column <- function(column) {
      if (is.numeric(column)) {
        return(c(Mean = mean(column, na.rm = TRUE),
                 Median = median(column, na.rm = TRUE),
                 Min = min(column, na.rm = TRUE),
                 Max = max(column, na.rm = TRUE),
                 '1st Quartile' = quantile(column, 0.25, na.rm = TRUE),
                 '3rd Quartile' = quantile(column, 0.75, na.rm = TRUE)))
      } else if (is.factor(column) || is.character(column)) {
        freq <- table(column)
        most_frequent <- names(freq)[which.max(freq)]
        return(c('Number of Levels' = length(levels(column)),
                 'Most Frequent Level' = most_frequent,
                 'Frequency' = freq[most_frequent]))
      } else {
        return(summary(column))
      }
    }
    
    summary_list <- lapply(data, summarize_column)
    summary_df <- do.call(cbind, summary_list)
    
    # Set the row names of the summary dataframe to the measure names
    rownames(summary_df) <- names(summary_list[[1]])
    
    
    
    return(summary_df)
  }, rownames = TRUE)
  
  
  # -------------------------
  # 2nd tab Visual subtab
  
  output$histogram_plot <- renderPlot({
    
    # Check if the user is on the "Visual Summary" tab
    if(input$insights_tabs != "Visual Summary") {
      return(NULL)
    }
    
    data <- dataset_val()
    if (is.null(data) || is.null(input$selected_columns) || length(input$selected_columns) == 0) {
      return(NULL)
    }
    
    col_name <- input$selected_columns[1]
    column_data <- data[[col_name]]
    
    # Check if the selected column is numeric.
    if (is.numeric(column_data)) {
      # Create a histogram using ggplot2
      ggplot(data, aes_string(x = col_name)) + 
        geom_histogram(binwidth = (max(column_data, na.rm = TRUE) - min(column_data, na.rm = TRUE)) / 30, fill = "#0073C2", color = "#E1E1E1") +
        labs(title = paste("Histogram for", col_name), x = col_name, y = "Count") +
        theme_minimal()
    } else {
      # If not numeric, you can modify this part to handle categorical data or just show a message.
      plot(NA, xlim = c(0, 1), ylim = c(0, 1), ann = F, axes = F)
      text(0.5, 0.5, paste("Histogram not available for", col_name))
    }
  })
  
  
  # -----------------------------------------------
  # -----------------------------------------------
  
  
  output$table_message <- renderUI({
    data <- dataset_val()
    if (is.null(data)) {
      return(h3("Uploaded Data will be shown here in complete table form"))
    }
  })
  
  output$main_table <- renderDataTable({
    data <- dataset_val()
    if (is.null(data)) {
      return(NULL)
    }
    DT::datatable(data, options = list(pageLength = 25))
  })
  

  #-------------------------------------------------------
  #-------------------------------------------------------
  #-------------------------------------------------------
  
  # Help Tab:
  output$help_text <- renderText({
    if (input$tab_name == "Home") {
      return("This is the Home tab. Here you can upload your dataset and preview it.")
    } else if (input$tab_name == "Insights") {
      return("This is the Insights tab. Select columns and understand their statistical properties.")
    } else if (input$tab_name == "Table") {
      return("This is the Table tab. View the entire uploaded dataset here.")
    }
  })
  
  
  #--------------------------------------------------------
  #--------------------------------------------------------
  #--------------------------------------------------------
  
  # About Tab:
  output$about_text <- renderText({
    return("This is a Shiny App developed to provide insights on uploaded datasets. It allows for quick previews, statistical summaries, and full table views of data.")
  })
}

 

shinyApp(ui, server)