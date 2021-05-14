
# Define server function

shinyServer <- function(input, output, session) {
  
  #-------------------------------------------------
  # Read in inputted data and parse into tidy format
  #-------------------------------------------------
  
  # Single file
  
  singledat <- reactive({
    inFile <- input$userUpload
    return(inFile)
  })
  
  # Single file + metadata
  
  multidat <- reactive({
    inFile <- input$userUpload2
    return(inFile)
  })
  
  multidat_meta <- reactive({
    inFile <- input$userUpload2Meta
    return(inFile)
  })
  
  # Parse file(s) into tidy format
  
  tmp <- reactive({
    
    if(is.null(multidat())){
      
      if(endsWith(singledat()$name, ".xlsx")){
        mydat <- read_excel(singledat()$datapath)
      }
      
      if(endsWith(singledat()$name, ".xls")){
        mydat <- read_excel(singledat()$datapath)
      }
      
      if(endsWith(singledat()$name, ".csv")){
        mydat <- read_csv(singledat()$datapath)
      }
      
      if(endsWith(singledat()$name, ".txt")){
        mydat <- read_tsv(singledat()$datapath)
      }
      
    }
    return(mydat)
  })
  
  #---------------------
  # Feature calculations
  #---------------------
  
  featureMatrix <- reactive({
    
    # Create group to ID mapping
    
    if(!is.null(input$input_group_var)){
      group_labs <- featureMatrix %>%
        group_by(input$input_id_var, input$input_group_var) %>%
        summarise(counter = n()) %>%
        ungroup() %>%
        dplyr::select(-c(counter)) %>%
        rename(id = 1,
               group = 2)
    }
    
    # Calculate features
    
    featureMatrix <- calculate_features(tmp(), id_var = input$input_id_var, time_var = input$input_time_var, values_var = input$input_values_var, 
                                        feature_set = "catch22")
    
    # Re-join group labels
    
    if(!is.null(input$input_group_var)){
      featureMatrix <- featureMatrix %>%
        left_join(group_labs, by = c("id" = "id"))
    }
    
    return(featureMatrix)
  })
  
  #------------------ Low dim page --------------
  
  output$low_dim_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Draw graphic
    
    plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = "group", 
                       method = input$inputScaler, 
                       plot = TRUE)
  })
  
  output$raw_ts_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    if(input$selectID != "None"){
      
      # Filter to time-series of interest
      
      featureMatrixFilt <- featureMatrix %>%
        filter(id == input$selectID)
      
      # Draw graphic
      
      draw_ts(data = featureMatrix())
      
    } else{
      
      validate(
        need(featureMatrixFilt(), "Please select a unique ID to generate the time-series plot."
        )
      )
    }
  })
  
  #------------------ Classifier page -----------
  
  
  
  #------------------ Quality page --------------
  
  
  
  #------------------ Matrix page ---------------
  
  
  
  #------------------ About page ----------------
  
  
  
}