
# Define server function

shinyServer <- function(input, output, session) {
  
  # Adjust maximum file size upload. Currently 300MB
  
  options(shiny.maxRequestSize = 300*1024^2)
  
  #-------------------------------------------------
  # Read in inputted data and parse into tidy format
  #-------------------------------------------------
  
  # Parse file(s) into tidy format
  
  tmp <- reactive({
    
    singledat <- input$userUpload
    multidat <- input$userUpload2
    multidat_meta <- input$userUpload2Meta
    
    if(is.null(multidat)){
      
      validate(
        need(singledat, "Please upload a dataset to get started."
        )
      )
      
      if(endsWith(singledat$name, ".xlsx")){
        mydat <- read_excel(singledat$datapath)
      }
      
      if(endsWith(singledat$name, ".xls")){
        mydat <- read_excel(singledat$datapath)
      }
      
      if(endsWith(singledat$name, ".csv")){
        mydat <- read_csv(singledat$datapath)
      }
      
      if(endsWith(singledat$name, ".txt")){
        mydat <- read_tsv(singledat$datapath)
      }
      
    }
    return(mydat)
  })
  
  #---------------------
  # Feature calculations
  #---------------------
  
  featureMatrix <- reactive({
    
    if(str_detect(input$input_id_var, " ") | str_detect(input$input_group_var, " ") |
       str_detect(input$input_time_var, " ") | str_detect(input$input_values_var, " ")){
      
    } else {
      
      # Create group to ID mapping
      
      if(!str_detect(input$input_group_var, " ")){
        group_labs <- tmp() %>%
          group_by(input$input_id_var, input$input_group_var) %>%
          summarise(counter = n()) %>%
          ungroup() %>%
          dplyr::select(-c(counter)) %>%
          rename(id = 1,
                 group = 2)
      }
      
      # Calculate features
      
      featureMatrix <- calculate_features(tmp(), id_var = input$input_id_var, time_var = input$input_time_var, values_var = input$input_values_var, 
                                          feature_set = "catch22") %>%
        mutate(id = as.character(id))
      
      # Re-join group labels
      
      if(!str_detect(input$input_group_var, " ")){
        featureMatrix <- featureMatrix %>%
          left_join(group_labs, by = c("id" = "id"))
      }
      
      return(featureMatrix)
    }
  })
  
  #-----------------
  # ID filter update
  #-----------------
  
  observeEvent(featureMatrix(), {
    
    default <- c("None")
    thechoices <- append(default, featureMatrix()$id)
    
    updateSelectInput(session, "selectID", choices = thechoices)
  })
  
  #------------------ Low dim page --------------
  
  #---------
  # PCA plot
  #---------
  
  output$low_dim_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Draw graphic
    
    if(str_detect(input$input_group_var, " ")){
      
      plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = NULL, 
                         method = input$inputScaler, plot = TRUE)
    } else{
      
      plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = "group", 
                         method = input$inputScaler, plot = TRUE)
    }
  })
  
  #-----------------
  # Time-series plot
  #-----------------
  
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
      
      draw_ts(data = featureMatrixFilt)
      
    } else{
      
      validate(
        need(featureMatrixFilt, "Please select a unique ID to generate the time-series plot."
        )
      )
    }
  })
  
  #------------------ Classifier page -----------
  
  
  
  #------------------ Quality page --------------
  
  
  
  #------------------ Matrix page ---------------
  
  
  
  #------------------ About page ----------------
  
  
  
}