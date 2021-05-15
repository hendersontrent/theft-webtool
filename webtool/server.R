
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
          rename(id = all_of(input$input_id_var),
                 group = all_of(input$input_group_var)) %>%
          group_by(id, group) %>%
          summarise(counter = n()) %>%
          ungroup() %>%
          dplyr::select(-c(counter)) %>%
          mutate(id = as.character(id))
      }
      
      # Calculate features
      
      featureMatrix <- calculate_features(tmp(), id_var = input$input_id_var, time_var = input$input_time_var, 
                                          values_var = input$input_values_var, feature_set = "catch22") %>%
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
    
    thechoices <- featureMatrix()$id
    
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
      need(tmp(), "Please upload a dataset to get started."
      )
    )
    
    if(input$selectID != "None"){
      
      p <- tmp() %>%
        filter(id == input$selectID) %>%
        ggplot(aes(x = timepoint, y = values, group = 1,
                   text = paste('<b>ID:</b>', id,
                                '<br><b>Timepoint:</b>', timepoint,
                                '<br><b>Value:</b>', round(values, digits = 2)))) +
        geom_line(size = 1.05, colour = "#FFBA67") +
        theme_bw()
      
      # Convert to interactive graphic
      
      p_int <- ggplotly(p, tooltip = c("text")) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.2)) %>%
        config(displayModeBar = FALSE)
      
    } else{
      
      validate(
        need(featureMatrix(), "Please select a unique ID to generate the time-series plot."
        )
      )
    }
  })
  
  #------------------ Classifier page -----------
  
  
  
  #------------------ Quality page --------------
  
  
  
  #------------------ Matrix page ---------------
  
  
  
  #------------------ About page ----------------
  
  
  
}