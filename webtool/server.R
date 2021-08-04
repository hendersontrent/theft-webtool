
# Define server function

shinyServer <- function(input, output, session) {
  
  # Adjust maximum file size upload. Currently 300MB
  
  options(shiny.maxRequestSize = 300*1024^2)
  
  #-------------------------------------------------
  # Read in inputted data and parse into tidy format
  #-------------------------------------------------
  
  # Parse file(s) into tidy format with contingency on button
  
  tmp <- eventReactive(input$run, {
    
    if(!is.null(input$userUpload) && !is.null(input$userUpload2) && is.null(input$userUpload3)){
      
    } else if(!is.null(input$userUpload) && is.null(input$userUpload2) && is.null(input$userUpload3)){
        
        singledat <- input$userUpload
        
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
        
        return(mydat)
        
       } else if(!is.null(input$userUpload2) && !is.null(input$userUpload2Meta) && is.null(input$userUpload) && is.null(input$userUpload3)){
         
         multidat <- input$userUpload2
         multidat_meta <- input$userUpload2Meta
      
        validate(
          need(multidat, "Please upload a dataset to get started."
          ),
          need(multidat_meta, "Please upload a dataset to get started."
          )
        )
      
        # Data
        
        if(endsWith(multidat$name, ".xlsx")){
          widedat <- read_excel(multidat$datapath)
        }
        
        if(endsWith(multidat$name, ".xls")){
          widedat <- read_excel(multidat$datapath)
        }
        
        if(endsWith(multidat$name, ".csv")){
          widedat <- read_csv(multidat$datapath)
        }
        
        if(endsWith(multidat$name, ".txt")){
          widedat <- read_tsv(multidat$datapath)
        }
        
        widedat <- widedat %>%
          dplyr::select(-c(X1))
        
        # Metadata
        
        if(endsWith(multidat_meta$name, ".xlsx")){
          metadat <- read_excel(multidat_meta$datapath)
        }
        
        if(endsWith(multidat_meta$name, ".xls")){
          metadat <- read_excel(multidat_meta$datapath)
        }
        
        if(endsWith(multidat_meta$name, ".csv")){
          metadat <- read_csv(multidat_meta$datapath)
        }
        
        if(endsWith(multidat_meta$name, ".txt")){
          metadat <- read_tsv(multidat_meta$datapath)
        }
        
        metadat <- metadat %>%
          dplyr::select(-c(X1))
        
        # Merge
        
        mydat <- widedat %>%
          cbind(metadat)
          
        drop <- c("X1")
        mydat <- mydat[,!(names(mydat) %in% drop)]
          
        if(str_detect(input$input_group_var_multi, " ")){
          mydat <- mydat %>%
            rename(id = all_of(input$input_id_var_multi)) %>%
            pivot_longer(!id, names_to = "timepoint", values_to = "values") %>%
            mutate(timepoint = as.numeric(timepoint))
          } else{
          mydat <- mydat %>%
            rename(id = all_of(input$input_id_var_multi),
                   group = all_of(input$input_group_var_multi)) %>%
            pivot_longer(!c(id, group), names_to = "timepoint", values_to = "values") %>%
            mutate(timepoint = as.numeric(timepoint))
          }
        return(mydat)
       } else if(!is.null(input$userUpload3) && is.null(input$userUpload2Meta) && is.null(input$userUpload) && is.null(input$userUpload2)){
         
         matlabdat <- input$userUpload3
         
         validate(
           need(matlabdat, "Please upload a dataset to get started."
           )
         )
         
         if(endsWith(matlabdat$name, ".mat")){
           mydat <- welcome_mat(matlabdat$datapath)
         }
         return(mydat)
       } else{
    }
  })
  
  #---------------------
  # Feature calculations
  #---------------------
  
  featureMatrix <- reactive({
    
    validate(
      need(tmp(), "Please upload a dataset to get started."
      )
    )
    
    if(!is.null(input$userUpload) && is.null(input$userUpload2) && is.null(input$userUpload3)){
      if(str_detect(input$input_id_var, " ") || str_detect(input$input_time_var, " ") || str_detect(input$input_values_var, " ")){
        
      } else {
        
        # Catch non-numeric cases for time and values to simplify things
        
        tmpTest <- tmp() %>%
          rename(timepoint = all_of(input$input_time_var),
                 values = all_of(input$input_values_var)) %>%
          mutate(timepoint = as.numeric(as.character(timepoint)),
                 values = as.numeric(as.character(values)))
          
          if(sum(is.na(tmpTest$timepoint)) > 0 | sum(is.na(tmpTest$values)) > 0){
            return()
          } else{
            
            if(!str_detect(input$input_group_var, " ")){
              tmp2 <- tmp() %>%
                rename(id = all_of(input$input_id_var),
                       group = all_of(input$input_group_var),
                       timepoint = all_of(input$input_time_var),
                       values = all_of(input$input_values_var))
            } else{
              tmp2 <- tmp() %>%
                rename(id = all_of(input$input_id_var),
                       timepoint = all_of(input$input_time_var),
                       values = all_of(input$input_values_var))
            }
          }
        
        # Calculate features
        
        if(!str_detect(input$input_group_var, " ")){
        featureMatrix <- calculate_features(tmp2, id_var = "id", time_var = "timepoint", 
                                            values_var = "values", group_var = "group", feature_set = input$feature_set)
        } else{
          featureMatrix <- calculate_features(tmp2, id_var = "id", time_var = "timepoint", 
                                              values_var = "values", feature_set = input$feature_set)
        }
        return(featureMatrix)
      }
    } else if(is.null(input$userUpload) && !is.null(input$userUpload2) && is.null(input$userUpload3)){
      if(str_detect(input$input_id_var_multi, " ") ){
        
      } else {
        
        # Catch non-numeric cases for time and values to simplify things
        
        tmpTest <- tmp() %>%
          mutate(values = as.numeric(as.character(values)))
        
        if(sum(is.na(tmpTest$values)) > 0){
          return()
        } else{
        }
        
        # Calculate features
        
        if(!str_detect(input$input_group_var_multi, " ")){
        
        featureMatrix <- calculate_features(tmp(), id_var = "id", time_var = "timepoint", 
                                            values_var = "values", group_var = "group", feature_set = input$feature_set)
        } else{
          featureMatrix <- calculate_features(tmp(), id_var = "id", time_var = "timepoint", 
                                              values_var = "values", feature_set = input$feature_set)
        }
        return(featureMatrix)
      }
    } else if (!is.null(input$userUpload3) && is.null(input$userUpload2Meta) && is.null(input$userUpload) && is.null(input$userUpload2)){
      featureMatrix <- calculate_features(tmp(), id_var = "id", time_var = "timepoint", 
                                          values_var = "values", group_var = "group", feature_set = input$feature_set)
      
      return(featureMatrix)
    } else{
      
    }
  })
  
  #--------------------
  # Download calculated
  # feature dataframe
  #--------------------
  
  output$feature_download <- downloadHandler(
    filename = "feature_calculations.csv",
    content = function(filename){
      write.csv(featureMatrix(), filename)
    }
  )
  
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
    
    colsList <- colnames(featureMatrix())
    '%ni%' <- Negate('%in%')
    
    if("group" %ni% colsList){
      
      plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = NULL, 
                         method = input$inputScaler, plot = TRUE, highlight = input$pca_highlighter, id_filt = input$selectID,
                         low_dim_method = input$low_dimSelect, perplexity = input$perplexitySlider)
    } else{
      
      plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = "group", 
                         method = input$inputScaler, plot = TRUE, highlight = input$pca_highlighter, id_filt = input$selectID,
                         low_dim_method = input$low_dimSelect, perplexity = input$perplexitySlider)
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
    
    # Define a nice colour palette
    
    available_colours <- c("#ef6ade", "#75eab6", "#2a6866", "#14bae1", "#ad0599", 
                           "#513886", "#7f73ed", "#e4b8ec", "#0b29d0", "#3986da")
    
    if(input$selectID != "None"){
      if(!is.null(input$userUpload) && is.null(input$userUpload2) && is.null(input$userUpload3)){
        if(str_detect(input$input_id_var, " ") || str_detect(input$input_time_var, " ") || str_detect(input$input_values_var, " ")){
        } else{
            p <- tmp() %>%
              rename(id = all_of(input$input_id_var)) %>%
              rename(timepoint = all_of(input$input_time_var)) %>%
              rename(values = all_of(input$input_values_var)) %>%
              filter(id == input$selectID) %>%
              ggplot(aes(x = timepoint, y = values, group = 1,
                         text = paste('<b>ID:</b>', id,
                                      '<br><b>Timepoint:</b>', timepoint,
                                      '<br><b>Value:</b>', round(values, digits = 2)))) +
              geom_line(colour = "#3986da") +
              labs(x = "Timepoint",
                   y = "Value") +
              theme_bw()
          }
      } else if(!is.null(input$userUpload2) && !is.null(input$userUpload2Meta) && is.null(input$userUpload) && is.null(input$userUpload3)){
        p <- tmp() %>%
          rename(id = all_of(input$input_id_var_multi)) %>%
          filter(id == input$selectID) %>%
          ggplot(aes(x = timepoint, y = values, group = 1,
                     text = paste('<b>ID:</b>', id,
                                  '<br><b>Timepoint:</b>', timepoint,
                                  '<br><b>Value:</b>', round(values, digits = 2)))) +
          geom_line(colour = "#3986da") +
          labs(x = "Timepoint",
               y = "Value") +
          theme_bw()
      } else if(!is.null(input$userUpload3) && is.null(input$userUpload2Meta) && is.null(input$userUpload) && is.null(input$userUpload2)){
        p <- tmp() %>%
          filter(id == input$selectID) %>%
          ggplot(aes(x = timepoint, y = values, group = 1,
                     text = paste('<b>ID:</b>', id,
                                  '<br><b>Timepoint:</b>', timepoint,
                                  '<br><b>Value:</b>', round(values, digits = 2)))) +
          geom_line(colour = "#3986da") +
          labs(x = "Timepoint",
               y = "Value") +
          theme_bw()
      } else{
        
      }
      
      # Convert to interactive graphic
      
      p_int <- ggplotly(p, tooltip = c("text")) %>%
        config(displayModeBar = FALSE)
      
    } else{
      
      validate(
        need(tmp(), "Please select a unique ID to generate the time-series plot."
        )
      )
    }
  })
  
  #------------------ Quality page --------------
  
  output$data_qual_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    plot_quality_matrix(data = featureMatrix())
   })
  
  #------------------ Matrix page ---------------
  
  # ID x Feature plot
  
  output$discrim_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    if(input$feature_set != "catch22"){
      return()
    } else{
      
      # Draw plot
      
      colsList <- colnames(featureMatrix())
      '%ni%' <- Negate('%in%')
      
      if("group" %ni% colsList){
      } else {
        
        # Define a nice colour palette
        
        available_colours <- c("#ef6ade", "#75eab6", "#2a6866", "#14bae1", "#ad0599", 
                               "#513886", "#7f73ed", "#e4b8ec", "#0b29d0", "#3986da")
        
        # Draw graphic
        
        p <- featureMatrix() %>%
          mutate(group = as.factor(group)) %>%
          ggplot(aes(x = group, y = values, colour = group)) +
          geom_violin() +
          geom_point(size = 1, alpha = 0.7, position = position_jitter(w = 0.05)) +
          labs(x = "Group",
               y = "Value") +
          scale_color_manual(values = available_colours) +
          theme_bw() +
          theme(panel.grid.minor = element_blank(),
                legend.position = "none",
                strip.background = element_blank()) +
          facet_wrap(~names, ncol = 4, scales = "free_y")
        
        # Convert to interactive graphic
        
        p_int <- ggplotly(p, tooltip = c("text")) %>%
          layout(legend = list(orientation = "h", x = 0, y = -0.2)) %>%
          config(displayModeBar = FALSE)
     }
    }
  })
  
  # ID x Feature plot
  
  output$feat_mat_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Render plot
    
    plot_connectivity_matrix(data = featureMatrix(), is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values",
                             method = input$inputScaler2)
  })
  
  # Feature x Feature plot
  
  output$id_by_feat_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Render plot
    
    plot_feature_matrix(data = featureMatrix(), id_var = "id", is_normalised = FALSE, method = input$inputScaler2)
  })
  
  #------------------ Classifier page -----------
  
  
  
}