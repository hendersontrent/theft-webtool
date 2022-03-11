
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
           mydat <- process_hctsa_file(matlabdat$datapath)
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
    
    if(input$covarianceSlider == "Yes"){
      show_covariance_param <- TRUE
    } else{
      show_covariance_param <- FALSE
    }
    
    if("group" %ni% colsList){
      
      plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = NULL, 
                         method = input$inputScaler, plot = TRUE, highlight = input$pca_highlighter, id_filt = input$selectID,
                         low_dim_method = input$low_dimSelect, perplexity = input$perplexitySlider, show_covariance = show_covariance_param)
    } else{
      
      plot_low_dimension(featureMatrix(), is_normalised = FALSE, id_var = "id", group_var = "group", 
                         method = input$inputScaler, plot = TRUE, highlight = input$pca_highlighter, id_filt = input$selectID,
                         low_dim_method = input$low_dimSelect, perplexity = input$perplexitySlider, show_covariance = show_covariance_param)
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
              geom_line(colour = "#666666") +
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
          geom_line(colour = "#666666") +
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
          geom_line(colour = "#666666") +
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
  
  #------------------
  # ID x Feature plot
  #------------------
  
  output$id_by_feat_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Render plot
    
    plot_feature_matrix(data = featureMatrix(), id_var = "id", is_normalised = FALSE, method = input$inputScaler2, interactive = TRUE)
  })
  
  #-----------------------
  # Feature x Feature plot
  #-----------------------
  
  output$feat_by_feat_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Render plot
    
    plot_feature_correlations(data = featureMatrix(), is_normalised = FALSE, id_var = "id", names_var = "names", values_var = "values",
                             method = input$inputScaler2, interactive = TRUE)
  })
  
  #-------------------------------
  # Time Series x Time Series plot
  #-------------------------------
  
  output$id_by_id_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(tmp(), "Please upload a dataset to get started."
      )
    )
    
    # Render plot
    
    plot_ts_correlations(data = tmp(), is_normalised = FALSE, id_var = "id", values_var = "values",
                         method = input$inputScaler2, cor_method = input$corMethod, interactive = TRUE)
  })
  
  #------------------ Classifier page ------------------
  
  #------------------------
  # Multivariate classifier
  #------------------------
  
  multivariateOutputs <- eventReactive(input$runMultivariate, {
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset to get started."
      )
    )
    
    # Set up set specification
    
    if(input$bysetSelect == binaries[2]){
      by_set <- TRUE
    } else{
      by_set <- FALSE
    }
    
    if(input$kfoldSelect == binaries[2]){
      use_k_fold <- TRUE
    } else{
      use_k_fold <- FALSE
    }
    
    if(input$empiricalnullSelect == binaries[2]){
      use_empirical_null <- TRUE
    } else{
      use_empirical_null <- FALSE
    }
    
    # Fit model(s) and draw graphic
    
    multivariateOutputList <- fit_multivariate_classifier(featureMatrix(), id_var = "id", group_var = "group",
                                                       by_set = by_set, test_method = input$classifierSelect,
                                                       use_k_fold = use_k_fold, num_folds = input$kfoldSlider, 
                                                       use_empirical_null = use_empirical_null, null_testing_method = input$nullmethodSelect,
                                                       p_value_method = input$pvaluemethodSelect, num_permutations = input$permutationSlider)
    
    return(multivariateOutputList)
  })
  
  # Get plot
  
  output$multivariatePlot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(multivariateOutputs(), "Please upload a dataset and specify controls to get started."
      )
    )
    
    # Render plot
    
    multivariateOutputs()$FeatureSetResultsPlot
  })
  
  # Get table summary
  
  output$multivariateTable <- renderTable({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(multivariateOutputs(), "Please upload a dataset and specify controls to get started."
      )
    )
    
    if(input$empiricalnullSelect == "No"){
      
      multivariateOutputs()$RawClassificationResults %>%
        filter(method %in% c("catch22", "feasts", "tsfeatures")) %>%
        dplyr::select(c(method, statistic, classifier_name, statistic_name)) %>%
        rename(Method = method,
               `Classification Accuracy` = statistic,
               `Classifier Name` = classifier_name,
               `Statistic Name` = statistic_name)
      
    } else{
      multivariateOutputs()$TestStatistics %>%
        rename(Method = method,
               `Classification Accuracy` = statistic_value,
               `p value` = p_value,
               `Classifier Name` = classifier_name,
               `Statistic Name` = statistic_name)
    }
  })
  
  #----------------------
  # Univariate classifier
  #----------------------
  
  # Fit model
  
  univariateOutputs <- eventReactive(input$runUnivariate, {
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(featureMatrix(), "Please upload a dataset and specify controls to get started."
      )
    )
    
    # Set up set specification
    
    if(input$kfoldSelect == binaries[2]){
      use_k_fold <- TRUE
    } else{
      use_k_fold <- FALSE
    }
    
    if(input$empiricalnullSelect == binaries[2]){
      use_empirical_null <- TRUE
    } else{
      use_empirical_null <- FALSE
    }
    
    if(input$poolednullSelect == binaries[2]){
      pool_empirical_null <- TRUE
    } else{
      pool_empirical_null <- FALSE
    }
    
    # Catch cases where user makes an error in the browser to avoid red message
    
    if(input$nullmethodSelect == "model free shuffles" && pool_empirical_null){
      pool_empirical_null <- FALSE
    }
    
    # Fit model(s) and draw graphic
    
    univariateOutputList <- compute_top_features(featureMatrix(), id_var = "id", group_var = "group",
                                                 num_features = input$numFeaturesSlider, normalise_violin_plots = FALSE, test_method = input$classifierSelect,
                                                 method = "z-score", cor_method = input$corMethodUnivariate, 
                                                 use_k_fold = use_k_fold, num_folds = input$kfoldSlider,
                                                 use_empirical_null = use_empirical_null, null_testing_method = input$nullmethodSelect,
                                                 p_value_method = input$pvaluemethodSelect, num_permutations = input$permutationSlider, 
                                                 pool_empirical_null = pool_empirical_null)
    
    return(univariateOutputList)
  })
  
  # Draw top feature correlation plot
  
  output$top_feature_cor_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(univariateOutputs(), "Please upload a dataset and specify controls to get started."
      )
    )
    
    # Render plot
    
    univariateOutputs()$FeatureFeatureCorrelationPlot
  })
  
  # Draw top feature violin plot
  
  output$top_feature_violin_plot <- renderPlotly({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(univariateOutputs(), "Please upload a dataset and specify controls to get started."
      )
    )
    
    # Render plot
    
    univariateOutputs()$ViolinPlots
  })
  
  # Get table summary
  
  output$univariateTable <- renderTable({
    
    # Account for lack of data upload to avoid error message
    
    validate(
      need(univariateOutputs(), "Please upload a dataset and specify controls to get started."
      )
    )
    
    univariateOutputs()$ResultsTable %>%
      rename(Feature = feature,
             `Classification Accuracy` = statistic_value,
             `p value` = p_value,
             `Classifier Name` = classifier_name,
             `Statistic Name` = statistic_name)
  })
  
}