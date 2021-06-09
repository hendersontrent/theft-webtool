#' Produce a principal components analysis (PCA) on normalised feature values and render a bivariate plot to visualise it. Modified version of {theft}
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import tibble
#' @import Rtsne
#' @importFrom tidyr drop_na
#' @importFrom broom augment
#' @importFrom broom tidy
#' @importFrom stats prcomp
#' @param data a dataframe with at least 2 columns called 'names' and 'values'
#' @param is_normalised a Boolean as to whether the input feature values have already been scaled. Defaults to FALSE
#' @param id_var a string specifying the ID variable to group data on (if one exists). Defaults to NULL
#' @param group_var a string specifying the grouping variable that the data aggregates to. Defaults to NULL
#' @param method a rescaling/normalising method to apply. Defaults to 'RobustSigmoid'
#' @param plot a Boolean as to whether a bivariate plot should be returned or the calculation dataframe. Defaults to TRUE
#' @param highlight a binary of whether to call out a specific ID on the plot or not
#' @param id_filt the specific ID to filter the plot on
#' @param low_dim_method the low dimensional embedding method to use
#' @param perplexity the perplexity hyperparameter to use if t-SNE algorithm is selected. Defaults to 30
#' @return plotly object
#' @author Trent Henderson
#' @export
#'

plot_low_dimension <- function(data, is_normalised = FALSE, id_var = NULL, group_var = NULL, method = c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract"), 
                               plot = TRUE, highlight = c("No", "Yes"), id_filt = NULL, low_dim_method = c("PCA", "t-SNE"), perplexity = 30){
  
  # Make RobustSigmoid the default
  
  if(missing(method)){
    method <- "RobustSigmoid"
  } else{
    method <- match.arg(method)
  }
  
  expected_cols_1 <- "names"
  expected_cols_2 <- "values"
  the_cols <- colnames(data)
  '%ni%' <- Negate('%in%')
  
  if(expected_cols_1 %ni% the_cols){
    stop("data should contain at least two columns called 'names' and 'values'. These are automatically produced by feature calculations such as calculate_features(). Please consider running one of these first and then passing the resultant dataframe in to this function.")
  }
  
  if(expected_cols_2 %ni% the_cols){
    stop("data should contain at least two columns called 'names' and 'values'. These are automatically produced by feature calculations such as calculate_features(). Please consider running one of these first and then passing the resultant dataframe in to this function.")
  }
  
  if(!is.numeric(data$values)){
    stop("'values' column in data should be a numerical vector.")
  }
  
  if(!is.null(id_var) && !is.character(id_var)){
    stop("id_var should be a string specifying a variable in the input data that uniquely identifies each observation.")
  }
  
  if(!is.null(group_var) && !is.character(group_var)){
    stop("group_var should be a string specifying a variable in the input data that identifies an aggregate group each observation relates to.")
  }
  
  # Method selection
  
  the_methods <- c("z-score", "Sigmoid", "RobustSigmoid", "MinMax", "MeanSubtract")
  
  if(method %ni% the_methods){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  if(length(method) > 1){
    stop("method should be a single selection of 'z-score', 'Sigmoid', 'RobustSigmoid', 'MinMax' or 'MeanSubtract'")
  }
  
  #------------- Assign ID variable ---------------
  
  if(is.null(id_var)){
    stop("Data is not uniquely identifiable. Please add a unique identifier variable.")
  }
  
  if(!is.null(id_var)){
    data_id <- data %>%
      dplyr::rename(id = dplyr::all_of(id_var))
  }
  
  #------------- Normalise data -------------------
  
  if(is_normalised){
    normed <- data_id
  } else{
    normed <- data_id %>%
      dplyr::select(c(id, names, values)) %>%
      dplyr::group_by(names) %>%
      dplyr::mutate(values = normalise_feature_vector(values, method = method)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()
    
    if(nrow(normed) != nrow(data_id)){
      message("Filtered out rows containing NaNs.")
    }
  }
  
  #------------- Perform PCA ----------------------
  
  # Produce matrix
  
  dat <- normed %>%
    tidyr::pivot_wider(id_cols = id, names_from = names, values_from = values) %>%
    tibble::column_to_rownames(var = "id")
  
  # Remove any columns with all NAs to avoid whole dataframe being dropped
  
  dat_filtered <- dat[colSums(!is.na(dat)) > 0]
  
  # Drop any remaining rows with NAs
  
  dat_filtered <- dat_filtered %>%
    tidyr::drop_na()
  
  if(low_dim_method == "PCA"){
    
    # PCA calculation
    
    set.seed(123)
    
    fits <- dat_filtered %>%
      stats::prcomp(scale = FALSE)
    
    # Retrieve eigenvalues and tidy up variance explained for plotting
    
    eigens <- fits %>%
      broom::tidy(matrix = "eigenvalues") %>%
      dplyr::filter(PC %in% c(1,2)) %>% # Filter to just the 2 going in the plot
      dplyr::select(c(PC, percent)) %>%
      dplyr::mutate(percent = round(percent*100), digits = 1)
    
    eigen_pc1 <- eigens %>%
      dplyr::filter(PC == 1)
    
    eigen_pc2 <- eigens %>%
      dplyr::filter(PC == 2)
    
    eigen_pc1 <- paste0(eigen_pc1$percent,"%")
    eigen_pc2 <- paste0(eigen_pc2$percent,"%")
  } else{
    
    # tSNE calculation
    
    set.seed(123)
    
    tsneOut <- Rtsne::Rtsne(as.matrix(dat_filtered), perplexity = perplexity, max_iter = 5000, dims = 2,
                            check_duplicates = FALSE)
    
    # Retrieve 2-dimensional embedding and add in unique IDs
    
    id_ref <- dat_filtered %>%
      tibble::rownames_to_column(var = "id") %>%
      dplyr::select(c(id))
    
    fits <- data.frame(.fitted1 = tsneOut$Y[,1],
                       .fitted2 = tsneOut$Y[,2]) %>%
      dplyr::mutate(id = id_ref$id)
  }
  
  #------------- Output & graphic -----------------
  
  if(!is.null(group_var)){
    
    # Retrieve groups
    
    if(low_dim_method == "PCA"){
      fits <- fits %>%
        broom::augment(dat_filtered) %>%
        dplyr::rename(id = `.rownames`) %>%
        dplyr::mutate(id = as.factor(id)) %>%
        rename(.fitted1 = .fittedPC1,
               .fitted2 = .fittedPC2)
    } else{
      fits <- fits %>%
        dplyr::mutate(id = as.factor(id))
    }
    
    groups <- data_id %>%
      dplyr::rename(group_id = dplyr::all_of(group_var)) %>%
      dplyr::group_by(id, group_id) %>%
      dplyr::summarise(counter = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(counter)) %>%
      dplyr::mutate(id = as.factor(id))
    
    fits <- fits %>%
      dplyr::inner_join(groups, by = c("id" = "id"))
    
    # Define a nice colour palette
    # Palette from https://www.schemecolor.com/land-of-pastels.php
    
    available_colours <- c("#E494D3", "#87DCC0", "#88BBE4", "#998AD3", "#CDF1AF",
                           "#FDD1D2", "#FBEDE0", "#96D4CC", "#D4BBDD")
    
    # Draw plot
    
    if(highlight == "No"){
      
      if(low_dim_method == "PCA"){
        p <- fits %>%
          mutate(group_id = as.factor(group_id)) %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, colour = group_id,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>Group:</b>', group_id,
                                                    '<br><b>PC1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>PC2 Value:</b>', round(.fitted2, digits = 2))))
      } else{
        p <- fits %>%
          mutate(group_id = as.factor(group_id)) %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, colour = group_id,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>Group:</b>', group_id,
                                                    '<br><b>Dim 1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>Dim 2 Value:</b>', round(.fitted2, digits = 2))))
      }
      
      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5)
      } else{
        p <- p +
          ggplot2::geom_point(size = 2.25)
      }
      
      if(low_dim_method == "PCA"){
        p <- p +
          ggplot2::labs(x = paste0("PC 1"," (",eigen_pc1,")"),
                        y = paste0("PC 2"," (",eigen_pc2,")"),
                        colour = "Group")
      } else{
        p <- p +
          ggplot2::labs(x = "Dimension 1",
                        y = "Dimension 2",
                        colour = "Group")
      }
      
      p <- p +
        ggplot2::scale_color_manual(values = available_colours) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "bottom")
    }
    
    if(highlight == "Yes"){
      
      fits <- fits %>%
        mutate(group_id = as.factor(group_id))
      
      fitsfilt <- fits %>%
        mutate(id = as.character(id)) %>%
        filter(id != as.character(id_filt))
      
      idfilt <- fits %>%
        mutate(id = as.character(id)) %>%
        filter(id == as.character(id_filt))
      
      if(low_dim_method == "PCA"){
        p <- fitsfilt %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, colour = group_id,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>Group:</b>', group_id,
                                                    '<br><b>PC1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>PC2 Value:</b>', round(.fitted2, digits = 2)))) +
          ggplot2::geom_point(size = 1.5, alpha = 0.3) +
          ggplot2:::geom_point(data = idfilt, size = 4) +
          ggplot2::labs(x = paste0("PC 1"," (",eigen_pc1,")"),
                        y = paste0("PC 2"," (",eigen_pc2,")"),
                        colour = "Group")
      } else{
        p <- fitsfilt %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, colour = group_id,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>Group:</b>', group_id,
                                                    '<br><b>Dim 1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>Dim 2 Value:</b>', round(.fitted2, digits = 2)))) +
          ggplot2::geom_point(size = 1.5, alpha = 0.3) +
          ggplot2:::geom_point(data = idfilt, size = 4) +
          ggplot2::labs(x = "Dimension 1",
                        y = "Dimension 2",
                        colour = "Group")
      }
      p <- p +
        ggplot2::scale_color_manual(values = available_colours) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "bottom")
    }
  }
  
  if(is.null(group_var)){
    
    if(low_dim_method == "PCA"){
      fits <- fits %>%
        broom::augment(dat_filtered) %>%
        dplyr::rename(id = `.rownames`) %>%
        dplyr::mutate(id = as.factor(id)) %>%
        rename(.fitted1 = .fittedPC1,
               .fitted2 = .fittedPC2)
    } else{
      fits <- fits %>%
        dplyr::mutate(id = as.factor(id))
    }
    
    if(highlight == "No"){
      
      if(low_dim_method == "PCA"){
        p <- fits %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, group = 1,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>PC1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>PC2 Value:</b>', round(.fitted2, digits = 2))))
      } else{
        p <- fits %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, group = 1,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>Dim 1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>Dim 2 Value:</b>', round(.fitted2, digits = 2))))
      }
      
      if(nrow(fits) > 200){
        p <- p +
          ggplot2::geom_point(size = 1.5, colour = "#E494D3")
      } else{
        p <- p +
          ggplot2::geom_point(size = 2, colour = "#E494D3")
      }
      
      if(low_dim_method == "PCA"){
        p <- p +
          ggplot2::labs(x = paste0("PC 1"," (",eigen_pc1,")"),
                        y = paste0("PC 2"," (",eigen_pc2,")"))
      } else{
        p <- p +
          ggplot2::labs(x = "Dimension 1",
                        y = "Dimension 2")
      }
      p <- p +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank())
    }
    
    if(highlight == "Yes"){
      
      fitsfilt <- fits %>%
        mutate(id = as.character(id)) %>%
        filter(id != as.character(id_filt))
      
      idfilt <- fits %>%
        mutate(id = as.character(id)) %>%
        filter(id == as.character(id_filt))
      
      if(low_dim_method == "PCA"){
        p <- fitsfilt %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, group = 1,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>PC1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>PC2 Value:</b>', round(.fitted2, digits = 2)))) +
          ggplot2::geom_point(size = 1.5, alpha = 0.3, colour = "#E494D3") +
          ggplot2:::geom_point(data = idfilt, size = 4, colour = "#E494D3") +
          ggplot2::labs(x = paste0("PC 1"," (",eigen_pc1,")"),
                        y = paste0("PC 2"," (",eigen_pc2,")"))
      } else{
        p <- fitsfilt %>%
          ggplot2::ggplot(ggplot2::aes(x = .fitted1, y = .fitted2, group = 1,
                                       text = paste('<b>ID:</b>', id,
                                                    '<br><b>Dim 1 Value:</b>', round(.fitted1, digits = 2),
                                                    '<br><b>Dim 2 Value:</b>', round(.fitted2, digits = 2)))) +
          ggplot2::geom_point(size = 1.5, alpha = 0.3, colour = "#E494D3") +
          ggplot2:::geom_point(data = idfilt, size = 4, colour = "#E494D3") +
          ggplot2::labs(x = "Dimension 1",
                        y = "Dimension 2")
      }
      
      p <- p +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank())
    }
  }
  
  #-------- Convert to interactive graphic --------
  
  p_int <- ggplotly(p, tooltip = c("text")) %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.2)) %>%
    config(displayModeBar = FALSE)
  
  return(p_int)
}
