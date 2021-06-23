#' Function to load in hctsa formatted MATLAB files
#' 
#' @param data the MATLAB file to parse
#' @return an object of class dataframe
#' @author Trent Henderson
#' 

welcome_mat <- function(data){
  
  # Read MATLAB file into list
  
  d <- readMat(data)
  
  theNames <- names(d)
  correctNames <- c("timeSeriesData", "labels", "keywords")
  
  if(length(theNames) != 3){
    stop("3 variables should be 'timeSeriesData', 'labels', and 'keywords'.")
  }
  
  if(setequal(theNames, correctNames) == FALSE){
    stop("3 variables should be 'timeSeriesData', 'labels', and 'keywords'.")
  }
  
  # Parse 3 separate variables into single dataframe
  
  tmp <- rbindlist(d$keywords, use.names = TRUE)
  tmpVec <- tmp$V1
  tmp1 <- rbindlist(d$labels, use.names = TRUE)
  tmp1Vec <- tmp1$V1
  tmp2 <- d$timeSeriesData
  
  indices <- seq(from = 1, to = length(tmp2), by = 1)
  storage <- list()
  
  for(i in indices){
    tmpList <- tmp2[[i]][[1]]
    tmpListDat <- as.data.frame(tmpList)
    
    tmpList2 <- tmpListDat %>%
      rename(values = V1) %>%
      mutate(timepoint = row_number()) %>%
      mutate(id = tmpVec[i],
             group = tmp1Vec[i])
    
    storage[[i]] <- tmpList2
  }
  
  myData <- rbindlist(storage, use.names = TRUE)
  
  return(myData)
}
