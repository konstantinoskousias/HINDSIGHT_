normalization <- function(data) {
    
    minimums = list()
    maximums = list()
    
    data_norm <- data
    
    maxim <<- max(data)
    minim <<- min(data)
    data_norm <- (data - min(data))/(max(data) - min(data))
  
    return(data_norm)
}