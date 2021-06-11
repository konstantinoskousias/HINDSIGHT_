# not currently available 

standardization <- function(data) {
  
    data_stand <- data
    
    m <<- mean(data)
    sdev <<- sd(data)
    data_stand <<- (data - mean(data))/sd(data)
    
    return(data_stand)
}