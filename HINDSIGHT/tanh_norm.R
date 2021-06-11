# not currently available 

tanh_norm <- function(data){
  
  data_norm <<- data
  m <<- mean(data)
  standard_dev <<- sd(data)
  
  data_norm <<- (1/2)*log(2*exp(0.02*(data-m)/standard_dev) + 1)
  
  return(data_norm)  
}