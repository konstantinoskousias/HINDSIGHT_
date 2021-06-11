reverse_prep <- function(prep_params, prep_option) {
    for (i in 1:dim(as.data.frame(y_train))[2]) {
        n = ceiling(i/dim(y_train)[2])
        if (i == 1) {
            yhat_train_back <- as.data.frame(yhat_train)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n] 
            yhat_val_back <- as.data.frame(yhat_val)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n]
            yhat_test_back <- as.data.frame(yhat_test)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n]
            
            y_train_back <- as.data.frame(y_train)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n]  
            y_val_back <- as.data.frame(y_val)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n]  
            y_test_back <- as.data.frame(y_test)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n]
        } else {
            yhat_train_back <- cbind(yhat_train_back, as.data.frame(yhat_train)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n])
            yhat_val_back <- cbind(yhat_val_back, as.data.frame(yhat_val)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n])
            yhat_test_back <- cbind(yhat_test_back, as.data.frame(yhat_test)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n])
            
            y_train_back <- cbind(y_train_back, as.data.frame(y_train)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n])
            y_val_back <- cbind(y_val_back, as.data.frame(y_val)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n])
            y_test_back <- cbind(y_test_back, as.data.frame(y_test)[i]*(prep_params[[2]][n] - prep_params[[1]][n]) + prep_params[[1]][n])
        }
    }
  
  y_train_back <<- y_train_back
  y_val_back <<- y_val_back
  y_test_back <<- y_test_back
  
  return(list(yhat_train_back, yhat_val_back, yhat_test_back))
}