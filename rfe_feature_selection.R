#-------------------- RFE --------------------#

rfe_feature_selection <- function(input_df_list, retain = 2) {
  
    message('RFE ...')
    # RFE for each dataset
    feature_mat <- list()
    for(i in 1:length(input_df_list)) {
        temp <- data.frame(input_df_list[[i]])
        print(names(temp))
        control <- rfeControl(functions= rfFuncs, method="cv", number=10, verbose = FALSE)
        results <- rfe(temp[2:dim(temp)[2]], temp[,1], rfeControl = control, sizes = retain)
        feature_mat <- cbind(feature_mat, results$optVariables[1:retain])
    }
    
    # A simple majority vote implementation
    feature_names <- names(table(as.character(feature_mat)))
    
    feature_index <- c()
    for(i in 1:length(feature_names)) {
        feature_index <- c(feature_index, which(colnames(input_df_list[[1]]) == feature_names[i]))
    }
    
    print(feature_index)
    
    # Return a vector containing the indexes(sorted) of the proposed features 
    return(sort(feature_index, decreasing = FALSE))
}
