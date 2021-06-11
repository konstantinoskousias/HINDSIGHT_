#-------------------- Preprocessing function for HINDSIGHT++ --------------------#
# Note: Standardization function is under work -- in comments

preprocessing <- function(df, nlags, ntimesteps, nfeatures, split) {
    for (i in 1:nfeatures) {
        for (j in 1:length(df)) {
            #if (prep_option == "normalization"){
            temp = normalization(df[[j]][[i]])
            if(i == 1) {
                if(j == 1){
                    maximums <- c(maxim)
                    minimums <- c(minim)
                } else {
                    minimums <- c(minimums, minim)
                    maximums <- c(maximums, maxim)
                }
            }
            # } else {
            #       temp = standardization(df[[j]][[i]])
            #       if (i == 1) {
            #           if (j == 1) {
            #               means <- c(m)
            #               sdevs <- c(sdev)
            #           } else {
            #               means <- c(means, m)
            #               sdevs <- c(sdevs, sdev)
            #           }
            #       }
            # }
            
            temp = as.data.frame(temp)
      
            df_train = temp[1:as.integer(dim(as.data.frame(temp))[1]*(split-valsplit)), ]
            df_val = temp[as.integer(dim(temp)[1]*(split-valsplit) + 1):as.integer(dim(as.data.frame(temp))[1]*(split)), ]
            df_test = temp[as.integer(dim(temp)[1]*split + 1):dim(temp)[1],]
            
            
            df_train = time_series(na.omit(as.data.frame((df_train))), nlags + ntimesteps -1, 1)
            df_val = time_series(na.omit(as.data.frame((df_val))), nlags + ntimesteps -1, 1)
            df_test = time_series(na.omit(as.data.frame((df_test))), nlags + ntimesteps -1, 1)
            
            starting_point = ncol(df_train)-ntimesteps+1 
            
            if (i == 1) {
                if (j == 1) {
                    X_train <<- as.data.frame(df_train[, 1:starting_point-1])
                    y_train <<- as.data.frame(df_train[, starting_point:ncol(df_train)])
                    
                    X_val <<- as.data.frame(df_val[, 1:starting_point-1])
                    y_val <<- as.data.frame(df_val[, starting_point:ncol(df_val)])
                    
                    X_test <<- as.data.frame(df_test[, 1:starting_point-1])
                    y_test <<- as.data.frame(df_test[, starting_point:ncol(df_test)])
                } else {
                      y_train <<- cbind(y_train, as.data.frame(df_train[, starting_point:ncol(df_train)]))
                      y_val <<- cbind(y_val, as.data.frame(df_val[, starting_point:ncol(df_val)]))
                      y_test <<- cbind(y_test, as.data.frame(df_test[, starting_point:ncol(df_test)]))
            
                      X_train <<- cbind(X_train, as.data.frame(df_train[, 1:starting_point-1]))
                      X_val <<- cbind(X_val, as.data.frame(df_val[, 1:starting_point-1]))
                      X_test <<- cbind(X_test, as.data.frame(df_test[, 1:starting_point-1]))
                }
            } else {
                  X_train <<- cbind(X_train, as.data.frame(df_train[, 1:starting_point-1]))
                  X_val <<- cbind(X_val, as.data.frame(df_val[, 1:starting_point-1]))
                  X_test <<- cbind(X_test, as.data.frame(df_test[, 1:starting_point-1]))
            }
        }    
    }
    
    # split data in training/validation/testing
    X_train <<- matrix(unlist(X_train), ncol = nlags*length(df)*nfeatures, byrow = FALSE)
    X_val <<- matrix(unlist(X_val), ncol = nlags*length(df)*nfeatures, byrow = FALSE)
    X_test <<- matrix(unlist(X_test), ncol = nlags*length(df)*nfeatures, byrow = FALSE)
    X_train <<- array(X_train, dim = c(nrow(X_train), nlags, nfeatures*length(df)))
    X_val <<- array(X_val, dim = c(nrow(X_val), nlags, nfeatures*length(df)))
    X_test <<- array(X_test, dim = c(nrow(X_test), nlags, nfeatures*length(df)))
    
    y_train <<- matrix(unlist(y_train), ncol = ntimesteps*length(df), byrow = FALSE)
    y_val <<- matrix(unlist(y_val), ncol = ntimesteps*length(df), byrow = FALSE)
    y_test <<- matrix(unlist(y_test), ncol = ntimesteps*length(df), byrow = FALSE)
    y_train <<- array(y_train, dim = c(nrow(y_train), ntimesteps, length(df)))
    y_val <<- array(y_val, dim = c(nrow(y_val), ntimesteps, length(df)))
    y_test <<- array(y_test, dim = c(nrow(y_test), ntimesteps, length(df)))
    
    #if (prep_option == "normalization") {
    return (list(minimums, maximums))
    #} else {
    #    return (c(means, sdevs))
    #}
}

