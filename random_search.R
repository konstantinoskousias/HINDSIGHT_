#-------------------- Random Search --------------------#

random_search <- function(nlags, network_type, units, units1, units2, units3, lr, nepochs, bs, nlayers, niter, opt) {

    cat("\nInitiating Random Search optimization method...\n")
    
  
    gridtable <<- data.frame(units1 = integer(), 
                             units2 = integer(),
                             units3 = integer(),
                             lr = numeric(),
                             nepochs = integer(),
                             bs = integer(),
                             nlayers = integer(),
                             mae = numeric())
      
    RS_tic = proc.time() # Start timer
      
    for (i in 1:niter) {
        units1 = sample(2^(4:10),1)
        units2 = sample(2^(4:10),1) 
        units3 = sample(2^(4:10),1)
        lr = sample(c(0.00001, 0.0001, 0.001, 0.01),1)
        nepochs = sample(c(50,60,70,80,90,100),1)
        bs = sample(2^(3:7),1)
        nlayers = sample(1:4, 1)
        
        lstm_tic = proc.time() # Start timer
        
        if (all(network_type == "Vanilla")) {    
            results = lstm(nlags = nlags, units = units, units1 = units1, units2 = units2, units3 = units3, nepochs = nepochs, lr = lr,
                           bs = bs, nlayers = nlayers, opt = opt)
        } else {
            results = bidi_lstm(nlags = nlags, units = units, units1 = units1, units2 = units2, units3 = units3, nepochs = nepochs, lr = lr,
                               bs = bs, nlayers = nlayers, opt = opt)
        }
          
        model <- results[1]
        loss_train_set <- as.numeric(results[2])
        loss_val_set <- as.numeric(results[3])
        loss_test_set <- as.numeric(results[4])
          
        yhat_train <<- model %>% predict(X_train)
        yhat_val <<- model %>% predict(X_val)
        yhat_test <<- model %>% predict(X_test)
        
        datasets_denormalized <- reverse_prep(prep_params, prep_option)
        yhat_train_back <<- datasets_denormalized[1]
        yhat_val_back <<- datasets_denormalized[2]
        yhat_test_back <<- datasets_denormalized[3]
        
        rmse_train = list()
        mae_train = list()
        
        rmse_val = list()
        mae_val = list()
        
        rmse_test = list()
        mae_test = list()
        
        n=1
        for (k in 1:dim(y_train)[3]) {
            cat('\n time series',n)
            for (j in 1:ntimesteps) {
                train = yhat_train_back[[1]]
                val = yhat_val_back[[1]]
                test = yhat_test_back[[1]]
                
                rmse_train[n] = rmse(as.numeric(unlist(y_train_back[,n])),unlist(as.vector(train[,n]))) 
                mae_train[n] = mae(as.numeric(unlist(y_train_back[,n])),unlist(as.vector(train[,n])))
                
                rmse_val[n] = rmse(as.numeric(unlist(y_val_back[,n])),unlist(as.vector(val[,n]))) 
                mae_val[n] = mae(as.numeric(unlist(y_val_back[,n])),unlist(as.vector(val[,n])))
                
                rmse_test[n] = rmse(as.numeric(unlist(y_test_back[,n])),unlist(as.vector(test[,n]))) 
                mae_test[n] = mae(as.numeric(unlist(y_test_back[,n])),unlist(as.vector(test[,n])))
                
                cat('\n (train) The error for timestep',j)
                cat("\nRMSE (train): ", as.numeric(rmse_train[n]))
                cat("\nMAE (train): ", as.numeric(mae_train[n]))
                
                cat('\n (val) The error for timestep',j)
                cat("\nRMSE (val): ", as.numeric(rmse_val[n]))
                cat("\nMAE (val): ", as.numeric(mae_val[n]))
                
                cat('\n (test) The error for timestep',j)
                cat("\nRMSE (test): ", as.numeric(rmse_test[n]))
                cat("\nMAE (test): ", as.numeric(mae_test[n]))
                
                n = n+1
            }
        }
          
        training_time <- proc.time() - lstm_tic # End timer
        
        cat("\nIteration", i, ":")
        cat("\nTraining took", training_time[3], "secs")
        cat("\nLoss on Train set = ", loss_train_set)
        cat("\nLoss on Test set = ", loss_test_set, "\n")
        cat("\n")
        
        if (i == 1) {
            loss_rs = mean(unlist(mae_val))
            model_rs <<- model
            iteration <- i
        } else {
              if (mean(unlist(mae_val)) < loss_rs) {
                  loss_rs = mean(unlist(mae_val))
                  model_rs <<- model
                  iteration <- i
              }
        }
        
        if (flag == 1) {
            gridtable[i, ] <<- list(units1, units2, units3, lr, nepochs, bs, nlayers, -loss_val_set)
        } else {
            gridtable[i, ] <<- list(units1, units2, units3, lr, nepochs, bs, nlayers, mae_val[j][[1]])
        }
        RS_time <<- proc.time() - RS_tic # End timer
      
    }
    
    cat("\nRandom Search finished after", RS_time[3])
    cat("\nBest loss on validation set found to be:", loss_rs, "in iteration number", iteration, "\n")
    
    # Estimate predictions
    yhat_train <<- model_rs %>% predict(X_train)
    yhat_val <<- model_rs %>% predict(X_val)
    yhat_test <<- model_rs %>% predict(X_test)
  
    if (flag == 1) {
        return(gridtable)
    } else {
        return(c(loss_rs, model_rs))
    }
}