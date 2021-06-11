#-------------------- Bayesian Search --------------------#

bayesian_search <- function(nlags, network_type, units, units1, units2, units3, lr, nepochs, bs, nlayers, niter, opt) {
  
    flag <<- 1
    return_value <<- 0
    
    # Make these two parameters global
    nlags <<- nlags
    opt <<- opt
    
    # Define lower and upper bounds
    lower_bounds <- list( units1 = 16L, units2 = 16L, units3 = 16L, lr = 0.00001, nepochs = 50L, bs = 8L, nlayers = 1L)
    upper_bounds <- list( units1 = 1024L, units2 = 1024L, units3 = 1024L, lr = 0.01, nepochs = 100L, bs = 128L, nlayers = 4L)
  
    boundst = list(#nlags = c(lower_bounds[1], upper_bounds[1]),
                   units1 = c(lower_bounds[1], upper_bounds[1]),
                   units2 = c(lower_bounds[2], upper_bounds[2]), 
                   units3 = c(lower_bounds[3], upper_bounds[3]),
                   lr = c(lower_bounds[4], upper_bounds[4]),
                   nepochs = c(lower_bounds[5], upper_bounds[5]), 
                   bs = c(lower_bounds[6], upper_bounds[6]),
                   nlayers = c(lower_bounds[7], upper_bounds[7]))#,
                   #opt = c(lower_bounds[9], upper_bounds[9]))
    
    #boundst$nlags = as.integer(boundst$nlags)
    boundst$units1 = as.integer(boundst$units1)
    boundst$units2 = as.integer(boundst$units2)
    boundst$units3 = as.integer(boundst$units3)
    boundst$lr = as.numeric(boundst$lr)
    boundst$nepochs = as.integer(boundst$nepochs)
    boundst$bs = as.integer(boundst$bs)
    boundst$nlayers = as.integer(boundst$nlayers)
    #boundst$opt = as.integer(boundst$opt)
    
    loss_lstm <<- NA
  
    ptm_lstm = proc.time() # keep track of time
    
    # Run bayesian Optimization 
    cat("Initiating Bayesian search... \n")
    flag <<- 1
    return_value <<- 1
    
    if (network_type == "Vanilla") {
        fun <- lstm_bayesian
    } else {
        fun <- lstm_bidi_bayesian
    }
    
    ba_search_lstm <<- BayesianOptimization(fun,
                                            bounds = boundst, 
                                            init_grid_dt = NULL, 
                                            init_points = 2, # Do not set init_points to 1
                                            n_iter = niter,
                                            acq = "ucb", 
                                            kappa = 2.576,
                                            verbose = TRUE)
    ptm_lstm_time = proc.time() - ptm_lstm
    cat("Bayesian Optimization finished after ", ptm_lstm_time[3])
    
    # Estimate predictions
    yhat_train <<- model_bayesian %>% predict(X_train)
    yhat_val <<- model_bayesian %>% predict(X_val)
    yhat_test <<- model_bayesian %>% predict(X_test)
    
    return (ba_search_lstm)
}



