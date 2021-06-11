#-------------------- Vanilla (Bayesian) --------------------#

lstm_bayesian <- function(units1, units2, units3, lr, nepochs, bs, nlayers) {

    units = nlags
    model <- keras_model_sequential()
  
    if (opt==1) {
        optimizer = optimizer_sgd(lr = lr)
    } else if (opt==2) {
        optimizer = optimizer_adam(lr = lr)
    } else if (opt==3) {
        optimizer = optimizer_rmsprop(lr = lr)
    } else {
        optimizer = optimizer_adagrad(lr = lr)
    } 
    
    if (activation==1) {
        act = "relu"
    } else if (activation==2) {
        act = "sigmoid"
    } else if (activation==3) {
        act = "tanh"
    } else {
        act = NULL
    } 
    
    if (nlayers == 1) {
        model %>%
          layer_lstm(units = units, input_shape = c(nlags,ntimeseries*nfeat), return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid') %>%
          layer_repeat_vector(ntimesteps) %>%
          layer_lstm(units = units, return_sequences = TRUE, activation = act,  recurrent_activation ='sigmoid') %>%
          time_distributed(layer_dense(units = ntimeseries))
    } else if (nlayers == 2) {
          model %>%
            layer_lstm(units = units, input_shape = c(nlags,ntimeseries*nfeat),  return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>%
            layer_lstm(units = units1, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid') %>%
            layer_repeat_vector(ntimesteps) %>%
            layer_lstm(units = units1, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>% 
            time_distributed(layer_dense(units = ntimeseries))
    } else if (nlayers == 3) {
          model %>%
            layer_lstm(units = units, return_sequences = TRUE, input_shape = c(nlags, nfeat*ntimeseries), activation = act, recurrent_activation ='sigmoid') %>% 
            layer_lstm(units = units1, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>%
            layer_lstm(units = units2, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid') %>% 
            layer_repeat_vector(ntimesteps) %>%
            layer_lstm(units = units2, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>% 
            time_distributed(layer_dense(units = ntimeseries))
    } else {
          model %>%
            layer_lstm(units = units, return_sequences = TRUE, input_shape = c(nlags, nfeat*ntimeseries), activation = act, recurrent_activation ='sigmoid') %>% 
            layer_lstm(units = units1, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>%
            layer_lstm(units = units2, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>%
            layer_lstm(units = units3, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid') %>%
            layer_repeat_vector(ntimesteps)%>%
            layer_lstm(units = units3, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid') %>% 
            time_distributed(layer_dense(units = ntimeseries))
    }
    
    summary(model)
    
    model %>% compile(
        loss = "mean_absolute_error",
        optimizer = optimizer,
        metrics = c("mean_squared_error")
    )
    
    history <- model %>% fit(
        X_train, y_train, 
        epochs = nepochs, batch_size = bs,
        validation_data = list(X_val, y_val),
        verbose = 0
    )
    
    loss_train_set = model %>% evaluate(X_train, y_train, batch_size = bs, verbose = 0)
    loss_val_set = model %>% evaluate(X_val, y_val, batch_size = bs, verbose = 0) 
    loss_test_set = model %>% evaluate(X_test, y_test, batch_size = bs, verbose = 0)
    
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
            
            n = n + 1
        }
    }
    
    if (is.na(loss_lstm)) {
        model_bayesian <<- model
        save_model_hdf5(model, 'bayesian_vanilla.h5')
        loss_lstm <<- -as.numeric(mae_val[j][[1]])
    } else {
        if (-as.numeric(mae_val[j][[1]]) > loss_lstm) {
            model_bayesian <<- model
            save_model_hdf5(model, 'bayesian_vanilla.h5')
            loss_lstm <<- -as.numeric(mae_val[j][[1]])
        }
    }
    
    list(Score = -as.numeric(mae_val[j][[1]]), Pred = 0)
}