#-------------------- Bidirectional (Bayesian) --------------------#

lstm_bidi_bayesian <- function(units1, units2, units3, lr, nepochs, bs, nlayers) {
  
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
          bidirectional(layer_lstm(units = units, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid'), input_shape = c(nlags,ntimeseries*nfeat)) %>%
          layer_repeat_vector(ntimesteps) %>%
          bidirectional(layer_lstm(units = units, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid')) %>%
          time_distributed(layer_dense(units = ntimeseries))
    } else if (nlayers == 2) {
          model %>%
            bidirectional(layer_lstm(units = units, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid'), input_shape = c(nlags,ntimeseries*nfeat)) %>%
            bidirectional(layer_lstm(units = units1, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid')) %>%
            layer_repeat_vector(ntimesteps) %>%
            bidirectional(layer_lstm(units = units1, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid'), input_shape = c(nlags,ntimeseries*nfeat)) %>%
            time_distributed(layer_dense(units = ntimeseries))
      
    } else if (nlayers == 3) {
          model %>%
            bidirectional(layer_lstm(units = units, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid'), input_shape = c(nlags,ntimeseries*nfeat)) %>%
            bidirectional(layer_lstm(units = units1, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid')) %>%
            bidirectional(layer_lstm(units = units2, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid')) %>%
            layer_repeat_vector(ntimesteps) %>%
            bidirectional(layer_lstm(units = units2, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid')) %>%
            time_distributed(layer_dense(units = ntimeseries))
    } else {
          model %>%
            bidirectional(layer_lstm(units = units, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid'), input_shape = c(nlags,ntimeseries*nfeat)) %>%
            bidirectional(layer_lstm(units = units1, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid')) %>%
            bidirectional(layer_lstm(units = units2, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid')) %>%
            bidirectional(layer_lstm(units = units3, return_sequences = FALSE, activation = act, recurrent_activation ='sigmoid')) %>%
            layer_repeat_vector(ntimesteps) %>%
            bidirectional(layer_lstm(units = units3, return_sequences = TRUE, activation = act, recurrent_activation ='sigmoid')) %>%
            time_distributed(layer_dense(units = ntimeseries))
    }
    
    summary(model)
    
    model %>% compile(
        loss = "mean_absolute_error",
        optimizer = optimizer,
        metrics = c("mean_squared_error")
    )
    
    history <- model %>% fit(
        x = X_train, y = y_train,
        epochs = nepochs, batch_size = bs,
        validation_data = list(X_val, y_val),
        verbose=0
    )
    
    loss_train_set = model %>% evaluate(X_train, y_train, batch_size = bs, verbose = 0)
    loss_val_set = model %>% evaluate(X_val, y_val, batch_size = bs, verbose = 0) 
    loss_test_set = model %>% evaluate(X_test, y_test, batch_size = bs, verbose = 0)
    
    if (is.na(loss_lstm)) {
        save_model_hdf5(model, 'bayesian_bidi.h5')
        loss_lstm <<- -as.numeric(loss_val_set[1])
    } 
    else {
        if (-as.numeric(loss_val_set[1]) > loss_lstm) {
            save_model_hdf5(model, 'bayesian_bidi.h5')
            loss_lstm <<- -as.numeric(loss_val_set[1])
        }
    }
    
    list(Score = -as.numeric(loss_val_set[1]), Pred = 0)
}