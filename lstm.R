#-------------------- Vanilla --------------------#

lstm <- function(nlags, lr, units, units1, units2, units3, nepochs, bs, nlayers, opt, verbose = 1) {
  
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
    
    save_model_hdf5(model, 'vanilla.h5')
    
    return(c(model, loss_train_set[1], loss_val_set[1], loss_test_set[1]))
}