#-------------------- Main function for HINDSIGHT++ --------------------#

hindsightpp <- function(files, 
                        nfeatures = 1, nlags = 5, msteps = 1, fs_method = 'rfe',
                        network_type = 'Vanilla', act = 3, opt = 2,
                        split = 0.8, valsplit = 0.2, search_type = "Manual", niter = 50,  
                        units1 = 256, units2 = 128, units3 = 64, lr = 0.001, nepochs = 50, bs = 8, nlayers = 2) {
  
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    
    source('libraries.R')
    libraries()
  
    files = files
  
    # --Global parameters--
    nfeat <<- nfeatures
    ntimesteps <<- msteps
    ntimeseries <<- length(files)
    activation <<- act
    valsplit <<- valsplit
    flag <<- 0 # debug for RS
    
    df_list <- list()
    # feature_mat <- list()
    
    for(i in 1:length(files)){
        df_list[[i]] <- read.csv(files[i], header = TRUE, sep = ",", dec = ".")
        df_list[[i]] <- df_list[[i]][,c(1:nfeat)]
        df_list[[i]] <- as.data.frame(df_list[[i]])
    }
  
    # --One-Hot-Encoding--
    new_features = 0
    for(i in 1:length(files)) {
        for(j in 1:nfeat) {
            if (is.factor(df_list[[i]][[j]])) {
                new_features = new_features + nlevels(df_list[[i]][[j]]) - 1
                dmy <- dummyVars("~.", data = df_list[[i]])
                df_list[[i]] <- data.frame(predict(dmy, newdata = df_list[[i]]))
            }
        }
    }
    
    nfeat = nfeat + new_features # Update nfeat
   
    # Feature selection
    if (fs_method == "rfe") {
        if (nfeat>1) { # Works for nfeat>1
            feature_indexes <- rfe_feature_selection(df_list, retain = 2)
            nfeat <- length(feature_indexes) + 1
            
            for(i in 1:length(files)) {
                temp <- df_list[[i]]
                temp <- temp[,c(1, feature_indexes)]
                df_list[[i]] <- temp
            }
        }
    } else if (fs_method == "gfsm") {
          if (nfeat > 2) { # Works for nfeat>2
              feature_indexes = gfsm(df_list, targetIndex = 1, threshold = 0.0, clus = FALSE, nbre_vars = 2) # nbre_vars >=2
              nfeat <- length(feature_indexes) + 1
            
              for(i in 1:length(files)){
                  temp <- df_list[[i]]
                  temp <- temp[,c(1, feature_indexes)]
                  df_list[[i]] <- temp
              }
          }
    }
    
    nfeat <<- nfeat # Make it global
  
    prep_params <<- preprocessing(df_list, nlags = nlags, ntimesteps = ntimesteps, nfeatures = nfeat, split = split)
  
    # Hyperparameter Optimization
    if(search_type == "Manual") {
        results = manual_search(nlags = nlags, network_type = network_type, units = nlags,  units1 = units1, units2 = units2, 
                                units3 = units3,  lr = lr, nepochs = nepochs, bs = bs, nlayers = nlayers, opt = opt)
    } else if(search_type == "Random") {
          results = random_search(nlags = nlags, network_type = network_type, units = nlags,  units1 = units1, units2 = units2, 
                                units3 = units3,  lr = lr, nepochs = nepochs, bs = bs, nlayers = nlayers, niter = niter, opt = opt)
    } else if(search_type == "Bayesian") {
          results = bayesian_search(nlags = nlags, network_type = network_type, units = nlags, units1 = units1, units2 = units2, 
                                  units3 = units3, lr = lr, nepochs = nepochs, bs = bs, nlayers = nlayers, niter = niter, opt = opt)
    } else{
          warning("No valid optimization option was provided")
          return()
    }
  
    # De-normalize predicted data
    datasets_denormalized <- reverse_prep(prep_params)
    yhat_train_back <<- datasets_denormalized[1]
    yhat_val_back <<- datasets_denormalized[2]
    yhat_test_back <<- datasets_denormalized[3]
    
    df_list <<- df_list # For debugging purposes
    
    # Visualization (only for ntimesteps = 1)
    if (ntimesteps==1)
        visualize(nlags = nlags, list(as.data.frame(yhat_train_back), as.data.frame(yhat_val_back), as.data.frame(yhat_test_back)), df_list)
}


