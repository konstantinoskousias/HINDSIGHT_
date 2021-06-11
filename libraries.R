libraries <- function () {

        # Load functions
        message('Loading functions/libraries ...')
        
        source("rfe_feature_selection.R")
        source("gfsm.R")
        
        source("preprocessing.R")
        source("normalization.R")
        source("time_series.r")
        
        source("manual_search.r")
        source("random_search.r")
        source("bayesian_search.r")
        
        source("lstm.r")
        source("bidi_lstm.r")
        source("lstm_bayesian.r")
        source("lstm_bidi_bayesian.r")
        
        source("reverse_prep.r")
        source("visualize.r")
        
        # Load packages
        packages <- c("keras", "tensorflow", "caret", 
                      "devtools", "rBayesianOptimization", "stats", 
                      "cluster", "kernlab", "factoextra",
                      "lmtest", "imputeTS", "tseries", "Metrics")
        if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
                install.packages(setdiff(packages, rownames(installed.packages())))  
        }
        
        suppressPackageStartupMessages(library(keras))
        suppressPackageStartupMessages(library(tensorflow))
        suppressPackageStartupMessages(library(caret))
        suppressPackageStartupMessages(library(devtools))
        suppressPackageStartupMessages(library(rBayesianOptimization))
        suppressPackageStartupMessages(library(stats))
        suppressPackageStartupMessages(library(cluster))
        suppressPackageStartupMessages(library(kernlab))
        suppressPackageStartupMessages(library(factoextra))
        suppressPackageStartupMessages(library(lmtest))
        suppressPackageStartupMessages(library(imputeTS))
        suppressPackageStartupMessages(library(tseries))
        suppressPackageStartupMessages(library(Metrics))
}
