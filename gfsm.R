#-------------------- GFSM --------------------#

# Implementation of the feature selection algorithm proposed in the paper "A Causality Based Feature Selection
# Approach for Multivariate Time Series Forecasting" by Youssef Hmamouche et al. 
# Link to Github repo: https://github.com/Hmamouche/GFSM

# We modify the function so it returns the feature indices in the same manner as the rfe_feature_selection.R function.

gfsm <- function (input_df_list, targetIndex, threshold = 0.0, clus = FALSE, nbre_vars) {
  
    message ('GFSM ...')
    feature_mat <- list()
  
    for (list_ind in 1:length(input_df_list)) {
        F = input_df_list[[list_ind]]
        temp = F # keep initial df for indexing purposes
        gmat = matrix(nrow = dim(temp)[2], ncol = dim(temp)[2])
        
        for(i in 1:dim(temp)[2]) {
            for(j in 1:dim(temp)[2]) {
                if(i != j){
                    gmat[i,j] = 1 - grangertest(temp[,i],temp[,j])$`Pr(>F)`[2]
                } else {
                    gmat[i,j] = 1
                }
            } 
        }
        
        # in case of nbre_vars = 1, we return just one variable based on causality to the target
        if (nbre_vars == 1) {
            max_caus = 1
            for (i in 2:nrow (gmat))
                if (gmat[i, targetIndex] > gmat[max_caus, targetIndex])
                    max_caus = i 
            GSM = data.frame (F[,max_caus])
            colnames (GSM) = colnames (F)[max_caus]
            return (GSM)
        }
        
        # First, we eliminate variables that do not cause the target according to the threshold
        gsmCluster = c()
        delet = c()
        n = 1
        m = 1
        for (i in 1:ncol(gmat)) {
            if (i == targetIndex)
                target = m
            if (i != targetIndex) {
                if (gmat[i,targetIndex] < threshold) {
                    delet[n] = i
                    n = n + 1
                }
            }
            else {
                m = m + 1
            }
        }
        
        # Applying the PAM methode for the clustering task
        if ((length(delet) + 1) == ncol(F)) {
            return (data.frame())
        }
        
        if (length(delet) > 0) {
            gmat = gmat[-delet, -delet]
            F = F[,-delet]
        } 
        
        if (ncol (gmat)  == 1) {
            return (0)
        }
        
        if (ncol (gmat)  == 2) {
            #  one variable that remains
            return (F[, -target])
        }
        
        x = gmat[-target, -target]
        
        ## determine the optimal number of cluster in case of clus = TRUE
        if(clus == TRUE) {
            kmax = nrow(x)-1
            if (kmax < 2){
                return (F[,-target])
            }
            a=fviz_nbclust(x, pam, method = "silhouette",k.max = kmax)
            k = which.max(a$data[,2])
        } else {
              k = nbre_vars
        }
          
        
        if (k < ncol(x)) {
            for (l in 2:ncol(x))
                for(m in 1: (l-1))
                    x[l,m] = x[m,l] = 1 - max(x[l,m], x[m,l])
            
                clusters = pam (as.dist (x), diss = TRUE, k)
                clusteringVector = clusters$cluster
            
                classes = data.frame()
                for (j in 1:k) {
                    l = 1
                    for (i in 1:ncol(x))
                        if (clusteringVector[i] == j) {
                            classes[l,j] = i
                            if (classes[l,j] >= target) {
                                classes[l,j] = classes[l,j] + 1
                            }
                            l = l + 1
                        }
                }
            
                ### choose the best variable from each cluster
                for (j in 1:k) {
                    bestInGroup = 1;
                    if (ncol(classes) >= j) {
                        caus = gmat[classes[1,j],target];
                    
                    for (l in 2:length(classes[,j])) {
                        if(is.na(classes[l,j]) == FALSE) {
                            if (gmat[classes[l,j],target] > caus) {
                                caus = gmat[classes[l,j],target];
                                bestInGroup = l;
                            }
                        }
                    }
                        gsmCluster[j] = classes[bestInGroup,j];
                    }
                }
        
                GSM = F [,gsmCluster]
        } else {
              GSM =  F[,-target]
        }
        
        # Get the index of each feature in the initial data frame and return a vector that contains them
        feature_names <- names(GSM)
        
        feature_index <- c()
        for(i in 1:length(feature_names)) {
            feature_index <- c(feature_index, which(colnames(temp) == feature_names[i]))
        }
        feature_mat <- cbind(feature_mat, feature_index)
    }
    
    feature_indexes <- as.integer(names(table(as.integer(feature_mat))))[1:nbre_vars]
    
    return(sort(feature_indexes, decreasing = FALSE))
}
