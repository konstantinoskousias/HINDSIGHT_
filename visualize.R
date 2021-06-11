#-------------------- Visualization function for HINDSIGHT++ --------------------#

visualize <- function(nlags, predicted, df_list){
      for (i in 1:length(df_list)) {
          x = as.data.frame(df_list[i])
          index = c(1:dim(as.data.frame(x))[1])
          plot(index, (x[,1]), xlab='t', ylab = '', cex.axis = 2.6, cex.lab = 2.6, cex = 2)
          par(mar=c(5,5,4,4))
          lines(index, (x[,1]), lwd = 2)
          
          index1 = c((1 + (nlags)):(length(predicted[[1]][, i]) + nlags))
          lines(index1, predicted[[1]][, i], col=4, lty = 5, lwd = 2)
          index2 = c((index1[length(index1)] + nlags + 1):(index1[length(index1)] + nlags + dim(predicted[[2]])[1]))
          lines(index2, predicted[[2]][, i], col=2, lty = 5, lwd = 2)
          index3 = c((index2[length(index2)] + nlags + 1):(dim(as.data.frame(x))[1]))
          lines(index3, as.data.frame(predicted[[3]])[,i], col=3, lty = 5, lwd = 2)
    }
}