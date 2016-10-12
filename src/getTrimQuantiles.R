# AUTHOR: Xiaoyu Dai
###############################################################################

getTrimQuantiles <- function(data, beta = 0.025, feature, reference){
  # Reads in "trim" quantiles, tuples <var-name, min, max, mean>, from the specified data. 
  
  if(beta > 0.5){
    beta = 1 - beta
  }
  
  trims.df <- data.frame(matrix(NA, nrow = length(feature), ncol = 4))
  colnames(trims.df) <- c("vname", "min2keep", "max2keep", "mean")
  trims.df[,1] = feature
  idx_linear = which(reference=="continuousFeature")
  
  print(paste("Number of Linear Terms:", length(idx_linear)))
  
  i=1
  for(idx in idx_linear){
    print(i)
    i = i+1
    
    qt = h2o.quantile(data[,feature[idx]], probs = c(beta, 1-beta))
    x.min2keep = qt[1]
    x.max2keep = qt[2]
    trims.df[idx,2] = x.min2keep
    trims.df[idx,3] = x.max2keep
    trims.df[idx,4] = h2o.mean(data[,feature[idx]])
  }
  
  return(trims.df)
}
