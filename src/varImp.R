# AUTHOR: Xiaoyu Dai
###############################################################################


varImp <- function(Rules, feature, reference, importance_Rules, importance_linear){
  # calculate input variable importance
  #
  # Returns:
  #     sorted relative input variable importance
  
  # linear input variables
  linear_names = feature[which(reference=="continuousFeature")]
  
  varimp = data.frame(matrix(nrow = length(feature), ncol = 2))
  colnames(varimp) = c('scaled_importance', 'variable')
  for(i in 1:length(feature)){
    var = feature[i]
    
    # importance from linear predictor
    if(reference[i] == 'continuousFeature'){
      imp_var_linear = importance_linear[which(linear_names == var)]
    } else {
      imp_var_linear = 0
    }
    
    # importance from rules containing var
    imp_var_rule = 0
    for(j in 1:length(Rules)){
      rule = Rules[[j]]
      # total number of splits
      n_splits = length(rule)
      # number of splits on var
      n_var_split = 0
      for(split in rule){
        if(split[[2]] == i-1){
          n_var_split = n_var_split + 1
        }
      }
      # split the rule importance with each var
      imp_var_rule = imp_var_rule + importance_Rules[j]*n_var_split/n_splits
    }
    
    varimp[i,'variable'] = var
    varimp[i,'scaled_importance'] = imp_var_linear + imp_var_rule
  }
  
  varimp = varimp[order(varimp[,'scaled_importance'], decreasing = TRUE),]
  if(varimp[1,'scaled_importance'] > 0){
    varimp[,'scaled_importance'] = varimp[,'scaled_importance'] / varimp[1,'scaled_importance'] * 100 
  } else {
    varimp[,'scaled_importance'] = varimp[,'scaled_importance'] / 0.00000001 * 100
  }
  
  return(varimp)
}