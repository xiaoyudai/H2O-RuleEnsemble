
LogOdds <- function(p) {
  # Computes the logarithm of the odds p/(1 − p).'
  if (p < 0.0000001) {
    p <- 0.0000001
  } else if (p > 0.9999999) {
    p <- 0.9999999
  }
  return(log(p/(1-p)))
}

h2oPred <- function(data, model, Rules, keep_rules, feature, reference){
  # Rule Ensemble model prediction
  #
  # Args:
  #     data
  #     model: final rulefit regularization model
  #     Rules
  #     feature: input variables
  #     reference: 'continuous' for linear input variables
  #                 level set for categoriacal variables
  #
  # Returns:
  #     a vector of predictions (LogOdds if classification)
  
  
  # # Option 1 (fast): data are stored in memory instead of h2o dataframe
  # skip_rules = which(h2o.coef(model)[2:(length(Rules)+1)] == 0)
  # keep_rules = setdiff(1:length(Rules),skip_rules)
  # all.coef = h2o.coef(model)
  # intercept = all.coef[1]
  # rules.coef = all.coef[keep_rules+1]
  # linear.coef = all.coef[(2+length(Rules)):length(all.coef)]
  # coef = c(intercept, rules.coef, linear.coef)
  # 
  # dataALL = matrix(1, nrow = nrow(data), ncol = (1+length(keep_rules)+length(linear.coef)))
  # for(i in 2:ncol(dataALL)){
  #   if(i <= (1+length(keep_rules))){
  #     f_rule = createFunction(Rules[[keep_rules[i-1]]], feature, reference)
  #     dataALL[,i] = as.matrix(f_rule(data))
  #   } else {
  #     linearFeature = which(reference == 'continuousFeature')
  #     dataALL[,i] = as.matrix(data[,linearFeature[i-(1+length(keep_rules))]])
  #   }
  #   print(i)
  # }
  # pred = dataALL%*%coef
  # if(model@model$model_summary$family == "binomial"){
  #   pred = 1 - exp(pred)/(1+exp(pred))
  # }
  # 
  # 
  # option 2: data are stored as h2o dataframe
#   skip_rules = which(h2o.coef(model)[2:(length(Rules)+1)] == 0)
#   keep_rules = setdiff(1:length(Rules),skip_rules)
#   all.coef = h2o.coef(model)
#   intercept = all.coef[1]
#   rules.coef = all.coef[keep_rules+1]
#   linear.coef = all.coef[(2+length(Rules)):length(all.coef)]
#   coef = c(intercept, rules.coef, linear.coef)
# 
#   dataALL = as.h2o(matrix(intercept, nrow = nrow(data), ncol = (1+length(keep_rules)+length(linear.coef))))
#   for(i in 2:ncol(dataALL)){
#     if(i <= (1+length(keep_rules))){
#       f_rule = createFunction(Rules[[keep_rules[i-1]]], feature, reference)
#       dataALL[,i] = f_rule(data) * coef[i]
#     } else {
#       linearFeature = which(reference == 'continuousFeature')
#       dataALL[,i] = data[,linearFeature[i-(1+length(keep_rules))]] * coef[i]
#     }
#     print(i)
#   }
#   pred = as.vector(apply(dataALL,1,sum))
#   
#   
#   if(model@model$model_summary$family == "gaussian"){
#     pred = pred
#   } else if(model@model$model_summary$family == "binomial"){
#     pred = 1 - exp(pred)/(1+exp(pred))
#   } else {
#     error(logger, "unrecognized model family")
#   }
  
  # option 3: using h2o.predict (the only choice with NA values)
  
  dataOnRule = list()
  for(i in 1:length(Rules)){
    f_rule = createFunction(Rules[[i]], feature, reference)
    dataOnRule[[i]] = f_rule(data)
    colnames(dataOnRule[[i]]) = paste("rule",i,sep="")
    print(i)
  }
  dataALL = h2o.cbind(dataOnRule,data)
  
  pred = h2o.predict(model, dataALL)
  if(model@model$model_summary$family == "gaussian"){
    pred = pred
  } else if(model@model$model_summary$family == "binomial"){
    pred = pred[,2]
  } else {
    error(logger, "unrecognized model family")
  }
  

  return(pred)
}




