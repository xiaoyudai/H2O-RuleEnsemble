# AUTHOR: Xiaoyu Dai
###############################################################################

H2Oconvert2Rego <- function(Rules, feature, reference, coef, importance, std_supp, out.path="/tmp"){
  # export H2O model output into external files that could be read by REGO and RErunner for later process
  # Args:
  #       Rules: list of <'continuous', var, min, max> or <'categorical', var, set, not|in> 
  #                   or <'differ', var, point, not|equal> or <'isNAorLess', var, min, max> or <'isNA', var, isNA|notNA>
  #       feature: list of feature names
  #       reference: levels of catigorical feature, and "continuousFeature" otherwise
  #       coef: H2O glm coefficient, including intercept, Rules, and linear term
  #       importance: importance of Rules and linear terms, excluding intercept
  #
  # Returns:
  #       intercept.txt
  #       varimp.txt
  #       xtrain_levels.txt
  
  continuousFeature = feature[which(reference=="continuousFeature")]
  n_linear = length(continuousFeature)
  n_split = length(Rules)
  intercept = coef[1]
  max_Imp = sort(abs(importance),decreasing = TRUE)[1]
  rescale <- function(x){100*(x-min(x))/(max_Imp-min(x))} # rescale the importance with the largest being 100
  Imp = rescale(abs(importance))
  idx_sorted_Rules = sort.int(Imp,decreasing = TRUE, index.return = TRUE)$ix
  # export rules and varimp
  varimp = NULL
  n_positive_imp = sum(Imp>0)
  rego_Rules = vector(mode = "list")
  
  print(paste("number of active predictors:", n_positive_imp))
  
  for(i in 1:n_positive_imp){
    idx = idx_sorted_Rules[i]
    if(idx <= n_split){
      # Split rule
      rego_rule = convertSplitRule(Rules[[idx]], coef[idx+1], Imp[idx], std_supp[idx], feature)
    }
    if(idx > n_split){
      # Linear rule
      rego_rule = convertLinearRule(continuousFeature[idx-n_split], coef[idx+1], Imp[idx], std_supp[idx])
    }
    rego_Rules[[i]] = rego_rule
  }


  # reference
  output_reference = ""
  for(i in 1:length(reference)){
    levels = reference[[i]]
    if(levels[[1]] == "continuousFeature"){
      output_reference = paste(output_reference, "'",feature[i] ,"'\n",sep="")
    }else{
      output_reference = paste(output_reference,"'",feature[i],"'",sep="")
      for(level in levels){
        output_reference = paste(output_reference,",","'",level,"'",sep="")
      }
      output_reference = paste(output_reference,"\n",sep="")
    }
  }
  
  write(intercept, file = paste(out.path, "/intercept.txt", sep=""))
  write(output_reference, file = paste(out.path, "/xtrain_levels.txt", sep=""))
  
  return(rego_Rules)
}






convertSplitRule = function(rule, coef_, Imp_, std_supp_, feature){
  n_terms = length(rule)
  splits = vector(mode = "list")
  for(i_term in 1:n_terms){
    condition = rule[[i_term]]
    
    if(condition[[1]] == 0){
      splitType = "continuous"
      splitVarName = feature[condition[[2]]+1]
      splitRangeMin = condition[[3]]
      splitRangeMax = condition[[4]]
      split <- list(type=splitType, var = splitVarName, min = splitRangeMin, max = splitRangeMax)
    } else if(condition[[1]] == 1){
      splitType = "categorical"
      splitVarName = feature[condition[[2]]+1]
      if(condition[[4]] == 0){
        categ.cond = "not"
      }else{
        categ.cond = "in"
      }
      level.list = c()
      for(i in 1:length(condition[[3]])){
        level.list[i] = condition[[3]][i] + 1
      }
      split <- list(type=splitType, var = splitVarName, cond = categ.cond, levels = level.list)
    } else if(condition[[1]] == 2){
      splitType = 'differ'
      splitVarName = feature[condition[[2]]+1]
      if(condition[[4]] == 0){
        differ.cond = "unequal"
      }else{
        differ.cond = "equal"
      }
      differ.point = condition[[3]]
      split <- list(type=splitType, var = splitVarName, cond = differ.cond, point = differ.point)
    } else if (condition[[1]] == -1){
      splitType = "isNAorLess"
      splitVarName = feature[condition[[2]]+1]
      splitRangeMin = condition[[3]]
      splitRangeMax = condition[[4]]
      split <- list(type=splitType, var = splitVarName, min = splitRangeMin, max = splitRangeMax)
    } else if (condition[[1]] == 3){
      splitType = 'isNA'
      splitVarName = feature[condition[[2]]+1]
      if(condition[[4]] == 0){
        splitIs = 'notNA'
      } else {
        splitIs = 'isNA'
      }
      split <- list(type=splitType, var = splitVarName, Is = splitIs)
    }
    
    
    splits[[i_term]] = split
  }
  
  rego_rule = list(type="split", supp = std_supp_, coeff = coef_, imp = Imp_, splits = splits)
  return(rego_rule)
}




convertLinearRule <- function(name_feature, coef_, Imp_, std_supp_){
  
  return(list(type="linear", std = std_supp_, coeff = coef_, imp = Imp_, var = name_feature))
}















































