createFunction <- function(rule, feature, reference){
  # input:   
  #   rule: a list of rules
  #   feature: a list of feature names
  #   reference: "continuousFeautre" for continuous feature
  #   enumeric level set for categorical feature
  # output:
  #   a list of binary-output functions correpsonding to rules
  
  f <- function(data){
    numMatch = 0
    conditionIndex = 1
    while(conditionIndex <= length(rule)){
      numMatch = numMatch + testCondition(data, rule[[conditionIndex]], feature, reference)
      conditionIndex = conditionIndex + 1
    }
    return(numMatch == length(rule))
  }
  return(f)
}


testCondition <- function(data, condition, feature, reference){
  featureName = feature[[condition[[2]]+1]]
  # linear split
  if(condition[[1]] == 0){
    # in H2O, NA > c (NA < c) is always 0
    return((data[,featureName] > condition[[3]])&(data[,featureName] < condition[[4]]))
  }
  
  if(condition[[1]] == -1){
    return(!(data[,featureName] > condition[[3]]))
  }
  
  if(condition[[1]] == 3){
    if(condition[[3]] == 0){
      return(1 - is.na(data[,featureName]))
    } else {
      return(is.na(data[,featureName]))
    }
  }
  
  # catigorical split
  if(condition[[1]] == 1){
    if(condition[[4]] == 0){
      if('NA' %in% reference[[condition[[2]]+1]][condition[[3]]+1]){
        setContain = reference[[condition[[2]]+1]][-(condition[[3]]+1)]
        return(data[,featureName] %in% setContain)
      }else{
        return(1-(data[,featureName] %in% reference[[condition[[2]]+1]][condition[[3]]+1]))
      }
    }else{
      if('NA' %in% reference[[condition[[2]]+1]][condition[[3]]+1]){
        setContain = reference[[condition[[2]]+1]][-(condition[[3]]+1)]
        return(1-(data[,featureName] %in% setContain))
      }else{
        return(data[,featureName] %in% reference[[condition[[2]]+1]][condition[[3]]+1])
      }
    }
  }
  
  # differ split (for continuous feature only)
  if(condition[[1]] == 2){
    if(condition[[4]] == 0){
      return(data[,featureName] != condition[[3]])
    }else{
      return(data[,featureName] == condition[[3]])
    }
  }
}


