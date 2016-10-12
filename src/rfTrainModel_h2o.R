# AUTHOR: Xiaoyu Dai
###############################################################################

library(rPython)
python.load(file.path(REGO_HOME, "/src/main.py"), get.exception = TRUE )
library(R2HTML)
library(ROCR, verbose = FALSE, quietly=TRUE, warn.conflicts = FALSE)
source(file.path(REGO_HOME, "/src/createFunction.r"))
source(file.path(REGO_HOME, "/src/rfR2HTML.R"))
source(file.path(REGO_HOME, "/src/H2Oconvert2Rego.R"))
source(file.path(REGO_HOME, "/src/getTrimQuantiles.R"))
source(file.path(REGO_HOME, "/src/getTrimQuantiles.R"))
source(file.path(REGO_HOME, "/src/rfExportSQL.R"))
source(file.path(REGO_HOME, "/src/rfExport.R"))
source(file.path(REGO_HOME, "/src/varImp.R"))
source(file.path(REGO_HOME, "/src/PD2HTML.R"))

rfTrainModel_h2o <- function(data, conf, rf.ctxt){
  # model path
  model.path = paste(rf.ctxt$working.dir,"/export",sep="")
  if (!file.exists(model.path)) {
    dir.create(model.path)
  }
  # POJO path
  POJO.path = paste(rf.ctxt$working.dir,"/export/POJO",sep="")
  if (!file.exists(POJO.path)) {
    dir.create(POJO.path)
  }
  
  # gradient boosted machine
  print("Tree Ensemble...")
  
  if(rf.ctxt$rfmod == "regress"){
    gbm.distribution = "gaussian"
    regularization.family = "gaussian"
  } else if(rf.ctxt$rfmod == "class"){
    gbm.distribution = "bernoulli"
    regularization.family = "binomial"
  }
  # skip columns
  cols2skip = scan(conf$col.skip.fname, what = "character")
  allCols = colnames(data)
  # input variable and response variable
  output_name = conf$col.y
  input_names = setdiff(allCols, cols2skip)
  input_names = setdiff(input_names, output_name)
  # get number of trees and depth based the input tree.size and max.rules
  rulesPerTree = 2 * (rf.ctxt$tree.size - 1)
  ntrees = as.integer(rf.ctxt$max.rules/rulesPerTree) + 1 # plus one to make more flexible
  max_depth = as.integer(log(rf.ctxt$tree.size, base = 2)) + 1 # plus one to make more flexible
  # GBM
  gbm <- h2o.gbm(x=input_names, y=output_name, 
                 training_frame = data, model_id = "gbm", ignore_const_cols = TRUE,
                 distribution = gbm.distribution, ntrees = ntrees, max_depth = max_depth, weights_column = conf$col.weights,
                 learn_rate = rf.ctxt$memory.par, sample_rate = rf.ctxt$samp.fract)
  
  
  # export POJO
  h2o.download_pojo(gbm,path=POJO.path)
  
  print("Applying Rules on Training Set...")
  # parse POJO file
  pojo_out = python.call("pojo_extractor", path=paste(POJO.path,"/gbm.java",sep=""))
  # get Rules, feature and reference
  feature = pojo_out[[1]]
  reference = pojo_out[[2]]
  Rules = pojo_out[[3]]
  # apply rules on training set 
  print(paste("number of rules:" , length(Rules)))
  dataOnRule = list()
  for(i in 1:length(Rules)){
    f_rule = createFunction(Rules[[i]], feature, reference)
    dataOnRule[[i]] = f_rule(data)
    colnames(dataOnRule[[i]]) = paste("rule",i,sep="")
    print(i)
  }
  dataALL = h2o.cbind(dataOnRule,data)
  
  # winsorize
  print("Winsorizing...")
  # linear input variables
  linear_names = feature[which(reference=="continuousFeature")]
  
  x.trims = getTrimQuantiles(data, beta=rf.ctxt$data.trim.quantile, feature, reference)
  write.table(x.trims, file = paste(model.path, "/x.trims.txt", sep=""), row.names = FALSE, quote = FALSE) 
  
  # winsorize training dataset 
  for(name in linear_names){
    name.idx = which(x.trims[,1] == name)
    min2keep = x.trims[name.idx,2]
    max2keep = x.trims[name.idx,3]
    dataALL[,name] = ifelse(dataALL[,name]<min2keep, min2keep, dataALL[,name])
    dataALL[,name] = ifelse(dataALL[,name]>max2keep, max2keep, dataALL[,name])
    
    # also winsorize original data for partial dependence plot later
    data[,name] = ifelse(data[,name]<min2keep, min2keep, data[,name])
    data[,name] = ifelse(data[,name]>max2keep, max2keep, data[,name])
  }
  
  # regularization
  # need to manually tune lambda to control number of rules with non-zero coeff's. To be continued...
  
  print("Regularization...")
  
  if(rf.ctxt$test.reps == 0 && rf.ctxt$test.fract > 0){
    data.split = h2o.splitFrame(dataALL, rf.ctxt$test.fract)
    valid_data = data.split[[1]]
    train_data = data.split[[2]]
    rfRegularization <- h2o.glm(x=c(colnames(dataALL)[1:length(Rules)], linear_names), y=output_name, 
                                training_frame = train_data, model_id = "rfRegularization", 
                                validation_frame = valid_data, 
                                ignore_const_cols = FALSE, # for structure maintainance. doesn't matter since coef of const col will be zero
                                family = regularization.family, weights_column = conf$col.weights, 
                                alpha = rf.ctxt$regularization.alpha, lambda = rf.ctxt$regularization.lambda)
  } else if (rf.ctxt$test.reps > 0) {
    rfRegularization <- h2o.glm(x=c(colnames(dataALL)[1:length(Rules)], linear_names), y=output_name, 
                                training_frame = dataALL, model_id = "rfRegularization",
                                nfolds = rf.ctxt$test.reps,  
                                ignore_const_cols = FALSE, # for structure maintainance. doesn't matter since coef of const col will be zero
                                family = regularization.family, weights_column = conf$col.weights, 
                                alpha = rf.ctxt$regularization.alpha, lambda = rf.ctxt$regularization.lambda)
  } else {
    print('Warning: No validation data is available, so the best lambda is selected as the minimal lambda.')
    rfRegularization <- h2o.glm(x=c(colnames(dataALL)[1:length(Rules)], linear_names), y=output_name, 
                                training_frame = dataALL, model_id = "rfRegularization", 
                                ignore_const_cols = FALSE, # for structure maintainance. doesn't matter since coef of const col will be zero
                                family = regularization.family, weights_column = conf$col.weights, 
                                alpha = rf.ctxt$regularization.alpha, lambda = rf.ctxt$regularization.lambda)
  }
  
  # export h2o model
  # H2O on Hadoop has problem saving model to specific path
  # h2o.saveModel(rfRegularization, path = model.path, force = TRUE) 
  h2o.saveModel(rfRegularization, path = "/tmp", force = TRUE)
  
  # export rulefit coef
  write.csv(h2o.coef(rfRegularization), file = paste(model.path,'/coefficients.csv',sep=''), quote = F, row.names = F)
  
  if(rfRegularization@allparameters$family == "binomial"){
    print(paste("AUC on training data: ", h2o.auc(rfRegularization)))
  }
  # y and y.hat for test error and ROC curve
#   if(rf.ctxt$rfmod == "regress"){
#     y = as.vector(data[,output_name])
#     y.hat = as.vector(h2o.predict(rfRegularization, dataALL)[,1])
#   } else {
#     keys = h2o.levels(data, output_name)
#     y = as.vector(data[,output_name])
#     y = ifelse(y == keys[1], 1, -1)
#     y.hat = as.vector(h2o.predict(rfRegularization, dataALL)[,2] - h2o.predict(rfRegularization, dataALL)[,3])
#   }
  
  
  # converting to REGO
  print("Converting to rego_Rules...")
  coef = h2o.coef(rfRegularization)
  if(sum(coef!=0) == 1){
    print("Warning: no predictors selected because lambda is too big.")
  }
  
  # Rule based importance
  coef_Rules = coef[2:(length(Rules)+1)]
  coef_Linear = coef[(length(Rules)+2):length(coef)]
  
  s = mean(dataALL[,1:length(Rules)], na.rm = T)
  std_linear = sqrt(diag(as.matrix(var(data[,linear_names], na.rm = T))))
  
  importance_Rules = abs(coef_Rules)*sqrt(s*(1-s))
  importance_linear = abs(coef_Linear) * std_linear
  importance = c(importance_Rules, importance_linear)
  importance[which(is.na(importance))] = 0
  importance_Rules[which(is.na(importance_Rules))] = 0
  importance_linear[which(is.na(importance_linear))] = 0
  
  # Input variable importance
  
  varimp = varImp(Rules, feature, reference, importance_Rules, importance_linear)
  write.table(varimp, file=paste(model.path,"/varimp.txt",sep=""), col.names = FALSE, row.names = FALSE, quote = FALSE, sep="\t")
  
  # support of rules and std of liner terms
  supp = as.vector(apply(dataALL[,1:length(Rules)],2,sum)/nrow(dataALL))
  std_supp = c(supp, std_linear)
  
  # convert to rego-compatible Rules for model summary plot and SQL output
  rego_Rules = H2Oconvert2Rego(Rules, feature, reference, coef, importance, std_supp, out.path=model.path)
  
  # export SQL clause of the model, can be parsed into RErunner
  
  print("Exporting SQL... ")
  # export SQL
  ExportModel2SQL(rego_Rules, model.path, out.path = model.path, levels.fname = "xtrain_levels.txt", out.fname = "rules_forSQL.txt", 
                  merge.dups = FALSE, expand.lcl.mode = 1,
                  export.type = "score", db.type = "SQLServer", max.sql.length = 500, x.trims = x.trims) 
  
  # export HTML model summary for reading
  print("Exporting Model Summary...")
  # suppress AUC plot to speed up 
  y = 0
  y.hat = 0
  WriteHTML(rego_Rules, conf, model.path, out.path = model.path, rfmod = rfRegularization, y, y.hat)

  
  ############### Partial dependence plots #######################
  if(conf$is.pardep == 'on'){
    PD2HTML(rfRegularization, data, Rules, feature, reference, varimp, rf.ctxt, conf, model.path)  
  }
  
}

