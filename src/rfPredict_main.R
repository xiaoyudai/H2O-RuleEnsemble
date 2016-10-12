#!/usr/local/bin/Rscript

###############################################################################
# FILE: rfPredict_main.R
#
# USAGE:  rfPredict_main.R -m <model.dir> -d DATA.conf -o H2O.conf
#
# DESCRIPTION:
#       Computes predictions from a fitted RuleFit model.
#
# ARGUMENTS:
#       model.dir: path to RuleFit model exported files
#       DATA.conf: specifies test data location.
#        H2O.conf: the H2O configuartion file specifying options such as the ip, 
#                  number of threads, port, etc.
#
# REQUIRES:
#       REGO_HOME: environment variable pointing to the directory where you 
#                  have placed this file (and its companion ones)
#         RF_HOME: environment variable pointing to appropriate RuleFit
#                  executable -- e.g., export RF_HOME=$REGO_HOME/lib/RuleFit/mac
#           
# AUTHOR: Giovanni Seni <Giovanni_Seni@intuit.com> 
###############################################################################
REGO_HOME <- Sys.getenv("REGO_HOME")
source(file.path(REGO_HOME, "/src/logger.R"))
source(file.path(REGO_HOME, "/src/rfExport.R"))
source(file.path(REGO_HOME, "/src/rfTrain.R"))
source(file.path(REGO_HOME, "/src/rfGraphics.R"))
source(file.path(REGO_HOME, "/src/rfRulesIO.R"))
source(file.path(REGO_HOME, "/src/rfStartH2O.R"))
source(file.path(REGO_HOME, "/src/rfLoadData.R"))
source(file.path(REGO_HOME, "/src/createFunction.r"))
library(rPython)
python.load(file.path(REGO_HOME, "/src/main.py"), get.exception = TRUE )
library(ROCR, verbose = FALSE, quietly=TRUE, warn.conflicts = FALSE)
library(getopt)
library(h2o)

ValidateConfigArgs <- function(conf)
{
  # Validates and initializes configuration parameters.
  #
  # Args:
  #      conf: A list of <param name, param value> pairs
  # Returns:
  #   A list of <param name, param value> pairs
  
  ## Must have a valid data source type
  stopifnot("data.source.type" %in% names(conf))
  stopifnot(conf$data.source.type %in% c("csv", "db", "rdata", "hdfs"))
  if (conf$data.source.type == "db") {
    stopifnot("db.dsn" %in% names(conf) && "db.name" %in% names(conf) &&
                "db.type" %in% names(conf) && "db.tbl.name" %in% names(conf))
  } else if (conf$data.source.type == "csv") {
    stopifnot("csv.path" %in% names(conf) && "csv.fname" %in% names(conf))
    if ("csv.sep" %in% names(conf)) {
      conf$csv.sep <- as.character(conf$csv.sep)
    } else {
      conf$csv.sep <- ","
    }
  } else if (conf$data.source.type == "hdfs") {
    stopifnot("hdfs.server" %in% names(conf) && "hdfs.fname" %in% names(conf))
  } else {  # rdata
    stopifnot(c("rdata.path", "rdata.fname") %in% names(conf))
  }
  
  ## Auto-detect the platform:
  conf$rf.platform <- switch(.Platform$OS.type
                             , windows = "windows"
                             , unix = switch(Sys.info()["sysname"]
                                             , Linux = "linux"
                                             , Darwin = "mac"))
  
  if (is.null(conf$rf.platform)) error(logger, "Unable to detect platform")
  
  ## Did user specified a log level?
  if (is.null(conf$log.level)) {
    conf$log.level <- kLogLevelINFO
  } else {
    conf$log.level <- get(conf$log.level)
  }
  
  ## Did user specified an output file name?
  if (is.null(conf$out.fname)) {
    conf$out.fname <- "rfPredict_out.csv"
  }
  ## ... field separator?
  if (is.null(conf$out.sep)) {
    conf$out.sep <- ","
  } else if (nchar(conf$out.sep) == 2 && (conf$out.sep == paste("\\", "t", sep=""))) {
    conf$out.sep <- "\t"
  }
  
  ## Generate plots? 
  if (is.null(conf$graph.plots.ROC)) {
    conf$graph.plots.ROC <- TRUE
  } else {
    conf$graph.plots.ROC <- (as.numeric(conf$graph.plots.ROC) == 1)
  }
  ## ... LIFT, Gain, etc. plots?
  if (is.null(conf$graph.plots.extra)) {
    conf$graph.plots.extra <- FALSE
  } else {
    conf$graph.plots.extra <- (as.numeric(conf$graph.plots.extra) == 1)
  }
  
  ## task
  if (!("task" %in% names(conf))) {
    conf$task <- "regression"
  } else {
    conf$task <- "classification"
  }
  
  # H2O cluster init
  if (!("ip" %in% names(conf))) {
    conf$ip <- "localhost"
  } else {
    conf$ip <- conf$ip
  }
  
  if (!("port" %in% names(conf))) {
    conf$port <- 54321
  } else {
    conf$port <- as.numeric(conf$port)
  }
  
  if (!("nthreads" %in% names(conf))) {
    conf$nthreads <- -2
  } else {
    conf$nthreads <- as.numeric(conf$nthreads)
  }
  
  return(conf)
}

ValidateCmdArgs <- function(opt, args.m)
{
  # Parses and validates command line arguments.
  #
  # Args:
  #      opt: getopt() object
  #   args.m: valid arguments spec passed to getopt().
  #
  # Returns:
  #   A list of <param name, param value> pairs
  kUsageString <- "/path/to/rfPredict_main.R -m <model dir> -d <Data configuration file>"
  
  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$model_path) || is.null(opt$data_conf) ) {
    self <- commandArgs()[1]
    cat("Usage: ", kUsageString, "\n")
    q(status=1);
  }
  
  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(opt$data_conf, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param
  conf <- ValidateConfigArgs(conf)
  
  # Check Model path
  if (!(file.exists(opt$model_path))) {
    stop("Didn't find model directory:", opt$model_path, "\n")
  } else {
    conf$model.path <- opt$model_path
  }
  
  # Do we have a log file name? "" will send messages to stdout
  if (is.null(opt$log)) {
    opt$log <- ""
  }
  conf$log.fname <- opt$log
  
  return(conf)
}

CheckFactorsEncoding  <- function(x.test, x.train.levels, x.train.levels.lowcount=NULL)
{
  # Check that the integer codes given to factors in x.test are the same as the ordering
  # used when the model was built. Otherwise, when the RuleFit::rfpred() function casts  
  # the data frame to matrix, a different ordering would lead to incorrect predictions.
  #
  # Args:
  #                  x.test : data frame
  #          x.train.levels : {<var.name, low count levels>} list
  # x.train.levels.lowcount : {<var.name, low count levels>} df (if exists)
  # Returns:
  #      A copy of the given test data frame with transformed columns
  for (iVar in 1:length(x.train.levels)) {
    var.levels.train <- x.train.levels[[iVar]]$levels
    # Was iVar a factor at train time? 
    if (!(is.null(var.levels.train))) {
      factor.name <- x.train.levels[[iVar]]$var
      factor.vals <- as.character(x.test[, factor.name])
      
      # Were there low-count levels at train time? If so, replace them in x.test too
      if (!is.null(x.train.levels.lowcount)) {
        i.recoded.var <- grep(paste("^", factor.name, "$", sep=""), x.train.levels.lowcount$var, perl=T)
        if (length(i.recoded.var) > 0) {
          warn(logger, paste("CheckFactorsEncoding: replacing low-count levels for '", factor.name))
          low.count.levels <- unlist(x.train.levels.lowcount$levels[i.recoded.var])
          factor.vals <- ifelse(factor.vals %in% low.count.levels, kLowCountLevelsName, factor.vals)
        }
      }
      # Check for presence of new levels and replace them with NA (if any)
      levels.diff <- setdiff(unique(factor.vals), var.levels.train)
      if (length(levels.diff) > 0) {
        warn(logger, paste("CheckFactorsEncoding: new levels found for '", factor.name, "' : ", 
                           paste(lapply(levels.diff, sQuote), collapse=","),
                           "; replacing with NA"))
        factor.vals <- ifelse(factor.vals %in% levels.diff, NA, factor.vals)
      }
      
      # Lastly, make sure we have the same level ordering
      x.test[, factor.name] <- factor(factor.vals, levels = var.levels.train)
    }
  }
  return(x.test)
}

##############
## Main
#

# Grab command-line arguments
args.m <- matrix(c(
  'model_path'  ,'m', 1, "character",
  'data_conf'   ,'d', 1, "character",
  'log'         ,'l', 1, "character",
  'help'        ,'h', 0, "logical"
), ncol=4,byrow=TRUE)
opt <- getopt(args.m)
conf <- ValidateCmdArgs(opt, args.m)

# Set global env variables required by RuleFit
platform <- conf$rf.platform
RF_HOME <- Sys.getenv("RF_HOME")
RF_WORKING_DIR <- conf$rf.working.dir

# Create logging object
logger <- new("logger", log.level = conf$log.level, file.name = conf$log.fname)
info(logger, paste("rfPredict_main args:", 'model.path =', conf$model.path,
                   ', log.level =', conf$log.level, ', out.fname =', conf$out.fname))

## Use own version of png() if necessary:
if (isTRUE(conf$graph.dev == "Bitmap")) {
  png <- png_via_bitmap
  if (!CheckWorkingPNG(png)) error(logger, "cannot generate PNG graphics")
} else {
  png <- GetWorkingPNG()
  if (is.null(png)) error(logger, "cannot generate PNG graphics")
}

# model export path
model.path = conf$model.path

# POJO export path
POJO.path = paste(conf$model.path,"/POJO",sep="")

# out path
if (!file.exists(conf$out.path)) {
  dir.create(conf$out.path)
}

# start H2O
rfStartH2O(conf)

# Load data
data = rfLoadData(conf)


# Load rules, apply on data and winsorize
print("Applying Rules on data set...")

pojo_out = python.call("pojo_extractor", path=paste(POJO.path,"/gbm.java",sep=""))

feature = pojo_out[[1]]
reference = pojo_out[[2]]
Rules = pojo_out[[3]]

print("Winsorizing...")

linear_names = feature[which(reference=="continuousFeature")]

x.trims = read.table(file = paste(model.path, "/x.trims.txt", sep=""), header = TRUE)

# winsorize 
print(paste("Number of Linear Terms:", length(linear_names)))
i = 1
for(name in linear_names){
  print(i)
  i = i+1
  name.idx = which(x.trims[,1] == name)
  min2keep = x.trims[name.idx,2]
  max2keep = x.trims[name.idx,3]
  data[,name] = ifelse(data[,name]<min2keep, min2keep, data[,name])
  data[,name] = ifelse(data[,name]>max2keep, max2keep, data[,name])
}




# ####################### option 1 ###########################

# load model
# rfRegularization = h2o.loadModel(path = paste(model.path, "/rfRegularization", sep=""))
rfRegularization = h2o.loadModel(path = "/tmp/rfRegularization")

print(paste("number of rules:" , length(Rules)))
dataOnRule = list()
for(i in 1:length(Rules)){
  f_rule = createFunction(Rules[[i]], feature, reference)
  dataOnRule[[i]] = f_rule(data)
  colnames(dataOnRule[[i]]) = paste("rule",i,sep="")
  print(i)
}
dataALL = h2o.cbind(dataOnRule,data)

# pred = h2o.predict(rfRegularization, dataALL)
# if(rfRegularization@model$model_summary$family == "gaussian"){
#   y.hat = as.vector(pred[,1])  
# } else if(rfRegularization@model$model_summary$family == "binomial"){
#   y.hat = as.vector(pred[,2] - pred[,3])
# }

print("Training Data: ")
if(conf$task == 'classification'){
  # classification
  mse = h2o.mse(rfRegularization)
  r2 = h2o.r2(rfRegularization)
  logloss = h2o.logloss (rfRegularization)
  mpce = h2o.mean_per_class_error(rfRegularization)
  auc = h2o.auc(rfRegularization)
  nd = h2o.null_deviance(rfRegularization)
  rd = h2o.residual_deviance(rfRegularization)
  aic = h2o.aic(rfRegularization)
  metric.df = data.frame(cbind(c("MSE", "R2", "LogLoss", "Mean Per-Class Error", "AUC", "Null Deviance", "Residual Deviance", "AIC"),
                               c(mse, r2, logloss, mpce, auc, nd, rd, aic)))
  colnames(metric.df) = c("metric", "value")
} else {
  # regression
  mse = h2o.mse(rfRegularization)
  r2 = h2o.r2(rfRegularization)
  mrd = h2o.mean_residual_deviance(rfRegularization)
  nd = h2o.null_deviance(rfRegularization)
  ndf = h2o.null_dof(rfRegularization)
  rd = h2o.residual_deviance(rfRegularization)
  rdf = h2o.residual_dof(rfRegularization)
  aic = h2o.aic(rfRegularization)
  
  metric.df = data.frame(cbind(c("MSE", "R2", "Mean Residual Deviance", "Null Deviance", "Null D.o.F", "Residual Deviance", "Residual D.o.F", "AIC"),
                               c(mse, r2, mrd, nd, ndf, rd, rdf, aic)))
  colnames(metric.df) = c("metric", "value")
}
print(metric.df)


print("Testing Data: ")
testModel = h2o.performance(rfRegularization, dataALL)
print(h2o.auc(testModel))
if(conf$task == 'classification'){
  # classification
  mse = h2o.mse(testModel)
  r2 = h2o.r2(testModel)
  logloss = h2o.logloss (testModel)
  mpce = h2o.mean_per_class_error(testModel)
  auc = h2o.auc(testModel)
  nd = h2o.null_deviance(testModel)
  rd = h2o.residual_deviance(testModel)
  aic = h2o.aic(testModel)
  metric.df = data.frame(cbind(c("MSE", "R2", "LogLoss", "Mean Per-Class Error", "AUC", "Null Deviance", "Residual Deviance", "AIC"),
                               c(mse, r2, logloss, mpce, auc, nd, rd, aic)))
  colnames(metric.df) = c("metric", "value")
} else {
  # regression
  mse = h2o.mse(testModel)
  r2 = h2o.r2(testModel)
  mrd = h2o.mean_residual_deviance(testModel)
  nd = h2o.null_deviance(testModel)
  ndf = h2o.null_dof(testModel)
  rd = h2o.residual_deviance(testModel)
  rdf = h2o.residual_dof(testModel)
  aic = h2o.aic(testModel)
  
  metric.df = data.frame(cbind(c("MSE", "R2", "Mean Residual Deviance", "Null Deviance", "Null D.o.F", "Residual Deviance", "Residual D.o.F", "AIC"),
                               c(mse, r2, mrd, nd, ndf, rd, rdf, aic)))
  colnames(metric.df) = c("metric", "value")
}
print(metric.df)


# #################### option 2 ###########################

# all.coef = read.csv(file = paste(model.path,'/coefficients.csv',sep=''), header = T)$x
# 
# print(all.coef)
# print(length(Rules))
# 
# skip_rules = which(all.coef[2:(length(Rules)+1)] == 0)
# keep_rules = setdiff(1:length(Rules),skip_rules)
# intercept = all.coef[1]
# rules.coef = all.coef[keep_rules+1]
# linear.coef = all.coef[(2+length(Rules)):length(all.coef)]
# coef = c(intercept, rules.coef, linear.coef)
# linearFeature = feature[which(reference == 'continuousFeature')]
# 
# print(paste('number of active rules: ', length(keep_rules)))
# 
# #dataALL = as.h2o(matrix(intercept, nrow = nrow(data), ncol = (1+length(keep_rules)+length(linear.coef))))
# dataALL = h2o.createFrame(rows = nrow(data), cols = (1+length(keep_rules)+length(linear.coef)), value = 0)
# for(i in 2:ncol(dataALL)){
#   if(i <= (1+length(keep_rules))){
#     f_rule = createFunction(Rules[[keep_rules[i-1]]], feature, reference)
#     dataALL[,i] = f_rule(data) * coef[i]
#   } else {
#     dataALL[,i] = data[,linearFeature[i-(1+length(keep_rules))]] * coef[i]
#   }
#   print(i)
# }
# y.hat = as.vector(apply(dataALL,1,sum))
# if(conf$task == 'classification'){
#   y.hat = 1 - exp(y.hat)/(1+exp(y.hat))
#   y.hat = 2*y.hat - 1
# }
# 
# 
# # Compute test error (if y is known)
# if ("col.y" %in% names(conf)) {
#   # print("\n")
#   # print("Model Summary on New Data:")
#   # h2o.performance(regularization, dataALL)
#   if(conf$task == 'classification'){
#     y = as.vector(data[,conf$col.y])
#     re.test.error <- sum(abs(y.hat - y))/length(y)
#     med.test.error <- sum(abs(y - median(y)))/length(y)
#     aae.test <- re.test.error / med.test.error
#     info(logger, sprintf("Test AAE: %f (RE:%f, Med:%f)", aae.test, re.test.error, med.test.error))
#   } else if(rfRegularization@model$model_summary$family == "binomial"){
#     keys = keys = h2o.levels(data, conf$col.y)
#     y = as.vector(data[,conf$col.y])
#     y = ifelse(y == keys[1], 1, 0) # instead of (1, -1) as in rfTrainModel_h2o.R
#     conf.m <- table(y, sign(y.hat - 0.5))
#     stopifnot("0" %in% rownames(conf.m))
#     stopifnot("1" %in% rownames(conf.m))
#     TN <- ifelse("-1" %in% colnames(conf.m), conf.m["0", "-1"], 0)
#     FP <- ifelse("1" %in% colnames(conf.m), conf.m["0","1"], 0)
#     FN <- ifelse("-1" %in% colnames(conf.m), conf.m["1", "-1"], 0)
#     TP <- ifelse("1" %in% colnames(conf.m), conf.m["1","1"], 0)    
#     test.acc <- 100*(TN+TP)/length(y.hat)
#     info(logger, paste("Test acc:", round(test.acc, 2)))
#     info(logger, sprintf("Test confusion matrix - 0/0: %d, 0/1: %d, 1/0: %d, 1/1: %d",
#                          TN, FP, FN, TP))
#     # AUC
#     pred <- prediction(y.hat, y)
#     perf <- performance(pred, "auc")
#     info(logger, paste("Area under the ROC curve:", as.numeric(perf@y.values)))
#     
#     # Generate ROC plot
# #     if (conf$graph.plots.ROC) {
# #       kPlotWidth <- 620
# #       kPlotHeight <- 480
# #       plot.fname <- "ROC.png"
# #       pred <- prediction(y.hat, y)
# #       perf <- performance(pred, "tpr", "fpr")
# #       png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
# #       plot(perf, colorize=T, main="")
# #       lines(x=c(0, 1), y=c(0,1))
# #       dev.off()
# #    }
#   } else {
#     error(logger, "unrecognized model family")
#   }
# }


# Plot histogram of yHat
# kPlotWidth <- 620
# kPlotHeight <- 480
# plot.fname <- "yHat_hist.png"
# 
# png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
# hist(y.hat, main="")
# dev.off()

# export prediction

# pred = h2o.predict(rfRegularization, dataALL)

# write.csv(y.hat, path = file.path(conf$out.path, conf$out.fname))

h2o.removeAll()

q(status=0)









# # Extract columns used to build model
# ok <- 1
# tryCatch(x.test <- data[,colnames(mod$x)], error = function(err){ok <<- 0})
# if (ok == 0) {
#   error(logger, "rfPredict_main.R: train/test column mismatch")
# } 
# 
# # Any preprocessing needed?
# # ... Ensure factor levels are encoded in the same order used at model building time
# # ... and substitute low-count levels (if appropriate)
# x.levels.fname <- file.path(conf$model.path, kMod.x.levels.fname)
# x.levels <- ReadLevels(x.levels.fname)
# x.levels.lowcount.fname <- file.path(conf$model.path, kMod.x.levels.lowcount.fname)
# if (file.exists(x.levels.lowcount.fname)) {
#   x.levels.lowcount <- as.data.frame(do.call("rbind", ReadLevels(x.levels.lowcount.fname)))
#   x.test <- CheckFactorsEncoding(x.test, x.levels, x.levels.lowcount)
# } else {
#   x.test <- CheckFactorsEncoding(x.test, x.levels)
# }

