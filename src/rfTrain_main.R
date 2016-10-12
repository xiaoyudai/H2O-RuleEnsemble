#!/usr/local/bin/Rscript

###############################################################################
# FILE: rfTrain_main.R
#
# USAGE:  rfTrain_main.R -d DATA.conf -m MODEL.conf -o H2O.conf
#
# DESCRIPTION:
#       Provides a "batch" interface to the RuleFit statistical model building
# program. RuleFit refers to Professor Jerome Friedman's implementation of Rule
# Ensembles, an interpretable type of ensemble model where the base-learners 
# consist of conjunctive rules derived from decision trees.
#
# ARGUMENTS:
#    DATA.conf: the data configuration file specifying options such as 
#               where the data is coming from, what column corresponds to 
#               the target, etc.
#   MODEL.conf: the model configuration file specifying options such as the
#               type of model being fit, the criteria being optimized, etc.
#     H2O.conf: the H2O configuartion file specifying options such as the ip, 
#               number of threads, port, etc.
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
source(file.path(REGO_HOME, "/src/rfAPI.R"))
source(file.path(REGO_HOME, "/src/rfTrain.R"))
source(file.path(REGO_HOME, "/src/rfR2HTML.R"))
source(file.path(REGO_HOME, "/src/rfGraphics.R"))
source(file.path(REGO_HOME, "/src/rfStartH2O.R"))
source(file.path(REGO_HOME, "/src/rfLoadData.R"))
source(file.path(REGO_HOME, "/src/rfTrainModel_h2o.R"))
library(getopt)
library(RODBC)


ValidateConfigArgs <- function(conf)
{
  # Validates and initializes configuration parameters.
  #
  # Args:
  #      conf: A list of <param name, param value> pairs
  # Returns:
  #   A list of <param name, param value> pairs
  
  # Must have a valid data source type
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

  # Must have column type specification, unless data source type is
  # "rdata"
  stopifnot(conf$data.source.type == "rdata" || "col.types.fname" %in% names(conf))
  
  ## Set defaults for the options that were not specified
  if (!("col.y" %in% names(conf))) {
    conf$col.y <- "y"
  }
  
  if (!("db.tbl.maxrows" %in% names(conf))) {
    conf$db.tbl.maxrows <- "ALL"
  } else {
    conf$db.tbl.maxrows <- as.numeric(conf$db.tbl.maxrows)
  }
  
  if (!("col.weights" %in% names(conf)) ||
      nchar(conf$col.weights) == 0) {
    conf$col.weights <- NULL
  }

  if (!("col.id" %in% names(conf)) ||
      nchar(conf$col.id) == 0) {
    conf$col.id <- NA
  }
  
  if (!("col.skip.fname" %in% names(conf))) {
    conf$col.skip.fname <- ""
  }
  
  if (!("col.winz.fname" %in% names(conf))) {
    conf$col.winz.fname <- ""
  }
  
  if (!("na.threshold" %in% names(conf))) {
    conf$na.threshold <- 0.95
  } else {
    conf$na.threshold <- as.numeric(conf$na.threshold)
  }
  
  if (!("min.level.count" %in% names(conf))) {
    conf$min.level.count <- 0
  } else {
    conf$min.level.count <- as.numeric(conf$min.level.count)
  }
  
  if (!("do.class.balancing" %in% names(conf))) {
    conf$do.class.balancing <- FALSE
  } else {
    conf$do.class.balancing <- (as.numeric(conf$do.class.balancing) == 1)
  }

  if (!("html.min.var.imp" %in% names(conf))) {
    conf$html.min.var.imp <- 5
  } else {
    conf$html.min.var.imp <- as.numeric(conf$html.min.var.imp)
  }

  if (!("html.min.rule.imp" %in% names(conf))) {
    conf$html.min.rule.imp <- 5
  } else {
    conf$html.min.rule.imp <- as.numeric(conf$html.min.rule.imp)
  }
  
  if ("html.singleplot.fname" %in% names(conf)) {
    if (!("html.singleplot.title" %in% names(conf))) {
      conf$html.singleplot.title <- "Dependence Plots:"
    }
    if (!("html.singleplot.nvars" %in% names(conf))) {
      conf$html.singleplot.nvars <- 10
    } else {
      conf$html.singleplot.nvars <- as.integer(conf$html.singleplot.nvars)
    }
  }

  if (!("rand.seed" %in% names(conf))) {
    conf$rand.seed <- 135711
  } else {
    conf$rand.seed <- as.numeric(conf$rand.seed)
  }
  
  if (!("log.level" %in% names(conf))) {
    conf$log.level <- kLogLevelDEBUG
  } else {
    conf$log.level <- get(conf$log.level)
  }
  
  # partial dependence plot or not 
  if (!("is.pardep" %in% names(conf))) {
    conf$is.pardep <- 'off'
  } else {
    conf$is.pardep <- conf$is.pardep
  }
  
  # Save workspace before training? (for debugging purposes)
  if (!("save.workspace" %in% names(conf))) {
    conf$save.workspace <- FALSE
  } else {
    conf$save.workspace <- as.logical(as.numeric(conf$save.workspace))
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
  kUsageString <- "/path/to/rfTrain_main.R -d <Data configuration file> -m <RF model configuration file> [-l <Log file name>]"
    
  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$data_conf) || is.null(opt$model_conf) ) {
    self <- commandArgs()[1]
    cat("Usage: ", kUsageString, "\n")
    q(status=1);
  }

  # Do we have a log file name? "" will send messages to stdout
  if (is.null(opt$log)) {
    opt$log <- ""
  }
  
  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(opt$data_conf, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param

  conf <- ValidateConfigArgs(conf)
  conf$log.fname <- opt$log
  
  return(conf)
}

GetSQLQueryTemplate <- function(conf)
{
  # Returns a SQL query template string for fetching training data
  if ("db.query.tmpl" %in% names(conf)) {
    # User-supplied template
    sql.query.tmpl <- scan(conf$db.query.tmpl, "character", quiet = T)
  } else {
    if (conf$db.type == "SQLServer") {
      sql.query.tmpl <- "
         SELECT _MAXROWS_ * 
           FROM _TBLNAME_
      "
    } else {
      stopifnot(conf$db.type == "Netezza")
      sql.query.tmpl <- "
         SELECT *
           FROM _TBLNAME_
          LIMIT _MAXROWS_
      "
    }
  }
  return(sql.query.tmpl)
}

CopyConfigFiles <- function(conf, rf.ctxt)
{
  # Save configuration files with model export directory
  ok <- 1
  if (!file.exists(file.path(rf.ctxt$working.dir,"configuration"))) {
    ok <- dir.create(file.path(rf.ctxt$working.dir,"configuration"))
  }
  if (ok) {
    ok <- file.copy(from = opt$data_conf, to = file.path(rf.ctxt$working.dir,"configuration"))
  }
  if (ok) {
    ok <- file.copy(from = opt$model_conf, to = file.path(rf.ctxt$working.dir,"configuration"))
  }
  if ("col.types.fname" %in% names(conf) && nchar(conf$col.types.fname) > 0 && ok) {
    ok <- file.copy(from = conf$col.types.fname, to = file.path(rf.ctxt$working.dir,"configuration"))
  }
  if ("col.skip.fname" %in% names(conf) && nchar(conf$col.skip.fname) > 0 && ok) {
    ok <- file.copy(from = conf$col.skip.fname, to = file.path(rf.ctxt$working.dir,"configuration"))
  }
  if (ok == 0) {
    dbg(logger, "CopyConfigFiles: couldn't copy files")
  }    
}

##############
## Main
#

# Grab command-line arguments
args.m <- matrix(c(
        'data_conf'   ,'d', 1, "character",
        'model_conf'  ,'m', 1, "character",
        'log'         ,'l', 1, "character",
        'help'        ,'h', 0, "logical"
    ), ncol=4,byrow=TRUE)
opt <- getopt(args.m)
conf <- ValidateCmdArgs(opt, args.m)

# Create logging object
logger <- new("logger", log.level = conf$log.level, file.name = conf$log.fname)

## Use own version of png() if necessary:
if (isTRUE(conf$html.graph.dev == "Bitmap")) {
  png <- png_via_bitmap
  if (!CheckWorkingPNG(png)) error(logger, "cannot generate PNG graphics")
} else {
  png <- GetWorkingPNG()
  if (is.null(png)) error(logger, "cannot generate PNG graphics")
}

# Load model specification parameters
rf.ctxt <- InitRFContext(opt$model_conf, conf)

# start H2O
rfStartH2O(conf)


# Load data
data = rfLoadData(conf)



# Set global env variables required by RuleFit
platform <- rf.ctxt$platform
RF_HOME <- Sys.getenv("RF_HOME")
RF_WORKING_DIR <- rf.ctxt$working.dir

# main function
rfTrainModel_h2o(data, conf, rf.ctxt)

# Save configuration files with model
CopyConfigFiles(conf, rf.ctxt)

# # Generate HTML report
# if ("html.fname" %in% names(conf)) {
#   rfmod.stats <- runstats(train.out$rfmod)
#   WriteHTML(conf, model.path = rf.ctxt$export.dir, rfmod.stats = rfmod.stats)
#   if ("html.singleplot.fname" %in% names(conf)) {
#     WriteHTMLSinglePlot(conf, model.path = rf.ctxt$export.dir)
#   }
# }

print("Rule Ensemble completed!")
print(paste("See model results in ", RF_WORKING_DIR, "/export/", sep=""))
q(status=0)

# close h2o cluster
h2o.shutdown()
print("H2O Cluster Closed")