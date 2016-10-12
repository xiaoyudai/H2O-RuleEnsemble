source(file.path(REGO_HOME, "/src/rfTrain.R"))
source(file.path(REGO_HOME, "/src/rfPreproc.R"))

rfLoadData <- function(conf){
  if (conf$data.source.type == "rdata") {
    print("reading RData...")
    envir <- new.env()
    load(file.path(conf$rdata.path, conf$rdata.fname), envir = envir)
    if (is.null(conf$rdata.dfname)) {
      dfname <- ls(envir)
      stopifnot(length(dfname) == 1)
    } else {
      dfname <- conf$rdata.dfname
    }
    data <- get(dfname, envir, inherits = FALSE)
    stopifnot(is.data.frame(data))
    # convert to h2o data frame
    data = as.h2o(data)
    if(sum(!is.na(as.vector(h2o.unique(data[,conf$col.y])))) == 2){
      data[,conf$col.y] = as.factor(data[,conf$col.y])
    }
    rm(envir)
  } else if (conf$data.source.type == "csv") {
    col.types = scan(conf$col.types.fname, what = "character")
    data <- h2o.uploadFile(file.path(conf$csv.path, conf$csv.fname), sep = ",", col.types = col.types)
    
  } else if (conf$data.source.type == "hdfs") {
    col.types = scan(conf$col.types.fname, what = "character")
    data = h2o.importFile(path = paste("hdfs://", conf$hdfs.server, conf$hdfs.fname, sep=""), col.types = col.types)
  } else {
    error(logger, paste("rfTrain_main.R: unknown data source type ", conf$data.source.type))
  }
  
  
  # data.out <- EnforceFactors(data, col.types, conf$min.level.count)$x
  
  return(data)
}