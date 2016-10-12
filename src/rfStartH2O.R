library(h2o)

rfStartH2O <- function(conf){
  h2o.init(ip = conf$ip, port = conf$port, nthreads = conf$nthreads)

}