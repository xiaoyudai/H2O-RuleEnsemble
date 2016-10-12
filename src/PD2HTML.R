source(file.path(REGO_HOME, "/src/rfExport.R"))
source(file.path(REGO_HOME, "/src/rfRulesIO.R"))
source(file.path(REGO_HOME, "/src/h2oPred.R"))
library(R2HTML)
library(ROCR, verbose = FALSE, quietly=TRUE, warn.conflicts = FALSE)

PD2HTML <- function(model, data, Rules, feature, reference, varimp, rf.ctxt, conf, model.path){
  
  out.path <- file.path(model.path, "R2HTML")
  # Create output directory (if appropriate)
  if (!file.exists(out.path)) {
    dir.create(out.path)
  } 
  # variables whose importance above the threshold get displayed
  vars = varimp[which(varimp[,1] > conf$html.min.var.imp),2]
  # rules with zero coefficients. For these, we don't need to calculate their scores on the data.
  skip_rules = which(h2o.coef(model)[2:(length(Rules)+1)] == 0)
  keep_rules = setdiff(1:length(Rules),skip_rules)
  
  # Initialize HTML report
  html.file <- HTMLInitFile(out.path, conf$html.singleplot.fname, Title = conf$html.title,
                            BackGroundColor = "#BBBBEE")
  HTML.title(conf$html.singleplot.title)
  
  for(var in vars){
    print(paste('Generating single partial dependence plot for:',var,sep = " "))
    single.PD = singlePD(var, model, data, Rules, keep_rules, feature, reference, rf.ctxt$pd.sample.size, rf.ctxt$qntl, rf.ctxt$max.var.vals)
    
    plot.fname <- paste(var, "png", sep = ".")
    plot.width <- 620
    plot.height <- 480
    png(file = file.path(out.path, plot.fname), width=plot.width, height=plot.height)
    if(is.factor(data[,var])){
      single.PD$par.dep = single.PD$par.dep - min(single.PD$par.dep)
      bar.widths <- as.vector(h2o.table(data[,var])[,2] /nrow.H2OFrame(data))
      barplot(single.PD$par.dep, names = single.PD$var.vals, width = bar.widths,
              xlab=var, ylab='Partial dependence', cex.names=0.75, las = 1, col = 637) # make sure it matches rego
    }else{
      plot(single.PD$var.vals, single.PD$par.dep, xlab=var, ylab='Partial dependence', type='l')
    }
    dev.off()
    
    plot.caption = var
    HTMLInsertGraph(plot.fname, Caption=plot.caption, WidthHTML=plot.width, HeightHTML=plot.height)
  }
  
  HTMLEndFile()

}


singlePD <- function(var, model, data, Rules, keep_rules, feature, reference, pd.sample.size, qntl, max.var.vals){
  ## Get evaluation points
  if (is.factor(data[,var])) {
    ## Use all levels for categorical variables
    is.fact <- TRUE
    var.vals <- h2o.levels(data[,var])
  } else {
    ## Use 'max.var.vals' percentiles for numeric variables
    is.fact <- FALSE
    var.vals <- quantile(data[,var], na.rm=T, probs=seq(qntl, 1-qntl, 1.0/max.var.vals))
  }
  num.var.vals <- length(var.vals)
  par.dep <- rep(0, num.var.vals)
  
  ## Get random sample of observations (to speed things up)
  data.sample <- data[row = sort(sample(1:nrow(data), size=pd.sample.size)), col = 1:ncol(data)]
  
  ## Compute partial dependence over selected random sample
  y.hat.m <- matrix(nrow=pd.sample.size, ncol=num.var.vals)
  for (i.var.val in 1:num.var.vals) {
    ## Hold x[,var] constant
    ####x.sample[,var] <- var.vals[i.var.val]
    data.var <- as.h2o(rep(var.vals[i.var.val], pd.sample.size))
    data.sample[,var] <- data.var
    ## Compute y.hat
    y.hat.m[,i.var.val] <- h2oPred(data.sample, model, Rules, keep_rules, feature, reference)
    ## Compute avg(y.hat) 
    par.dep[i.var.val] <- mean(y.hat.m[,i.var.val])
  }
  
  return(list(var.vals=var.vals, par.dep=par.dep))
}