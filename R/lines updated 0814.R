lines.Distribution <- function(x, fun, nPoints = 3000,...){
  
  plotFuns <- c("pdf","cdf","quantile","survival","hazard","cumHazard")
  # check user input plot names are correct
  if(!all(fun %in% plotFuns)) {
    stop("invalid plot function")}
  
  #######################################################################
  #######                   plottable structure                   #######
  #######################################################################
  
  plotStructure <- list(qty = matrix(NA, nrow = nPoints, ncol = 7),
                        n = nPoints, plotChoice = NA)
  plotStructure$plotChoice <- plotFuns
  colnames(plotStructure$qty) <- c("points","pdf","cdf","quantile","survival",
                                   "hazard","cumHazard")
  
  plotStructure$qty[,"cdf"] <- seq(0,1,length.out = plotStructure$n)
  plotStructure$qty[,"points"] <- x$quantile(plotStructure$qty[,"cdf"])
  plotStructure$qty[,"pdf"] <- x$pdf(plotStructure$qty[,"points"])
  plotStructure$qty[,"quantile"] <- plotStructure$qty[,"cdf"]
  
  if(any(fun %in% c("hazard","cumHazard","survival"))){
    # calculate survival
    plotStructure$qty[,"survival"] <- 1 - plotStructure$qty[,"cdf"]
    # calculate hazard
    plotStructure$qty[,"hazard"] <- plotStructure$qty[,"pdf"] / plotStructure$qty[,"survival"]
    # calculate cumulative hazard
    plotStructure$qty[,"cumHazard"] <- -log(plotStructure$qty[,"survival"])}
  
  if(testContinuous(x)){
   .plot_continuous_lines(fun,plotStructure,...)}
  # discrete case
  if(testDiscrete(x)){
   .plot_discrete_lines(fun,plotStructure,...)}
}



# FUN_TWO: continuous distribution: adding (a) line(s)
.plot_continuous_lines <- function(fun,plotStructure,...){
  for (i in length(fun)){
    lines(cbind(x = plotStructure$qty[,"points"], y = plotStructure$qty[,fun[i]]), ...)
  }}

# FUN_FOUR: discrete distribution: adding (a) line(s)
.plot_discrete_lines <- function(fun,plotStructure,...){
  for(i in 1:length(fun)){
   if(fun[i]=='cdf'){
      lines(stepfun(x = plotStructure$qty[,"points"], 
                    y = c(0,plotStructure$qty[,"cdf"])), verticals = F,...)
    }else if(fun[i]=='quantile'){
      lines(x = unique(plotStructure$qty[,"cdf"]), y = plotStructure$qty[,"points"], type = "s")
    }else{
        lines(cbind(x = plotStructure$qty[,"points"], y = plotStructure$qty[,fun[i]]), 
              type = "h",...)
    }
}}




