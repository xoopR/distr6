.plot_continuous <- function(fun,plotStructure,name,...){
  plots = list()

  if("pdf" %in% fun){
    plots$pdf = list(x = plotStructure$points, y = plotStructure$pdf, type = "l",
                     main = paste(name,"Pdf"), xlab = "x", ylab = "f(x)",...)
  }

  if ("cdf" %in% fun){
    plots$cdf = list(x = plotStructure$points, y = plotStructure$cdf, type = "l",
                     main = paste(name,"Cdf"), xlab = "x", ylab = "F(x)",...)
  }

  if("quantile" %in% fun){
    plots$quantile = list(x = plotStructure$cdf,
                          y = plotStructure$points, type = "l",
                          main = paste(name,"Quantile"), xlab = "q", ylab = parse(text = "F^-1*(q)"),...)
  }

  if ("survival" %in% fun){
    plots$survival = list(x = plotStructure$points, y = plotStructure$survival, type = "l",
                          main = paste(name,"Survival"), xlab = "x", ylab = "S(x)",...)
  }

  if ("hazard" %in% fun){
    plots$hazard = list(x = plotStructure$points, y = plotStructure$hazard, type = "l",
                        main = paste(name,"Hazard"), xlab = "x", ylab = "h(x)",...)
  }

  if("cumhazard" %in% fun){
    plots$cumhazard = list(x = plotStructure$points, y = plotStructure$cumhazard,
                           type = "l",main=paste(name,"CumHazard"),xlab='x',ylab="H(x)",...)
  }

  if(length(fun) > 1)
    plots = plots[match(fun, names(plots))]
  lapply(plots, function(x) do.call(plot, x))
}
