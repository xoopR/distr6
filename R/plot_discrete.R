.plot_discrete <- function(fun,plotStructure,name,...){
  plots = list()

  if("pdf" %in% fun)
    plots$pdf = list(x = plotStructure$points, y = plotStructure$pdf, type = "h",
                     main = paste(name,"Pdf"), xlab = "x", ylab = "f(x)",...)

  if("cumhazard" %in% fun){
    plots$cumhazard$plot = list(x = plotStructure$points, y = plotStructure$cumhazard, type = "n",
                                main= paste(name,"cumhazard"),xlab='x',ylab=expression(Lambda(x)),...)
    plots$cumhazard$points = list(x = plotStructure$points, y = plotStructure$cumhazard, pch = 16,...)
    plots$cumhazard$segments = list(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                                    y0 = plotStructure$cumhazard,...)
  }

  if("cdf" %in% fun){
    plots$cdf$plot = list(x = plotStructure$points, y = plotStructure$cdf, type = "n",
                          main = paste(name,"cdf"), xlab = "x", ylab = parse(text = "F(x)"),...)
    plots$cdf$points = list(x = plotStructure$points, y = plotStructure$cdf, pch = 16,...)
    plots$cdf$segments = list(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                              y0 = plotStructure$cdf,...)
  }

  if('quantile' %in% fun){
    plots$quantile$plot = list(x = plotStructure$cdf, y = plotStructure$points, type = "n",
                               main = paste(name,"quantile"), xlab = "q", ylab = parse(text = "F^-1*(q)"),...)
    plots$quantile$points = list(x = plotStructure$cdf, y = plotStructure$points, pch = 16,...)
    plots$quantile$segments = list(x0 = plotStructure$cdf, y0 = plotStructure$points,
                                   y1 = plotStructure$points+1,...)
  }

  if("survival" %in% fun){
    plots$survival$plot = list(x = plotStructure$points, y = plotStructure$survival, type = "n",
                               main=paste(name,"Survival"),xlab='x',ylab="S(x)",...)
    plots$survival$points = list(x = plotStructure$points, y = plotStructure$survival, pch = 16,...)
    plots$survival$segments = list(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                                   y0 = plotStructure$survival,...)
  }

  if("hazard" %in% fun)
    plots$hazard = list(x = plotStructure[,"points"], y = plotStructure[,"hazard"], type = "h",
                        main = paste(name,"Hazard"), xlab = "x", ylab = "h(x)",...)

  if(length(fun) > 1)
    plots = plots[match(fun, names(plots))]

  lapply(plots, function(x){
    if(length(x) != 3)
      do.call(plot, x)
    else{
      do.call(plot, x$plot)
      do.call(points, x$points)
      do.call(segments, x$segments)
    }
  })
}
