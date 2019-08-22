################################################################
#####qqlot.Distribution
################################################################

#To do: 
#1).add arguments for graphical parameters of 6 lines eg.lty, lwd, col ...
#2).decide contents of legend
#3).coverage in plot.Distribution & coverage in pointwise()    same meaning?
#   if not, use different names 
#4).add argument 'n' (number of quantiles at which to do the comparison)?


#######################################################################
#
#   Arguments:
#
#   x             A SDistribution class object.
#   y             A SDistribution class object.
#   nPoints       Number of evaluation points to be generated for each
#                 distribution, the default is set to be 3,000.
#   coverage      A numerical parameter between 0 and 1, indicating the range 
#                 of probabilities being evaluated, centered at probability 
#                 equal 0.5. (e.g. coverage = 0.9 will plot points from 0.05
#                 quantile to 0.95 quantile)
#   plot          A logical factor indicated whether to produce the quantile
#                 plot. 
#                 If false, a list containing graphical information 
#                 (x0, y0 and other arguments) will be returned.
#                 The default is set to be TRUE.
#   withIdLine    A logical factor indicated whether to produce plot for
#                 the line y = x, only works if plot = TRUE
#                 The default is set to be TRUE.
#   withConf      A logical factor indicated whether to produce plots for
#                 confidence lines, only works if plot = TRUE.
#                 The default is set to be TRUE. 
#   withConf.pw   A logical factor indicated whether to produce plots for
#                 pointwise confidence lines, only works if withConf = TRUE.
#                 The default is set to be TRUE.
#   withConf.sim  A logical factor indicated whether to produce plots for
#                 simultaneous confidence lines, only works if withConf = TRUE.
#                 The default is set to be TRUE.
#   xlab          x-lable.
#   ylab          y-lable.
#   ...           All graphical parameters in current R plot function can 
#                 be added as additional arguments.
#
#######################################################################

library(EnvStats)

qqplot.Distribution <- function(x, y, nPoints = 3000, coverage = 0.99, 
                                withIdLine = TRUE, withConf = TRUE, 
                                withConf.pw  = withConf, withConf.sim = withConf, 
                                plot = TRUE, xlab = deparse(substitute(x)), 
                                ylab = deparse(substitute(y)), ...){
  
  args <- list(x = x, y = y, nPoints = 3000, coverage = 0.99, withIdLine = withIdLine,
                withConf = withConf, withConf.pw  = withConf.pw,
                withConf.sim = if(missing(withConf.sim)) {
                  if(missing(withConf)) NULL else withConf} else withConf.sim,
                plot = plot, xlab = xlab, ylab = ylab)    ##with reference to package 'distr'
  
  
  ##Quantile vs Quantile##
  #use method in plot.Distribution to plot quantile vs quantile
  plotStructure <- list("points" = NA, "pdf" = NA, "cdf" = NA,
                        "hazard" = NA, "cumHazard" = NA, "survival" = NA,
                        n = nPoints, plotChoice = NA)
  plotStructure$plotChoice <- c("pdf","cdf","quantile","survival","hazard","cumHazard")
  
  # random number generator: sample quantiles from 0 to 1 (cdf)
  plotStructure$cdf <- seq((0.5 - coverage/2),(0.5 + coverage/2),
                           length.out = plotStructure$n)
  # calculate x0, y0
  plotStructure$x0 <- x$quantile(plotStructure$cdf)
  plotStructure$y0 <- y$quantile(plotStructure$cdf)
  
  
  #now plot!
  if(plot == TRUE){
    plot(x = plotStructure$x0, y = plotStructure$y0, type = "p", col = "black",
         xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),...)
    
    ##Identity Line##
    if(withIdLine == TRUE){
      abline(a = 0, b = 1, col = "blue", lty = 2, lwd = 2)
    }
    
    ##Confidence Lines##
    if(withConf == TRUE){
      
      x0 <- plotStructure$x0 
      y0 <- plotStructure$y0
      
      quantile.df <- data.frame(x0, y0)
      
      quantile.fit <- lm(y0 ~ x0, data = quantile.df) 
      
      new.x0 <- seq(min(x0), max(x0), length=100)  #how to generate newdata?
      
      predict.quantile <- predict(quantile.fit, 
                                  newdata = data.frame(x0 = new.x0), se.fit = TRUE) 
      
      ##Pointwise Confidence Interval##
      if(withConf.pw == TRUE){
        ci.pw <- pointwise(predict.quantile, coverage=0.95) 
        
        lines(new.x0, ci.pw$lower, lty=3, col = "orange", lwd = 2) 
        lines(new.x0, ci.pw$upper, lty=3, col = "orange", lwd = 2) 
      }
      
      ##Simultaneous Confidence Interval##
      if(withConf.sim==TRUE){
        ci.sim <- pointwise(predict.quantile, coverage=0.95, 
                            simultaneous=TRUE) 
        
        lines(new.x0, ci.sim$lower, lty=4, col = "red", lwd = 2) 
        lines(new.x0, ci.sim$upper, lty=4, col = "red", lwd = 2)
      }
      
      ##Legend
      legend("topleft", legend = c("Pintwise CI", "Simultaneous CI"), col = c("orange", "red"),
               lty = c(3,4), cex = 0.8)
    }
    
  }else{
    returnList = list(x0 = plotStructure$x0, y0 = plotStructure$x0, args = args)
    
    return(returnList)
  }
}





##Example
qqplot.Distribution(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15))

qqplot.Distribution(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15), 
                    plot = FALSE)

qqplot.Distribution(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15), 
                    withIdLine = FALSE)

qqplot.Distribution(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15), 
                    withConf = FALSE)

qqplot.Distribution(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15), 
                    withConf.pw = FALSE)

qqplot.Distribution(Normal$new(mean = 15, sd = sqrt(30)), ChiSquared$new(df = 15), 
                    withConf.sim = FALSE)

