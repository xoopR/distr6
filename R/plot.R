#######################################################################
#######################################################################
#######################################################################
######                                                           ######
######                Plot Methods for Distr6                    ######
######                                                           ######
#######################################################################
#######################################################################

plot.SDistribution <- function(x, fun, nPoints = 3000, coverage = 0.99,
                               plot = TRUE, iterative = FALSE, add = FALSE,
                               layout.row = TRUE, layout.col = FALSE, 
                               margin = TRUE,...){
  #######################################################################
  #
  #   To set up quantitis required to produce pdf/cdf/quantile/survival/
  #   hazard/cumulative hazard plots. Arguments:
  #
  #   x         A SDistribution class object
  #   fun       List of plots to be produced
  #   nPoints   Number of evaluation points to be generated for each
  #             distribution, the default is set to be 3,000.
  #   plot      A logical factor indicated whether to produce plots for 
  #             the distribution object. If false, a list containing 
  #             graphical information (evaluation points, pdf, cdf, etc.)
  #             will be returned.
  #   iterative A logical factor that allows the users to specify the
  #             way that plots are displayed. If TRUE, one plot will be
  #             produced at one time and the users can see the next plot
  #             by hitting <return>. If FALSE, all required plots will be
  #             shown on the same page.
  #   add       A logical factor indicated whether to have new plots
  #             superimposed over an existing plot.
  #   layout.row A logical factor that only works if iterative == FALSE.
  #             When iterative == TRUE, this argument will be ignored.
  #             If TRUE, the layout of figures in the plot window will be
  #             decided by default (1*3 for three plots, 2*2 for 4 plots
  #             and 2*3 for 5/6 plots). Once plots are produced, the
  #             graphical parameters prior to plotting are retrieved. 
  #             If FALSE, the users can change graphical parameters 
  #             via par() in the global environment. 
  #   layout.col Works in the same way as mfrow that changes the layout
  #             figures and only applies if iterative == FALSE. If TRUE, 
  #             figures will be displayed in a columnwise order. The 
  #             default is set to be FALSE. If both mfrow and mfcol are TRUE,
  #             mfcol will overwrite mfrow.
  #   margin    Similiar to mfrow, only works if iterative == FALSE. The
  #             margin of plots are changed when mar == TRUE.
  #   coverage  A numerical parameter between 0 and 1, indicating the range 
  #             of probabilities being evaluated, centered at probability 
  #             equal 0.5. (e.g. coverage = 0.9 will plot points from 0.05
  #             quantile to 0.95 quantile).
  #   ...       All graphical parameters in current R plot function can 
  #             be added as additional arguments.
  #
  #######################################################################
  
  
  
  #######################################################################
  #######                   plottable structure                   #######
  #######################################################################
  
  plotStructure <- list("points" = NA, "pdf" = NA, "cdf" = NA,
                        "hazard" = NA, "cumHazard" = NA, "survival" = NA,
                        n = nPoints, plotChoice = NA)
  plotStructure$plotChoice <- c("pdf","cdf","quantile","survival","hazard","cumHazard")
  
  # random number generator: sample quantiles from 0 to 1 (cdf)
  plotStructure$cdf <- seq((0.5 - coverage/2),(0.5 + coverage/2),
                           length.out = plotStructure$n)
  # calculate x
  plotStructure$points <- x$quantile(plotStructure$cdf)
  # calculate pdf
  plotStructure$pdf <- x$pdf(plotStructure$points)
  
  if("survival" %in% fun | "hazard" %in% fun | "cumHazard" %in% fun){
    plotStructure$pdf <- x$pdf(plotStructure$points)
    # calculate survival
    plotStructure$survival <- rep(1, plotStructure$n) - plotStructure$cdf
    # calculate hazard
    plotStructure$hazard <- plotStructure$pdf / plotStructure$survival
    # calculate cumulative hazard
    plotStructure$cumHazard <- -log(plotStructure$survival)}
  #######################################################################
  #######                         validations                     #######
  #######################################################################
  
  validPlots <- vector()
  # check user input plot names are correct
  for(i in fun){
    if((i %in% plotStructure$plotChoice) == FALSE){
      message(paste(i, "is not an available function.\n")) 
    } else{
      validPlots <- append(validPlots, values = i)
    }}
  
  #######################################################################
  #######                     graphical parameters                #######
  #######################################################################
  
  # save the current settings graphical parameters
  parDefault <- par(no.readonly = TRUE)
  
  # update par()
  par(ask = iterative)
  
  # set the number of plots on one page
  # check the number of plots to be produced
  if(iterative == FALSE){
    nPlots <- length(validPlots)
    mfList <- list()
    mfList[[1]] <- c(1,1)
    mfList[[2]] <- c(1,2)
    mfList[[3]] <- c(1,3)
    mfList[[4]] <- c(2,2)
    mfList[[5]] <- c(2,3)
    mfList[[6]] <- c(2,3)
    # update graphical parameters 
    if (layout.row == TRUE & layout.col == FALSE){
      par(mfrow = mfList[[nPlots]])}
    if (layout.col == TRUE){
      par(mfcol = mfList[[nPlots]])}
  }
  
  # set margins 
  if(iterative == FALSE & margin == TRUE){
    par(mar=c(4,4,2,2))
  }
  
  #######################################################################
  #######                       generate plots                    #######
  #######################################################################
  
  # Continuous distribution 
  # if plot == FALSE, return the plottable structure as a list
  if(plot == FALSE){
    returnList = list(n = plotStructure$n,
                      plotChoice = plotStructure$plotChoice,
                      points = plotStructure$points)
    
    if("pdf" %in% validPlots){returnList$pdf = plotStructure$pdf}
    if("cdf" %in% validPlots){returnList$cdf = plotStructure$cdf}
    if("quantile" %in% validPlots){returnList$quantile = plotStructure$quantile}
    if("survival" %in% validPlots){returnList$survival = plotStructure$survival}
    if("hazard" %in% validPlots){returnList$hazard = plotStructure$hazard}
    if("cumHazard" %in% validPlots){returnList$cumHazard = plotStructure$cumHazard}
    
    return(returnList)
  } 
  else{ if(add == FALSE){
    if(testContinuous(x)){
      if("pdf" %in% validPlots){
        # plot pdf
        plot(x = plotStructure$points, y = plotStructure$pdf, type = "l",
             main = "pdf", xlab = "x", ylab = "f(x)",...)}
      if("cdf" %in% validPlots){
        # plot cdf
        plot(x = plotStructure$points, y = plotStructure$cdf, type = "l",
             main = "cdf", xlab = "x", ylab = "F(x)",...)}
      if("quantile" %in% validPlots){
        # plot x against quantile
        plot(x = plotStructure$cdf, y = plotStructure$points, type = "l",
             main = "quantile", xlab = "q", ylab = parse(text = "F^(-1)(q)"),...)}
      if("hazard" %in% validPlots){
        # plot hazard function
        plot(x = plotStructure$points, y = plotStructure$hazard, type = "l",
             main = "hazard", xlab = "x", ylab = "h(x)",...)}
      if("cumHazard" %in% validPlots){
        # plot cumulative hazard function
        plot(x = plotStructure$points, y = plotStructure$cumHazard, type = "l",
             main = "cumulative hazard", xlab = "x", ylab = expression(Lambda(x)),...)}
      if("survival" %in% validPlots){
        # plot survival function
        plot(x = plotStructure$points, y = plotStructure$survival, type = "l",
             main = "survival", xlab = "x", ylab = "S(x)",...)}
      
    }}}
  
  #######################################################################
  #######                         line plots                      #######
  #######################################################################
  # if add == TRUE, add lines to an existing plot
  if(add == TRUE){
    if("pdf" %in% validPlots){
      points(cbind(plotStructure$points, y = plotStructure$pdf), type = "l",...)}
    if("cdf" %in% validPlots){
      points(cbind(plotStructure$points, y = plotStructure$cdf), type = "l",...)}
    if("quantile" %in% validPlots){
      points(cbind(plotStructure$cdf, y = plotStructure$points), type = "l",...)}
    if("hazard" %in% validPlots){
      points(cbind(plotStructure$points, y = plotStructure$hazard), type = "l",...)}
    if("cumHazard" %in% validPlots){
      points(cbind(plotStructure$points, y = plotStructure$cumHazard), 
             type = "l",...)}
    if("survival" %in% validPlots){
      points(cbind(plotStructure$points, y = plotStructure$survival), 
             type = "l",...)}
  }
  # return graphical parameters to the original settings
  par(parDefault)
}





#######################################################################
# example
#######################################################################
my.norm <- Normal$new(mean = 2, sd = 1.5)
my.norm.two <- Normal$new(mean = 2.5, sd = 1)
decorate(my.norm, ExoticStatistics)
decorate(my.norm.two, ExoticStatistics)

# draw a single plot
plot(my.norm, fun = "pdf")

# draw multiple plots
plot(my.norm, plot = TRUE, 
     fun = c("cdf", "pdf", "quantile","survival","hazard","cumHazard", "random"),
     layout.row = T, margin = T, col = "blue", add = F, iterative = F, 
     coverage = 0.99)

# add line to an existing plot
plot(my.norm, fun = "pdf")
plot(my.norm.two, fun = "pdf", add = T, col = "red")

