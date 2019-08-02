#' @export
plot.Distribution <- function(x, fun=c('pdf','cdf'), nPoints = 3000, coverage = 0.99,
                               plot = TRUE, iterative = FALSE, add = FALSE,
                               layout.row = TRUE, layout.col = FALSE, decompose = FALSE,
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
  #             default is set to be FALSE. If both layout.row and
  #             layout.col are TRUE, layout.col prevails.
  #   margin    Similiar to mfrow, only works if iterative == FALSE. The
  #             margin of plots are changed when mar == TRUE.
  #   coverage  A numerical parameter between 0 and 1, indicating the range
  #             of probabilities being evaluated, centered at probability
  #             equal 0.5. (e.g. coverage = 0.9 will plot points from 0.05
  #             quantile to 0.95 quantile).
  #   decompose A logical factor that works only for mixture distributions,
  #             if TRUE, the weighted quantities for each distribution will
  #             be displayed.
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


  names=c("pdf","cdf","hazard","survival")
  notations=c("f(x)","F(x)","h(x)","S(x)")
  func_notation=validPlots

  for(i in 1:length(names)){
    func_notation=replace(func_notation,func_notation==names[i],notations[i])
  }


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

  # define functions

  # FUN_ONE: continuous distribution
  plot_continuous <- function(validPlots,plotStructure,func_notation){
    for(i in 1:length(validPlots)){
      if(validPlots[i]=='cumHazard'){
        plot(x = plotStructure$points, y = plotStructure[[validPlots[i]]],
             type = "l",main=validPlots[i],xlab='x',ylab=expression(Lambda(x)),...)
      }else if(validPlots[i]=='quantile'){
        plot(x = plotStructure$cdf, y = plotStructure$points, type = "l",
             main = "quantile", xlab = "q", ylab = parse(text = "F^-1*(q)"),...)
      }else{
        plot(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "l",main=validPlots[i],xlab='x',ylab=func_notation[i],...)
      }}}

  # FUN_TWO: continuous distribution: adding (a) line(s)
  plot_continuous_lines <- function(validPlots,plotStructure){
    for (i in length(validPlots)){
      points(cbind(x = plotStructure$points, y = plotStructure[[validPlots[i]]]),
             type = "l", ...)
    }}

  # FUN_THREE: discrete distribution
  plot_discrete <- function(validPlots,plotStructure,func_notation){
    for(i in 1:length(validPlots)){
      if(validPlots[i]=='cumHazard'){
        plot(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "l",
             main=validPlots[i],xlab='x',ylab=expression(Lambda(x)),...)
      }else if(validPlots[i]=='cdf'){
        plot(ecdf(plotStructure$points), main='cdf',xlab='x',ylab='F(x)',...)
      }else if(validPlots[i]=='survival'){
        plot(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "l",
             main=validPlots[i],xlab='x',ylab=func_notation[i],...)
      }else if(validPlots[i]=='quantile'){
        plot(x = plotStructure$cdf, y = plotStructure$points, type = "l",
             main = "quantile", xlab = "q", ylab = parse(text = "F^-1*(q)"),...)
      }else{
        plot(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "h",
             main=validPlots[i],xlab='x',ylab=func_notation[i],...)
      }}}

  # FUN_FOUR: discrete distribution: adding (a) line(s)
  plot_discrete_lines <- function(validPlots,plotStructure){
    for(i in 1:length(validPlots)){
      if(validPlots[i]=='cumHazard'){
        points(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "l",
               ...)
      }else if(validPlots[i]=='cdf'){
        empcdf <- ecdf(plotStructure$points)
        kts <- knots(empcdf)
        points(x = kts, y = empcdf(kts), pch = 16, ...)
        for(i in 1:length(kts)){
          segments(x0 = kts[i], x1 = kts[i+1], y0 = empcdf(kts[i]), ...)}
        segments(x0 = kts[length(kts)], x1 = kts[length(kts)] + 10,
                 y0 = 1, ...)
        segments(x0 = kts[1] - 10, x1 = kts[1],
                 y0 = 0, ...)
      }else if(validPlots[i]=='survival'){
        points(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "l",
               ...)
      }else if(validPlots[i]=='quantile'){
        points(x = plotStructure$cdf, y = plotStructure$points, type = "l",
               ...)
      }else{
        points(x = plotStructure$points, y = plotStructure[[validPlots[i]]], type = "h",
               ...)
      }}
  }


  # now plot!
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

  else{
    # continuous case
    if(testContinuous(x)){
      if(add == FALSE){
        plot_continuous(validPlots,plotStructure,func_notation)
      } else {plot_continuous_lines(validPlots,plotStructure)}}
    # discrete case
    if(testDiscrete(x)){
      if(add == FALSE){
        plot_discrete(validPlots,plotStructure,func_notation)
      } else {plot_discrete_lines(validPlots,plotStructure)}}
    # mixture case
    if(testMixture(x)){
     decomposed <- decomposeMixture(x)
     nDiscrete <- length(decomposed$models$discrete)
     nContinuous <- length(decomposed$models$continuous)

     if(decomposed == FALSE){
       # mixture: con + con
       if(nDiscrete == 0 & nContinuous > 0){
         plot_continuous(validPlots,plotStructure,func_notation)}
       # mixture: discrete + discrete
       else if(nDiscrete > 0 & nContinuous == 0){
         plot_discrete(validPlots,plotStructure,func_notation)}
       # mixture: continuous + discrete
       else if(nDiscrete > 0 & nContinuous > 0){
         # plot dots at discrete vales
         # replace discrete values in plotStructure$points with NA, then draw a line plot.
         # The line plot will break at NAs
       }}

     # if decomposed == TRUE
     else{
       # con + con
       if(nDiscrete == 0 & nContinuous > 0){
         ###
         }
       # dis + dis
       else if(nDiscrete > 0 & nContinuous == 0){
         ###
         }
       # con + dis
       else if(nDiscrete > 0 & nContinuous > 0){
         ###
       }}}}

  # return graphical parameters to the original settings
  par(parDefault)
}

