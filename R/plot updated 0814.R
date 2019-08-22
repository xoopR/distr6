plot.Distribution <- function(x, fun=c('pdf','cdf'), nPoints = 3000,
                              plot = TRUE, iterative = FALSE,
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
  #   decompose A logical factor that works only for mixture distributions, 
  #             if TRUE, the weighted quantities for each distribution will
  #             be displayed.
  #   ...       All graphical parameters in current R plot function can 
  #             be added as additional arguments.
  #
  #######################################################################
  
  #######################################################################
  #######                         validations                     #######
  #######################################################################
  
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
  
  names=c("pdf","cdf","hazard","survival")
  notations=c("f(x)","F(x)","h(x)","S(x)")
  func_notation=fun
  
  for(i in 1:length(names)){
    func_notation=replace(func_notation,func_notation==names[i],notations[i])
  }
  
  
  #######################################################################
  #######                     graphical parameters                #######
  #######################################################################
  
  # update par()
  par(ask = iterative)
  
  # set the number of plots on one page
  # check the number of plots to be produced
  if(iterative == FALSE){
    nPlots <- length(fun)
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
  
  # now plot!
  if(plot == FALSE){
    return(plotStructure$qty[,c("points",fun)])
  } 
  
  else{
    # continuous case
    if(testContinuous(x)){
        .plot_continuous(fun,plotStructure,func_notation,...)
      }
    # discrete case
    if(testDiscrete(x)){
        .plot_discrete(fun,plotStructure,func_notation,...)
      }
}
}


# FUN_ONE: continuous distribution
.plot_continuous <- function(fun,plotStructure,func_notation,...){
  for(i in 1:length(fun)){
    if(fun[i]=='cumHazard'){
      plot(x = plotStructure$qty[,"points"], y = plotStructure$qty[,fun[i]], 
           type = "l",main=fun[i],xlab='x',ylab=expression(Lambda(x)),...)
    }else if(fun[i]=='quantile'){
      plot(x = plotStructure$qty[,"quantile"], 
           y = plotStructure$qty[,"points"], type = "l",
           main = "quantile", xlab = "q", ylab = parse(text = "F^-1*(q)"),...)
    }else{
      plot(x = plotStructure$qty[,"points"], y = plotStructure$qty[,fun[i]], type = "l",main=fun[i],xlab='x',ylab=func_notation[i],...)
    }}}


# FUN_THREE: discrete distribution
.plot_discrete <- function(fun,plotStructure,func_notation,...){
  for(i in 1:length(fun)){
    if(fun[i]=='cumHazard'){
      plot(x = plotStructure$qty[,"points"], y = plotStructure$qty[,fun[i]], type = "h",
           main=fun[i],xlab='x',ylab=expression(Lambda(x)),...)
    }else if(fun[i]=='cdf'){
      plot(ecdf(plotStructure$qty[,"points"]), main='cdf',xlab='x',ylab='F(x)',...)
    }else if(fun[i]=='quantile'){
      plot(x = unique(plotStructure$qty[,"cdf"]), y = plotStructure$qty[,"points"], type = "s",
           main = "quantile", xlab = "q", ylab = parse(text = "F^-1*(q)"),...)
    }else{
      plot(x = plotStructure$qty[,"points"], y = plotStructure$qty[,fun[i]], type = "h",
           main=fun[i],xlab='x',ylab=func_notation[i],...)
    }}}



my.binom <- Binomial$new()
my.geom=Geometric$new()
my.norm <- Normal$new(mean = 2, sd = 1.5)
my.norm.two <- Normal$new(mean = 2.5, sd = 1)


plot(my.norm, c("pdf","cdf","quantile","hazard","cumHazard","survival"), col = "red", plot = F)
plot(my.norm, c("pdf","cdf","quantile","hazard","cumHazard","survival"), col = "red", plot = T)
plot(my.binom, c("pdf","cdf","quantile","hazard","cumHazard","survival"), col = "red", plot = T)

plot(my.binom, "quantile")
lines(my.geom, "quantile", col = "red")

plot(my.norm, "pdf")
plot(my.norm, "pdf", ylim = c(0, 0.5))
lines(my.norm.two, "pdf", col = "red")

