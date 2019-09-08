#' @export
plot.Distribution <- function(x, fun=c('pdf','cdf'), nPoints = 3000,
                              plot = TRUE, iterative = FALSE, cowplot = TRUE,
                              ...){
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
  if(!all(fun %in% plotFuns))
    stop("invalid plot function")

  if("cdf" %in% fun & !x$.__enclos_env__$private$.isCdf){
    message("This distribution does not have a cdf expression. Use the
            FunctionImputation decorator to impute a numerical cdf.")
    fun = fun[!(fun %in% c("cdf", "survival", "hazard","cumHazard"))]
  }

  if("pdf" %in% fun & !x$.__enclos_env__$private$.isPdf){
    message("This distribution does not have a pdf expression. Use the
            FunctionImputation decorator to impute a numerical pdf.")
    fun = fun[!(fun %in% c("pdf", "hazard"))]
  }

  if("quantile" %in% fun & !x$.__enclos_env__$private$.isQuantile){
    message("This distribution does not have a quantile expression. Use the
            FunctionImputation decorator to impute a numerical quantile.")
    fun = fun[!(fun %in% c("quantile"))]
  }

  #######################################################################
  #######                   plottable structure                   #######
  #######################################################################

  if(testDiscrete(x) & x$support()$length() != Inf){
    plotStructure <- data.table::data.table(points = x$support()$elements())
    plotStructure$cdf <- x$cdf(plotStructure$points)
  } else {
    plotStructure <- data.table::data.table(cdf = seq(0,1,length.out = nPoints))
    plotStructure$points <- x$quantile(plotStructure$cdf)
    plotStructure <- plotStructure[,2:1]

    if(testDiscrete(x))
      plotStructure <- aggregate(cdf ~ points, plotStructure, max)
  }

  plotStructure$pdf <- x$pdf(plotStructure$points)

  if("survival" %in% fun)
    plotStructure$survival <- 1 - plotStructure$cdf
  if("hazard" %in% fun)
    plotStructure$hazard <- plotStructure$pdf/(1 - plotStructure$cdf)
  if("cumHazard" %in% fun)
    plotStructure$cumHazard <- -log(1 - plotStructure$cdf)


  #######################################################################
  #######                     graphical parameters                #######
  #######################################################################

  # update par()
 # par(ask = iterative)

  # set the number of plots on one page
  # check the number of plots to be produced
  #if(iterative == FALSE){
   #mfList <- list()
    #mfList[[1]] <- c(1,1)
    #mfList[[2]] <- c(1,2)
    #mfList[[3]] <- c(1,3)
    #mfList[[4]] <- c(2,2)
    #mfList[[5]] <- c(2,3)
    #mfList[[6]] <- c(2,3)
    # update graphical parameters
    #if (layout.row == TRUE & layout.col == FALSE){
      #par(mfrow = mfList[[nPlots]])}
    #if (layout.col == TRUE){
      #par(mfcol = mfList[[nPlots]])}


  # set margins
  #if(iterative == FALSE & margin == TRUE){
    #par(mar=c(4,4,2,2))


  #######################################################################
  #######                       generate plots                    #######
  #######################################################################

  # now plot!
  if(!plot)
    return(plotStructure)
  else{
    if(testContinuous(x))
      plots <- .plot_continuous(fun,plotStructure,cowplot,x$strprint(),...)
    else if(testDiscrete(x))
      plots <- .plot_discrete(fun,plotStructure,cowplot,x$strprint(),...)
    if(cowplot)
      cowplot::plot_grid(plotlist = plots)
  }
}


# FUN_ONE: continuous distribution
.plot_continuous <- function(fun,plotStructure,cowplot,name,...){
  cumH_plot = quan_plot = pdf_plot = cdf_plot = surv_plot = hazard_plot = NULL

  if("cumHazard" %in% fun){
      cumH_plot = ~plot(x = plotStructure$points, y = plotStructure$cumHazard,
           type = "l",main=paste(name,"cumHazard"),xlab='x',ylab="H(x)",...)
  }

  if("quantile" %in% fun){
    quan_plot <- ~plot(x = plotStructure$cdf,
           y = plotStructure$points, type = "l",
           main = paste(name,"quantile"), xlab = "q", ylab = parse(text = "F^-1*(q)",...))
  }

  if("pdf" %in% fun){
    pdf_plot <- ~plot(x = plotStructure$points, y = plotStructure$pdf, type = "l",
                               main = paste(name,"Pdf"), xlab = "x", ylab = "f(x)",...)
  }

  if ("cdf" %in% fun){
    cdf_plot <- ~plot(x = plotStructure$points, y = plotStructure$cdf, type = "l",
                               main = paste(name,"cdf"), xlab = "x", ylab = "F(x)",...)
  }

  if ("survival" %in% fun){
    surv_plot <- ~plot(x = plotStructure$points, y = plotStructure$survival, type = "l",
                               main = paste(name,"Survival"), xlab = "x", ylab = "S(x)",...)
  }

  if ("hazard" %in% fun){
    hazard_plot <- ~plot(x = plotStructure$points, y = plotStructure$hazard, type = "l",
                               main = paste(name,"Hazard"), xlab = "x", ylab = "h(x)",...)
  }

  list_plots <- list(pdf = pdf_plot,cdf = cdf_plot, quantile = quan_plot, cumHazard = cumH_plot,survival = surv_plot,
                     hazard = hazard_plot)
  list_plots <- list_plots[match(fun,names(list_plots))]

  if(cowplot)
    list_plots <- lapply(list_plots, cowplot::as_grob)
  else
    lapply(list_plots, plot)

  return(list_plots)
 }


# FUN_THREE: discrete distribution
.plot_discrete <- function(fun,plotStructure,cowplot,name,...){
  cumH_plot = quan_plot = pdf_plot = cdf_plot = surv_plot = hazard_plot = NULL

  if("pdf" %in% fun)
      pdf_plot <- ~plot(x = plotStructure[,"points"], y = plotStructure[,"pdf"], type = "h",
                               main = paste(name,"Pdf"), xlab = "x", ylab = "f(x)", ...)
  if("cumHazard" %in% fun)
      cumH_plot <- ~{plot(x = plotStructure$points, y = plotStructure$cumHazard, type = "n",
                          main= paste(name,"cumHazard"),xlab='x',ylab=expression(Lambda(x)), ...)
        points(x = plotStructure$points, y = plotStructure$cumHazard, pch = 16)
        segments(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                 y0 = plotStructure$cumHazard)
      }
  if("cdf" %in% fun)
      cdf_plot<- ~{
        plot(x = plotStructure$points, y = plotStructure$cdf, type = "n",
             main = paste(name,"cdf"), xlab = "x", ylab = parse(text = "F(x)"),...)
        points(x = plotStructure$points, y = plotStructure$cdf, pch = 16)
        segments(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                 y0 = plotStructure$cdf)
      }
  if('quantile' %in% fun)
      quan_plot<- ~{plot(x = plotStructure$cdf, y = plotStructure$points, type = "n",
                         main = paste(name,"quantile"), xlab = "q", ylab = parse(text = "F^-1*(q)"), ...)
        points(x = plotStructure$cdf, y = plotStructure$points, pch = 16)
        segments(x0 = plotStructure$cdf, y0 = plotStructure$points,
                 y1 = plotStructure$points+1)
      }
  if("survival" %in% fun)
      surv_plot <- ~{plot(x = plotStructure$points, y = plotStructure$survival, type = "n",
                          main=paste(name,"Survival"),xlab='x',ylab="S(x)", ...)
        points(x = plotStructure$points, y = plotStructure$survival, pch = 16)
        segments(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                 y0 = plotStructure$survival)
      }
  if("hazard" %in% fun)
    hazard_plot <- ~plot(x = plotStructure[,"points"], y = plotStructure[,"hazard"], type = "h",
           main = paste(name,"Hazard"), xlab = "x", ylab = "h(x)", ...)

  list_plots <- list(pdf = pdf_plot,cdf = cdf_plot, quantile = quan_plot, cumHazard = cumH_plot,survival = surv_plot,
                     hazard = hazard_plot)
  list_plots <- list_plots[match(fun,names(list_plots))]

  if(cowplot)
    list_plots <- lapply(list_plots, cowplot::as_grob)
  else
    lapply(list_plots, plot)

  return(list_plots)
 }
