#' @export
plot.Distribution <- function(x, fun=c('pdf','cdf'), npoints = 3000,
                              plot = TRUE, ask = FALSE, arrange = TRUE){
  #######################################################################
  #
  #   To set up quantitis required to produce pdf/cdf/quantile/survival/
  #   hazard/cumulative hazard plots. Arguments:
  #
  #   x         A SDistribution class object
  #   fun       List of plots to be produced
  #   npoints   Number of evaluation points to be generated for each
  #             distribution, the default is set to be 3,000.
  #   plot      A logical factor indicated whether to produce plots for
  #             the distribution object. If false, a list containing
  #             graphical information (evaluation points, pdf, cdf, etc.)
  #             will be returned.
  #   ask A logical factor that allows the users to specify the
  #             way that plots are displayed. If TRUE, one plot will be
  #             produced at one time and the users can see the next plot
  #             by hitting <return>. If FALSE, all required plots will be
  #             shown on the same page.
  #   layout.row A logical factor that only works if ask == FALSE.
  #             When ask == TRUE, this argument will be ignored.
  #             If TRUE, the layout of figures in the plot window will be
  #             decided by default (1*3 for three plots, 2*2 for 4 plots
  #             and 2*3 for 5/6 plots). Once plots are produced, the
  #             graphical parameters prior to plotting are retrieved.
  #             If FALSE, the users can change graphical parameters
  #             via par() in the global environment.
  #   layout.col Works in the same way as mfrow that changes the layout
  #             figures and only applies if ask == FALSE. If TRUE,
  #             figures will be displayed in a columnwise order. The
  #             default is set to be FALSE. If both layout.row and
  #             layout.col are TRUE, layout.col prevails.
  #   margin    Similiar to mfrow, only works if ask == FALSE. The
  #             margin of plots are changed when mar == TRUE.
  #   decompose A logical factor that works only for mixture distributions,
  #             if TRUE, the weighted quantities for each distribution will
  #             be displayed.
  #
  #######################################################################

  #######################################################################
  #######                         validations                     #######
  #######################################################################

  plotFuns <- c("pdf","cdf","quantile","survival","hazard","cumhazard")
  # check user input plot names are correct
  if(!all(fun %in% plotFuns))
    stop("invalid plot function")

  if("cdf" %in% fun & !x$.__enclos_env__$private$.isCdf){
    message("This distribution does not have a cdf expression. Use the
            FunctionImputation decorator to impute a numerical cdf.")
    fun = fun[!(fun %in% c("cdf", "survival", "hazard","cumhazard"))]
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
    plotStructure <- data.table::data.table(cdf = seq(0,1,length.out = npoints))
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
  if("cumhazard" %in% fun)
    plotStructure$cumhazard <- -log(1 - plotStructure$cdf)


  #######################################################################
  #######                     graphical parameters                #######
  #######################################################################

  if(length(fun) == 1)
    ask = arrange = FALSE

  if(plot){
    if(ask | arrange){
      def.par <- par(no.readonly = TRUE)
      par(ask = ask)
    }

    if(arrange & !ask){
      data = 1:length(fun)
      if(length(fun) == 3 | length(fun) == 5) data = c(data, 0)

      n = switch(length(fun),
                 "1" = list(nrow = 1, ncol = 1),
                 "2" = list(nrow = 1, ncol = 2),
                 "3" = list(nrow = 2, ncol = 2),
                 "4" = list(nrow = 2, ncol = 2),
                 "5" = list(nrow = 2, ncol = 3),
                 "6" = list(nrow = 2, ncol = 3)
      )

      layout(do.call(matrix, c(list(byrow = TRUE, data = data), n)))

    }

    if(testContinuous(x))
      .plot_continuous(fun,plotStructure,x$strprint())
    else if(testDiscrete(x))
      .plot_discrete(fun,plotStructure,x$strprint())

    if(ask | arrange)
      par(def.par)
  }

  invisible(plotStructure)
}


# FUN_ONE: continuous distribution
.plot_continuous <- function(fun,plotStructure,name){
  plots = list()

  if("pdf" %in% fun){
    plots$pdf = list(x = plotStructure$points, y = plotStructure$pdf, type = "l",
         main = paste(name,"Pdf"), xlab = "x", ylab = "f(x)")
  }

  if ("cdf" %in% fun){
    plots$cdf = list(x = plotStructure$points, y = plotStructure$cdf, type = "l",
         main = paste(name,"cdf"), xlab = "x", ylab = "F(x)")
  }

  if("quantile" %in% fun){
    plots$quantile = list(x = plotStructure$cdf,
         y = plotStructure$points, type = "l",
         main = paste(name,"quantile"), xlab = "q", ylab = parse(text = "F^-1*(q)"))
  }

  if ("survival" %in% fun){
    plots$survival = list(x = plotStructure$points, y = plotStructure$survival, type = "l",
         main = paste(name,"Survival"), xlab = "x", ylab = "S(x)")
  }

  if ("hazard" %in% fun){
    plots$hazard = list(x = plotStructure$points, y = plotStructure$hazard, type = "l",
         main = paste(name,"Hazard"), xlab = "x", ylab = "h(x)")
  }

  if("cumhazard" %in% fun){
    plots$cumhazard = list(x = plotStructure$points, y = plotStructure$cumhazard,
           type = "l",main=paste(name,"cumhazard"),xlab='x',ylab="H(x)")
  }

  if(length(fun) > 1)
    plots = plots[match(fun, names(plots))]
  lapply(plots, function(x) do.call(plot, x))
}


# FUN_THREE: discrete distribution
.plot_discrete <- function(fun,plotStructure,name){
  plots = list()

  if("pdf" %in% fun)
      plots$pdf = list(x = plotStructure$points, y = plotStructure$pdf, type = "h",
                               main = paste(name,"Pdf"), xlab = "x", ylab = "f(x)")

  if("cumhazard" %in% fun){
      plots$cumhazard$plot = list(x = plotStructure$points, y = plotStructure$cumhazard, type = "n",
                          main= paste(name,"cumhazard"),xlab='x',ylab=expression(Lambda(x)))
      plots$cumhazard$points = list(x = plotStructure$points, y = plotStructure$cumhazard, pch = 16)
      plots$cumhazard$segments = list(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                 y0 = plotStructure$cumhazard)
  }

  if("cdf" %in% fun){
    plots$cdf$plot = list(x = plotStructure$points, y = plotStructure$cdf, type = "n",
             main = paste(name,"cdf"), xlab = "x", ylab = parse(text = "F(x)"))
      plots$cdf$points = list(x = plotStructure$points, y = plotStructure$cdf, pch = 16)
      plots$cdf$segments = list(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                 y0 = plotStructure$cdf)
  }

  if('quantile' %in% fun){
      plots$quantile$plot = list(x = plotStructure$cdf, y = plotStructure$points, type = "n",
                         main = paste(name,"quantile"), xlab = "q", ylab = parse(text = "F^-1*(q)"))
      plots$quantile$points = list(x = plotStructure$cdf, y = plotStructure$points, pch = 16)
      plots$quantile$segments = list(x0 = plotStructure$cdf, y0 = plotStructure$points,
                 y1 = plotStructure$points+1)
  }

  if("survival" %in% fun){
    plots$survival$plot = list(x = plotStructure$points, y = plotStructure$survival, type = "n",
                          main=paste(name,"Survival"),xlab='x',ylab="S(x)")
    plots$survival$points = list(x = plotStructure$points, y = plotStructure$survival, pch = 16)
    plots$survival$segments = list(x0 = plotStructure$points, x1 = plotStructure$points + 1,
                 y0 = plotStructure$survival)
  }

  if("hazard" %in% fun)
    plots$hazard = list(x = plotStructure$points, y = plotStructure$hazard, type = "h",
           main = paste(name,"Hazard"), xlab = "x", ylab = "h(x)")

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
