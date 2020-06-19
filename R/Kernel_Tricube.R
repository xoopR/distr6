#' @title Tricube Kernel
#'
#' @description Mathematical and statistical functions for the Tricube kernel defined by the pdf,
#' \deqn{f(x) = 70/81(1 - |x|^3)^3}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The cdf and quantile functions are omitted as no closed form analytic expressions could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Tricube
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Tricube <- R6Class("Tricube",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Tricube",
    short_name = "Tric",
    description = "Tricube Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0) {

    cond1 <-  (70/81)^2 * (6561/6916 - (19683*x^2)/13090 + (9*x^4)/5 - (729*x^6)/182 +
              (747*abs(x)^7)/140 - (729*x^8)/220 + (81*abs(x)^9)/70- (31*x^10)/140 +
              (111*abs(x)^13)/20020 - (3*x^16)/40040 + abs(x)^19/461890)

    cond2 <- (70/81)^2 * (2592/1729 - (16 *abs(x))/5 + (5832*abs(x)^2)/935 - (972*abs(x)^3)/91 +
             (66*abs(x)^4)/5 - (729*abs(x)^5)/55 + (9963*abs(x)^6)/910 - (969*abs(x)^7)/140 +
             (729*abs(x)^8)/220 - (81 *abs(x)^9)/70 + (31*abs(x)^10)/140 - (57*abs(x)^13)/20020 +
             (3*abs(x)^16)/40040 - abs(x)^19/923780)

      return(ifelse(abs(x) >= 2,0, ifelse(abs(x) <= 1, cond1, cond2)))
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    variance = function() {
      return(35 / 243)
    }
  ),

  private = list(
    .pdf = function(x, log = FALSE) {
      C_TricubeKernelPdf(x, log)
    }
  )
)

.distr6$kernels <- rbind(.distr6$kernels, data.table::data.table(ShortName = "Tric", ClassName = "Tricube", Support = "[-1,1]", Packages = "-"))
