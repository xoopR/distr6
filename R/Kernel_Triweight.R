#' @title Triweight Kernel
#'
#' @description Mathematical and statistical functions for the Triweight kernel defined by the pdf,
#' \deqn{f(x) = 35/32(1 - x^2)^3}
#' over the support \eqn{x \in (-1,1)}{x \epsilon (-1,1)}.
#'
#' @details The quantile function is omitted as no closed form analytic expression could
#' be found, decorate with FunctionImputation for numeric results.
#'
#' @name Triweight
#' @template class_distribution
#' @template class_kernel
#' @template method_pdfsquared2Norm
#'
#' @export
Triweight <- R6Class("Triweight",
  inherit = Kernel, lock_objects = F,
  public = list(
    name = "Triweight",
    short_name = "Triw",
    description = "Triweight Kernel",

    #' @description
    #' The squared 2-norm of the pdf is defined by
    #' \deqn{\int_a^b (f_X(u))^2 du}
    #' where X is the Distribution, \eqn{f_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    pdfSquared2Norm = function(x = 0, upper = Inf) {
      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {

        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]

        if (abs(xi) >= 2) {
          ret[i] <- 0
        } else if (xi >= 0 & xi <= 2) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= 1) {
            ret[i] <- (35 / 32)^2 *
              (- xi^13 / 12012 + xi^11 / 385 - (4 * xi^9) / 105 + (16 * xi^7) / 35 +
              ui^3 * (xi^6 - 8 * xi^4 + 9 * xi^2 - 2) - (16 * xi^6) / 35 +
              ui^7 * (xi^6 / 7 - (48 * xi^4) / 7 + (102 * xi^2) / 7 - 20 / 7) +
              ui^5 * (- (3 * xi^6) / 5 + (54 * xi^4) / 5 - (78 * xi^2) / 5 + 3) +
              ui * (- xi^6 + 3 * xi^4 - 3 * xi^2 + 1) +
              ui^2 * (3 * xi^5 - 6 * xi^3 + 3 * xi) +
              ui^6 * (3 * xi^5 - 16 * xi^3 + 10 * xi) - (3 * xi^5) / 4 +
              ui^8 * (- (3 * xi^5) / 4 + 9 * xi^3 - (15 * xi) / 2) +
              ui^4 * (- (9 * xi^5) / 2 + 14 * xi^3 - (15 * xi) / 2) +
              ui^9 * ((5 * xi^4) / 3 - 7 * xi^2 + 5 / 3) + (64 * xi^4) / 105 + xi^3 +
              ui^10 * (3 * xi - 2 * xi^3) +
              ui^11 * ((15 * xi^2) / 11 - 6 / 11) - (256 * xi^2) / 385 -
              (ui^12 * xi) / 2 - xi / 2 + ui^13 / 13 + 1024 / 3003)
          } else if (ui >= 1 | ui == Inf) {
            ret[i] <- 350 / 429 - (35 * xi^2) / 22 + (35 *  xi^4) / 24 -
              (35 *  xi^6) / 32 + (35 * xi^7) / 64 -
              (35 * xi^9) / 768 + (35 * xi^11) / 11264 -
              (175 * xi^13) / 1757184
          }
        } else if (xi <= 0 & xi >= - 2) {
          if (ui <= -1) {
            ret[i] <- 0
          } else if (ui <= xi + 1 & ui >= - 1) {
            ret[i] <- (35 / 32)^2 *
              (- (16 * xi^6) / 35 + ui^7 *
                 (xi^6 / 7 - (48 * xi^4) / 7 + (102 * xi^2) / 7 - 20 / 7) +
                 ui^5 * (- (3 * xi^6) / 5 + (54 * xi^4) / 5 - (78 * xi^2) / 5 + 3) +
                 ui * (- xi^6 + 3 * xi^4 - 3 * xi^2 + 1) +
                 ui^2 * (3 * xi^5 - 6 * xi^3 + 3 * xi) +
                 ui^6 * (3 * xi^5 - 16 * xi^3 + 10 * xi) - (3 * xi^5) / 4 +
                 ui^8 * (- (3 * xi^5) / 4 + 9 * xi^3 - (15 * xi) / 2) +
                 ui^4 * (- (9 * xi^5) / 2 + 14 * xi^3 - (15 * xi) / 2) +
                 ui^9 * ((5 * xi^4) / 3 - 7 * xi^2 + 5 / 3) +
                 (64 * xi^4) / 105 + xi^3 +
                 ui^10 * (3 * xi - 2 * xi^3) +
                 ui^11 * ((15 * xi^2) / 11 - 6 / 11) - (256 * xi^2) / 385 -
                 (ui^12 * xi) / 2 - xi / 2 +
                 ui^13 / 13 + 1024 / 3003)
          } else if (ui == Inf | ui >= xi + 1) {
            ret[i] <- 350 / 429 - (35 * xi^2) / 22 + (35 * xi^4) / 24 -
              (35 * xi^6) / 32 - (35 * xi^7) / 64 +
              (35 * xi^9) / 768 - (35 * xi^11) / 11264 +
              (175 * xi^13) / 1757184
          }
        }
      }
      return(ret)
    },

    #' @description
    #' The squared 2-norm of the cdf is defined by
    #' \deqn{\int_a^b (F_X(u))^2 du}
    #' where X is the Distribution, \eqn{F_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    cdfSquared2Norm = function(x = 0, upper = 0) {

      xl = length(x)
      ul = length(upper)
      len = max(xl, ul)

      ret <- numeric(len)
      for (i in seq(len)) {

        xi = x[ifelse(i %% xl == 0, xl, i %% xl)]
        ui = upper[ifelse(i %% ul == 0, ul, i %% ul)]

        if (xi >= 0 & xi <= 2) {
          if (ui <= - 1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= 1) {
            ret[i] <-  (1 / 10543104) *
              (8 * (1 + ui)^9 *
                 (54740 + 3 * ui *
                    (- 54396 + ui *
                       (72924 + 55 * ui * (- 1011 + ui *
                                                 (459 + 13 * (-9 + ui) * ui))))) -
                      5148 * (1 + ui)^8 *
                 (- 16 + ui * (29 + 5 * (- 4 + ui) * ui))^2 * xi +
                      840 * (1 + ui)^7 *
                 (872 + ui *
                    (- 6104 + ui *
                       (14120 + 9 * ui *
                          (-1799 + ui *
                             (1137 + 55 * (- 7 + ui) * ui))))) * xi^2 -
                 30030 * (- 1 + ui)^2 * (1 + ui)^6 *
                 (- 35 + ui * (- 52 + ui *
                                       (138 + 25 * (- 4 + ui) * ui))) * xi^3 +
                      10920 * (1 + ui)^5 *
                 (- 68 + ui *
                    (340 + ui *
                       (- 228 + 5 * ui *
                          (- 85 + ui * (137 + 15 * (- 5 + ui) * ui))))) * xi^4 -
                      18018 * (1 + ui)^5 *
                 (35 + ui *
                    (17 + 15 * ui *
                       (- 15 + ui * (19 + 2 * (- 5 + ui) * ui)))) * xi^5 +
                      40040 * (- 2 + ui) *
                 (1 + ui)^5 *
                 (- 4 + ui *
                    (18 + 5 * (- 3 + ui) * ui)) * xi^6 -
                      6435 * (1 + ui)^5 *
                 (- 35 + ui *
                    (47 + 5 * (- 5 + ui) * ui)) * xi^7 - 80080 * xi^9 +
                      4368 * xi^11 - 210 * xi^13 + 5 * xi^15)
          } else if (ui >= 1 & ui <= xi + 1) {
            ret[i] <- 1042 / 1287 - xi / 2 - (175 * xi^2) / 429 +
              (35 * xi^4) / 264 + (7 * xi^5) / 16 - (35 * xi^6) / 72 +
              (5 * xi^7) / 32 - (35 * xi^9) / 4608 + (7 * xi^11) / 16896 -
              (35 * xi^13) / 1757184 + (5 * xi^15) / 10543104 +
              ((1 / 32) * (- (67 / 2) + 16 * ui +
                             (35 * ui^2) / 2 + (35 / 4) *
                             (- (ui - xi)^4 + (-1 + xi)^4) +
                         (7 / 2) * ((ui - xi)^6 - (- 1 + xi)^6) +
                           (5 / 8) * (- (ui - xi)^8 + (- 1 + xi)^8) +
                           35 * xi - 35 * ui * xi))
          } else if (ui >= xi + 1) {
            ret[i] <- 1042 / 1287 - xi / 2 - (175 * xi^2) / 429 +
              (35 * xi^4) / 264 + (7 * xi^5) / 16 - (35 * xi^6) / 72 +
              (5 * xi^7) / 32 - (35 * xi^9) / 4608 + (7 * xi^11) / 16896 -
              (35 * xi^13) / 1757184 + (5 * xi^15) / 10543104 +
              (xi - (7 * xi^5) / 16 + (7 * xi^6) / 16 - (5 * xi^7) / 32 +
                 (5 * xi^8) / 256) + (ui - xi - 1)
          }
        } else if (xi >= -2 & xi <= 0) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= - 1) {
            ret[i] <- 0
          } else if (ui >= - 1 & ui <= xi + 1) {
            ret[i] <-  (1 / 10543104) * ((1 + ui)^5 * (8 * (1 + ui)^4 *
                      (54740 + 3 * ui * (- 54396 +
                      ui * (72924 + 55 * ui *
                      (- 1011 + ui * (459 + 13 * (- 9 + ui) * ui))))) -
                      5148 * (1 + ui)^3 *
                      (- 16 + ui * (29 + 5 * (- 4 + ui) * ui))^2 * xi +
                      840 * (1 + ui)^2 *
                      (872 + ui *
                      (- 6104 + ui *
                      (14120 + 9 * ui *
                      (- 1799 + ui *
                      (1137 + 55 * (-7 + ui) * ui))))) *
                      xi^2 - 30030 * (- 1 + ui)^2 * (1 + ui) *
                      (- 35 + ui * (- 52 + ui * (138 + 25 * (-4 + ui) *
                      ui))) * xi^3 +
                      10920 * (- 68 + ui * (340 + ui *
                      (- 228 + 5 * ui *
                      (- 85 + ui * (137 + 15 * (- 5 + ui) * ui))))) *
                      xi^4 - 18018 * (35 + ui *
                      (17 + 15 * ui * (- 15 + ui *
                      (19 + 2 * (- 5 + ui) * ui)))) *
                      xi^5 + 40040 * (- 2 + ui) *
                        (- 4 + ui * (18 + 5 * (- 3 + ui) *
                      ui)) * xi^6 -
                      6435 * (- 35 + ui * (47 + 5 * (-5 + ui) *
                      ui)) * xi^7))
          } else if (ui >= xi + 1 & ui <= 1) {
            ret[i] <- 1042 / 1287 + xi / 2 - (175 * xi^2) / 429 +
              (35 * xi^4) / 264 - (7 * xi^5) / 16 - (35 * xi^6) /  72 -
              (5 * xi^7) / 32 + (35 * xi^9) / 4608 -
              (7 * xi^11) / 16896 + (35 * xi^13) / 1757184 -
              (5 * xi^15) / 10543104 +
              (- (221 / 256) + ui / 2 + (35 * ui^2) / 64 -
                 (35 * ui^4) / 128 + (7 * ui^6) / 64 - (5 * ui^8) /
                 256 - xi + (7 * xi^5) / 16 + (7 * xi^6) / 16 +
                 (5 * xi^7) / 32 + (5 * xi^8) / 256)
          } else if (ui >= 1) {
            ret[i] <- 1042 / 1287 + xi / 2 - (175 * xi^2) / 429 +
              (35 * xi^4) / 264 - (7 * xi^5) / 16 -
              (35 * xi^6) / 72 - (5 * xi^7) / 32 +
              (35 * xi^9) / 4608 - (7 * xi^11) / 16896 +
              (35 * xi^13) / 1757184 - (5 * xi^15) / 10543104 +
              (-xi + (7 * xi^5) / 16 + (7 * xi^6) / 16 +
                 (5 * xi^7) / 32 + (5 * xi^8) / 256) + (ui - 1)
          }
        } else if (xi >= 2) {
          if (ui <=  -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= 1) {
            ret[i] <- 0
          } else if (ui >= 1 & ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= xi + 1) {
            ret[i] <- (- (1 / 256)) * (1 + ui - xi)^5 *
                      (- 35 + 5 * ui^3 - 47 * xi - 25 * xi^2 - 5 * xi^3 -
                      5 * ui^2 * (5 + 3 * xi) + ui * (47 + 50 * xi + 15 * xi^2))
          } else if (ui >= xi + 1) {
            ret[i] <- ui - xi
          }
        } else if (xi <= -2) {
          if (ui <= xi - 1) {
            ret[i] <- 0
          } else if (ui >= xi - 1 & ui <= xi + 1) {
            ret[i] <- 0
          } else if (ui >= xi + 1 & ui <= -1) {
            ret[i] <- 0
          } else if (ui >= -1 & ui <= 1) {
            ret[i] <- 35 / 256 + ui / 2 + (35 * ui^2) / 64 -
              (35 * ui^4) / 128 + (7 * ui^6) / 64 -
              (5 * ui^8) / 256
          } else if (ui >= 1) {
            ret[i] <- ui
          }
        }
      }
      return(ret)
    },

    #' @description
    #' The variance of a distribution is defined by the formula
    #' \deqn{var_X = E[X^2] - E[X]^2}
    #' where \eqn{E_X} is the expectation of distribution X. If the distribution is multivariate the
    #' covariance matrix is returned.
    #' @param ... Unused.
    variance = function(...) {
      return(1 / 9)
    }
  ),

  private = list(
    .isQuantile = 0L,
    .pdf = function(x, log = FALSE) {
      C_TriweightKernelPdf(x, log)
    },
    .cdf = function(x, lower.tail = TRUE, log.p = FALSE) {
      C_TriweightKernelCdf(x, lower.tail, log.p)
    }
  )
)

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Triw", ClassName = "Triweight",
    Support = "[-1,1]", Packages = "-"
  )
)
