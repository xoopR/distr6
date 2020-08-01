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
    pdfSquared2Norm = function(x = 0, upper = Inf) {

      fixed1 = function(x, upper){

        ret1 = (upper*(-(4900*x^9)/6561+(4900*x^6)/2187-(4900*x^3)/2187+4900/6561)
                +upper^2*((2450*x^8)/729-(4900*x^5)/729+(2450*x^2)/729)
                +upper^3*(-(19600*x^7)/2187+(24500*x^4)/2187-(4900*x)/2187)
                +upper^4*(-(1225*x^9)/2187+(37975*x^6)/2187-(28175*x^3)/2187+2450/2187)
                +upper^5*((980*x^8)/243-(19600*x^5)/729+(7840*x^2)/729)
                +upper^6*(-(9800*x^7)/729+(71050*x^4)/2187-(12250*x)/2187)
                +upper^7*(-(700*x^9)/2187+(20300*x^6)/729-(63700*x^3)/2187+3500/2187)
                +upper^8*((1225*x^8)/486-(9800*x^5)/243+(13475*x^2)/729)
                +upper^9*(-(19600*x^7)/2187+(93100*x^4)/2187-(49000*x)/6561)
                +upper^10*(-(490*x^9)/6561+(41650*x^6)/2187-(71050*x^3)/2187+9800/6561)
                +upper^11*((4900*x^8)/8019-(19600*x^5)/729+(137200*x^2)/8019)
                +upper^12*(-(4900*x^7)/2187+(57575*x^4)/2187-(12250*x)/2187)
                +upper^13*((137200*x^6)/28431-(39200*x^3)/2187+24500/28431)
                +upper^14*((5950*x^2)/729-(4900*x^5)/729)+upper^15*((13720*x^4)/2187-(4900*x)/2187)
                +upper^16*(1225/4374-(8575*x^3)/2187) +(19600*upper^17*x^2)/12393 -(2450*upper^18*x)/6561
                +(4900*upper^19)/124659)
        return(ret1)

      }

      fixed2 = function(x, upper){

        ret2 = abs(upper)^4*((1225*abs(x)^9)/2187+(30625*abs(x)^6)/2187-(20825*abs(x)^3)/2187)+
          abs(upper)^10*((490*abs(x)^9)/6561+(40670*abs(x)^6)/2187+(12250*abs(x)^3)/2187)+
          abs(upper)^7*(-(700*abs(x)^9)/2187-(700*abs(x)^6)/27+(20300*abs(x)^3)/2187-700/2187)+
          abs(upper)*(-(4900*abs(x)^9)/6561+(4900*abs(x)^6)/2187-(4900*abs(x)^3)/2187+4900/6561)+
          abs(upper)^2*((2450*abs(x)^8)/729-(4900*abs(x)^5)/729+(2450*abs(x)^2)/729)+
          abs(upper)^8*((1225*abs(x)^8)/486+(2450*abs(x)^5)/81-(4900*abs(x)^2)/729)+
          abs(upper)^11*(-(4900*abs(x)^8)/8019-(196000*abs(x)^5)/8019+(9800*abs(x)^2)/8019)+
          abs(upper)^5*(-(980*abs(x)^8)/243-(7840*abs(x)^5)/729+(1960*abs(x)^2)/729)+
          abs(upper)^6*((9800*abs(x)^7)/729-(2450*abs(x)^4)/2187+(2450*abs(x))/2187)+
          abs(upper)^12*((4900*abs(x)^7)/2187+(45325*abs(x)^4)/2187-(2450*abs(x))/2187)+
          abs(upper)^3*(-(19600*abs(x)^7)/2187+(24500*abs(x)^4)/2187-(4900*abs(x))/2187)+
          abs(upper)^9*(-(19600*abs(x)^7)/2187-(4900*abs(x)^4)/243+(9800*abs(x))/6561)+
          abs(upper)^13*(-(137200*abs(x)^6)/28431-(313600*abs(x)^3)/28431+4900/28431)+
          abs(upper)^14*((4900*abs(x)^5)/729+(2450*abs(x)^2)/729)+
          abs(upper)^15*(-(13720*abs(x)^4)/2187-(980*abs(x))/2187)+
          (8575*abs(upper)^16*abs(x)^3)/2187-(19600*abs(upper)^17*abs(x)^2)/12393+
          (2450*abs(upper)^18*abs(x))/6561-(4900*abs(upper)^19)/124659
        return(ret2)

      }

      fixed3 = function(x, upper){

        ret3 = upper*((4900*x^9)/6561+(4900*x^6)/2187+(4900*x^3)/2187+4900/6561)+
          upper^7*((700*(x)^9)/2187+(20300*x^6)/729+(63700*x^3)/2187+3500/2187)+
          upper^10*(-(490*x^9)/6561-(41650*x^6)/2187-(71050*x^3)/2187-9800/6561)+
          upper^4*(-(1225*x^9)/2187-(37975*x^6)/2187-(28175*x^3)/2187-2450/2187)+
          upper^5*((980*x^8)/243+(19600*x^5)/729+(7840*x^2)/729)+
          upper^11*((4900*x^8)/8019+(19600*x^5)/729+(137200*x^2)/8019)+
          upper^8*(-(1225*x^8)/486-(9800*x^5)/243-(13475*x^2)/729)+
          upper^2*(-(2450*x^8)/729-(4900*x^5)/729-(2450*x^2)/729)+
          upper^9*((19600*x^7)/2187+(93100*x^4)/2187+(49000*x)/6561)+
          upper^3*((19600*x^7)/2187+(24500*x^4)/2187+(4900*x)/2187)+
          upper^12*(-(4900*x^7)/2187-(57575*x^4)/2187-(12250*x)/2187)+
          upper^6*(-(9800*x^7)/729-(71050*x^4)/2187-(12250*x)/2187)+
          upper^13*((137200*x^6)/28431+(39200*x^3)/2187+24500/28431)+
          upper^14*(-(4900*x^5)/729-(5950*x^2)/729)+
          upper^15*((13720*x^4)/2187+(4900*x)/2187)+
          upper^16*(-(8575*x^3)/2187-1225/4374)+
          (19600*upper^17*x^2)/12393-
          (2450*upper^18*x)/6561 + (4900 * upper^19)/124659

        return(ret3)
      }

      fixed4 = function(x, upper){

        ret4 = abs(upper)^4*((1225*abs(x)^9)/2187+(30625*abs(x)^6)/2187-(20825*abs(x)^3)/2187)+
          abs(upper)^10*((490*abs(x)^9)/6561+(40670*abs(x)^6)/2187+(12250*abs(x)^3)/2187)+
          abs(upper)^7*(-(700*abs(x)^9)/2187-(700*abs(x)^6)/27+(20300*abs(x)^3)/2187-700/2187)+
          abs(upper)*(-(4900*abs(x)^9)/6561+(4900*abs(x)^6)/2187-(4900*abs(x)^3)/2187+4900/6561)+
          abs(upper)^2*((2450*abs(x)^8)/729-(4900*abs(x)^5)/729+(2450*abs(x)^2)/729)+
          abs(upper)^8*((1225*abs(x)^8)/486+(2450*abs(x)^5)/81-(4900*abs(x)^2)/729)+
          abs(upper)^11*(-(4900*abs(x)^8)/8019-(196000*abs(x)^5)/8019+(9800*abs(x)^2)/8019)+
          abs(upper)^5*(-(980*abs(x)^8)/243-(7840*abs(x)^5)/729+(1960*abs(x)^2)/729)+
          abs(upper)^6*((9800*abs(x)^7)/729-(2450*abs(x)^4)/2187+(2450*abs(x))/2187)+
          abs(upper)^12*((4900*abs(x)^7)/2187+(45325*abs(x)^4)/2187-(2450*abs(x))/2187)+
          abs(upper)^3*(-(19600*abs(x)^7)/2187+(24500*abs(x)^4)/2187-(4900*abs(x))/2187)+
          abs(upper)^9*(-(19600*abs(x)^7)/2187-(4900*abs(x)^4)/243+(9800*abs(x))/6561)+
          abs(upper)^13*(-(137200*abs(x)^6)/28431-(313600*abs(x)^3)/28431+4900/28431)+
          abs(upper)^14*((4900*abs(x)^5)/729+(2450*abs(x)^2)/729)+
          abs(upper)^15*(-(13720*abs(x)^4)/2187-(980*abs(x))/2187)+
          (8575*abs(upper)^16*abs(x)^3)/2187-(19600*abs(upper)^17*abs(x)^2)/12393+
          (2450*abs(upper)^18*abs(x))/6561-(4900*abs(upper)^19)/124659

        return(ret4)
      }

      fixed5 = function(x){

        ret5 = (70 / 81)^2 * (6561 / 6916 - (19683 * abs(x)^2) / 13090 + (9 * abs(x)^4) / 5 -
                                (729 * abs(x)^6) / 182 +  (747 * abs(x)^7) / 140 -
                                (729 * abs(x)^8) / 220 + (81 * abs(x)^9) / 70 - (31 * abs(x)^10) / 140 +
                                (111 * abs(x)^13) / 20020 - (3 * abs(x)^16) / 40040 +
                                abs(x)^19 / 461890)
        return(ret5)

      }

      fixed6 = function(x){

        ret6 = 22400/20007 - (15680 * abs(x))/6561 + (7840 * x^2)/1683 - (2800 * abs(x)^3)/351 + (
          21560 * x^4)/2187 - (980 * abs(x)^5)/99 + (2870*  x^6)/351 - (
            11305 * abs(x)^7)/2187 + (245 * x^8)/99 - (70 * abs(x)^9)/81 + (1085 * x^10)/6561 - (
              665 * abs(x)^13)/312741 + (35 * x^16)/625482 - (245 * abs(x)^19)/303046029

        return(ret6)
      }

      ret = numeric(length(x))
      for (i in seq_along(x)){
        if (x[i] >= 0 & x[i] <= 1) {
          if (upper[i] == Inf | upper[i] > 1) {
            ret[i] = fixed5(x = x[i])
          }  else if (upper[i] > (x[i] - 1) & upper[i] <= 0) {
            ret[i] = ((245 * x[i]^19) / 303046029 + (35 * x[i]^13) / 34749 + (35 * x[i]^9) / 81 -
                        (245 * x[i]^8) / 198 + (3605 * x[i]^7) / 2187 - (175 * x[i]^6) / 117 +
                        (2695 * x[i]^4) / 2187 - (105 * x[i]^2) / 187 - (2450 * x[i]) / 6561 + 175 / 494) + fixed1(x = x[i], upper = upper[i])
          } else if (upper[i] >= 0 & upper[i] <= x[i]) {
            ret[i] = (245*x[i]^19)/303046029+(35*x[i]^13)/34749+(35*x[i]^9)/81+
              -(245*x[i]^8)/198 +(3605*x[i]^7)/2187 -(175*x[i]^6)/117 +(2695*x[i]^4)/2187 -(105*x[i]^2)/187 -(2450*x[i])/6561 +175/494 +
              fixed2(x = x[i], upper = upper[i])
          } else if (upper[i] >= x[i] & upper[i] <= 1) {
            ret[i] = 175 / 494  - (2450 * x[i]) / 6561 - (105 * x[i]^2) / 187  + (245 * x[i]^4) / 2187 - (
              175 * x[i]^6) / 117 + (3815 * x[i]^7) / 2187 - (245 * x[i]^8) / 198 + (35 * x[i]^9) / 81 - (
                1085 * x[i]^10) / 6561 + (1295 * x[i]^13) / 312741 - (35 * x[i]^16) / 625482 + (
                  245 * x[i]^19) / 101015343 + fixed3(upper = upper[i], x = x[i])
          } else {ret[i] = 0}
        } else if (x[i] >= 1 & x[i] < 2) {
          if (upper[i] == Inf | upper[i] > 1) {
            ret[i] = fixed6(x = x[i])
          } else if (upper[i] > (x[i] - 1) & upper[i] <= 1) {
            ret[i] = 11200/20007 - (3430 * x[i])/2187 +  + (3920 * x[i]^2)/1683  - (
              1400 * x[i]^3)/351 + (12005 * x[i]^4)/2187 - (
                490 * x[i]^5)/99 + (1435 * x[i]^6)/351 - (2135 * x[i]^7)/729 + (245 * x[i]^8)/198 - (
                  35 * x[i]^9)/81 + (1085 * x[i]^10)/6561 - (665 * x[i]^13)/312741 + (
                    35 * x[i]^16)/625482 - (245 * x[i]^19)/303046029 + fixed4(upper = upper[i], x= x[i])
          } else {
            ret[i] = 0
          }
        } else if (x[i] >= -2 & x[i] <= -1) {
          if (upper[i] > -1 & upper[i] <= x[i] + 1) {
            ret[i] = 11200/20007 + (5390 * x[i])/6561 + (3920 * x[i]^2)/1683  + (
              1400 * x[i]^3)/351 + (3185 * x[i]^4)/729 + (
                490 * x[i]^5)/99 + (1435 * x[i]^6)/351 + (4900 * x[i]^7)/2187 + (245 * x[i]^8)/198 + (
                  35 * x[i]^9)/81 - fixed4(upper = upper[i], x = x[i])
          } else if (upper[i] == Inf | upper[i] > x[i] + 1) {
            ret[i] = fixed6(x = x[i])
          } else {ret[i] = 0}
        } else if (x[i] >= -1 & x[i] <= 0) {
          if (upper[i] > -1 & upper[i] <= x[i]) {
            ret[i] =(-(35*x[i]^9)/81 -(245*x[i]^8)/198 -(4900*x[i]^7)/2187 -(175*x[i]^6)/117 +
                       (2695*x[i]^4)/2187 - (105*x[i]^2)/187 -(2450*x[i])/6561 + 175/494) +
              fixed1(x = x[i], upper = upper[i])
          } else if (upper[i] >= x[i] & upper[i] <= 0) {
            ret[i] = -(490*x[i]^19)/303046029-(35*x[i]^16)/625482-(980*x[i]^13)/312741-
              (1085*x[i]^10)/6561-(35*x[i]^9)/81 -(245*x[i]^8)/198 -(5110*x[i]^7)/2187-(175*x[i]^6)/117  +
              (245*x[i]^4)/2187 -(105*x[i]^2)/187 - (2450*x[i])/6561 +175/494 - fixed2(upper = upper[i], x = x[i])
          } else if (upper[i] >= 0 & upper[i] <= x[i] + 1) {
            ret[i] = -(490*x[i]^19)/303046029 - (35 * x[i]^16) / 625482 -
              (980 * x[i]^13) / 312741 - (1085 * x[i]^10) / 6561 - (35 * x[i]^9) / 81 - (245 * x[i]^8) / 198 -
              (5110 * x[i]^7) / 2187 - (175 * x[i]^6) / 117 + (245 * x[i]^4) / 2187 -
              (105 * x[i]^2) / 187 - (2450 * x[i]) / 6561 + 175 / 494 + fixed3(x= x[i], upper = upper[i])
          } else if (upper[i] == Inf | upper[i] > x[i] + 1) {
            ret[i] = fixed5(x = x[i])
          } else {ret[i] = 0}

        } else if (abs(x[i] == 2)) {ret[i] = 0}
      }
      return(ret)
    },

    #' @description
    #' The squared 2-norm of the cdf is defined by
    #' \deqn{\int_a^b (F_X(u))^2 du}
    #' where X is the Distribution, \eqn{F_X} is its pdf and \eqn{a, b}
    #' are the distribution support limits.
    cdfSquared2Norm = function(x = 0, upper = Inf) {

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

.distr6$kernels <- rbind(
  .distr6$kernels,
  data.table::data.table(
    ShortName = "Tric", ClassName = "Tricube",
    Support = "[-1,1]", Packages = "-"
  )
)
