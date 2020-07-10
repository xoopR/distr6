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

      ret = numeric(length(x))
  for (i in seq_along(x)){
  if (x[i] >= 0 & x[i] <= 1) {
    if (upper[i] == Inf | upper[i] >= 1) {
      ret[i] = (70 / 81)^2 * (6561 / 6916 - (19683 * x[i]^2) / 13090 + (9 * x[i]^4) / 5 -
               (729 * x[i]^6) / 182 +  (747 * abs(x[i])^7) / 140 -
               (729 * x[i]^8) / 220 + (81 * abs(x[i])^9) / 70 - (31 * x[i]^10) / 140 +
               (111 * abs(x[i])^13) / 20020 - (3 * x[i]^16) / 40040 +
               abs(x[i])^19 / 461890)
    }  else if (upper[i] >= x[i] - 1 & upper[i] <= 0) {
        ret[i] = 6561 / 13832 + upper[i] + (3 * upper[i]^4) / 2 + (15 * upper[i]^7) / 7 +
             2 * upper[i]^(10) + (15 * upper[i]^(13)) / 13 + (3 * upper[i]^16) / 8 + upper[i]^19 / 19 +
             (- 1 / 2 - 3 * upper[i]^3 - (15 * upper[i]^6) / 2 - 10 * upper[i]^9 - (15 * upper[i]^12) / 2
              - 3 * upper[i]^15 - (upper[i]^18) / 2) * x[i] +
             (- (19683) / 26180 + (9 * upper[i]^2) / 2 + (72 * upper[i]^5) / 5 + (99 * upper[i]^8) / 4 +
             (252 * upper[i]^11) / 11 + (153 * upper[i]^14) / 14 + (36 * upper[i]^17) / 17) * x[i]^2 +
             (- 3 * upper[i] - (69 * upper[i]^4) / 4 - 39 * upper[i]^7 - (87 * upper[i]^10) / 2 - 24 * upper[i]^13 -
             (21 * upper[i]^16) / 4) * x[i]^3 +
             (33 / 20 + 15 * upper[i]^3 + (87 * upper[i]^6) / 2 + 57 * upper[i]^9 + (141 * upper[i]^12) / 4 +
                (42 * upper[i]^15) / 5) * x[i]^4 +
             (- 9 * upper[i]^2 - 36 * upper[i]^5  - 54 * upper[i]^8 - 36 * upper[i]^(11) - 9 * upper[i]^14) *x[i]^5 +
              (- 729 / 364 + 3 * upper[i] + (93 * upper[i]^4) / 4 + (261 * upper[i]^7) / 7 + (51 * upper[i]^10) / 2 +
            (84 * upper[i]^13) / 13) * x[i]^6 +
            (309 / 140 - 12 * upper[i]^3 - 18 * upper[i]^6 - 12 * upper[i]^9 - 3 * upper[i]^12) * x[i]^7 +
            (- (729) / 440 + (9 * upper[i]^2) / 2 + (27 * upper[i]^5)/5 + (27 * upper[i]^8) / 8 + (9 * upper[i]^11) / 11) * x[i]^8 +
            (81 / 140 - upper[i] - (3 * upper[i]^4) / 4 - (3 * upper[i]^7) / 7 - (upper[i]^10) / 10) * x[i]^9 +
            (27 * x[i]^13) / 20020 + x[i]^19 / 923780
   } else if (upper[i] >= 0 & upper[i] <= x[i]) {
        ret[i] = 6561 / 13832 + upper[i] - (3 * upper[i]^7) / 7 + (3 * upper[i]^13) / 13 -
          upper[i]^19 / 19 - upper[i] / 2 - 3 * upper[i]^3 * x[i] +
          (3 * upper[i]^6 * x[i]) / 2 + 2 * upper[i]^9 * x[i] - (3 * upper[i]^12 * x[i]) / 2 -
          (3 * upper[i]^15  * x[i]) / 5 + (upper[i]^18 * x[i]) / 2 -
          (19683 * upper[i]^2) / 26180 + (9 * upper[i]^2 * x[i]^2) / 2 +
          (18 * upper[i]^5 * x[i]^2) / 5 - 9 * upper[i]^8 * x[i]^2 +
          (18 * upper[i]^11 * x[i]^2) / 11 + (9 * upper[i]^14 * x[i]^2) / 2 -
          (36 * upper[i]^17 * x[i]^2) / 17 - 3 * upper[i] * x[i]^3 -
          (51 * upper[i]^4 * x[i]^3) / 4 + (87 * upper[i]^7 * x[i]^3) / 7 +
          (15 * upper[i]^10 * x[i]^3) / 2 -
          (192 * upper[i]^13 * x[i]^3) / 13 + (21 * upper[i]^16 * x[i]^3) / 4 +
          (33 * x[i]^4) / 20 + 15 * upper[i]^3 * x[i]^4 -
          (3 * upper[i]^6 * x[i]^4) / 2 - 27 * upper[i]^9 * x[i]^4 +
          (111 * upper[i]^12 * x[i]^4) / 4 - (42 * upper[i]^15 * x[i]^4) / 5 -
          9 * upper[i]^2 * x[i]^5 - (72 * upper[i]^5 * x[i]^5) / 5 +
          (81 * upper[i]^8 * x[i]^5) / 2 - (360 * upper[i]^11 * x[i]^5) / 11 +
          9 * upper[i]^14 * x[i]^5 - (729 * x[i]^6) / 364 + 3 * upper[i] * x[i]^6 +
          (75 * upper[i]^4 * x[i]^6) / 4 - (243 * upper[i]^7 * x[i]^6) / 7 +
          (249 * upper[i]^10 * x[i]^6) / 10 - (84 * upper[i]^13 * x[i]^6) / 13 +
          (309 * x[i]^7) / 140 - 12 * upper[i]^3 * x[i]^7 + 18 * upper[i]^6 * x[i]^7 - 12 * upper[i]^9 * x[i]^7 + 3 * upper[i]^12 * x[i]^7 -
          (729 * x[i]^8) / 440 + (9 * upper[i]^2 * x[i]^8) / 2 - (27 * upper[i]^5 * x[i]^8) / 5 + (27 * upper[i]^8 * x[i]^8) / 8 -
          (9 * upper[i]^11 * x[i]^8) / 11 + (81 * x[i]^9) / 140 - upper[i] * x[i]^9 + (3 * upper[i]^4 * x[i]^9) / 4 -
          (3 * upper[i]^7 * x[i]^9) / 7 + (upper[i]^10 * x[i]^9) / 10 + (27 * x[i]^13) / 20020 + x[i]^19 / 923780
   } else if (upper[i] >= x[i] & upper[i] <= 1) {
      ret[i] = 6561 / 13832 + upper[i] - (3 * upper[i]^4) / 2 + (15 * upper[i]^7) / 7 -
            2 * upper[i]^10 + (15 * upper[i]^13) / 13 - (3 * upper[i]^16) / 8 + upper[i]^19 / 19 +
            (-(1 / 2) + 3 * upper[i]^3 - (15 * upper[i]^6) / 2 + 10 * upper[i]^9 -
            (15 * upper[i]^12) / 2 + 3 * upper[i]^15 - upper[i]^18 / 2) * x[i] +
            (-(19683 / 26180) - (9 * upper[i]^2) / 2 + (72 * upper[i]^5) / 5 -
            (99 * upper[i]^8) / 4 + (252 * upper[i]^11) / 11 - (153 * upper[i]^14) / 14 +
              (36 * upper[i]^17) / 17) * x[i]^2 +
            (3 * upper[i] - (69 * upper[i]^4) / 4 + 39 * upper[i]^7 - (87 * upper[i]^10) / 2 +
               24 * upper[i]^13 - (21 * upper[i]^16)/4) * x[i]^3 +
            (3 / 20 + 15 * upper[i]^3 - (87 * upper[i]^6) / 2 + 57 * upper[i]^9 -
               (141 * upper[i]^12) / 4 + (42 * upper[i]^15)/5) * x[i]^4 +
            (- 9 * upper[i]^2 + 36 * upper[i]^5 - 54 * upper[i]^8 + 36 * upper[i]^11 - 9 * upper[i]^14) * x[i]^5 +
            (- (729 / 364) + 3 * upper[i] - (93 * upper[i]^4) / 4 + (261 * upper[i]^7) / 7 -
               (51 * upper[i]^10) / 2 + (84 * upper[i]^13) / 13) * x[i]^6 +
            (327 / 140 + 12 * upper[i]^3 - 18 * upper[i]^6 + 12 * upper[i]^9 - 3 * upper[i]^12) * x[i]^7 +
            (- (729 / 440) - (9 * upper[i]^2) / 2 + (27 * upper[i]^5) / 5 - (27 * upper[i]^8) / 8 +
               (9 * upper[i]^11) / 11) * x[i]^8 +
            (81 / 140 + upper[i] - (3 * upper[i]^4) / 4 + (3 * upper[i]^7) / 7 - upper[i]^10 / 10) * x[i]^9 -
            (31 * x[i]^10) / 140 + (111 * x[i]^13) / 20020 - (3 * x[i]^16) / 40040 + (3 * x[i]^19) / 923780
   } else {ret[i] = 0}
  } else if (x[i] >= 1 & x[i] <= 2) {
      if (upper[i] >= x[i] - 1 & upper[i] <= 1) {
      ret[i] = 1296 / 1729 + upper[i] - (3 * upper[i]^7) / 7 + (3 * upper[i]^13) / 13 - upper[i]^19 / 19 +
              (- (21 / 10) - 3 * upper[i]^3 + (3 * upper[i]^6) / 2 + 2 * upper[i]^9 -
              (3 * upper[i]^12) / 2 - (3 * upper[i]^15) / 5 + upper[i]^18 / 2) * x[i] +
              (2916 / 935 + (9 * upper[i]^2) / 2 + (18 * upper[i]^5) / 5 - 9 * upper[i]^8 +
              (18 * upper[i]^11) / 11 + (9 * upper[i]^14) / 2 - (36 * upper[i]^17) / 17) * x[i]^2 +
              (- (486 / 91) - 3 * upper[i] - (51 * upper[i]^4) / 4 + (87 * upper[i]^7) / 7 +
              (15 * upper[i]^10) / 2 - (192 * upper[i]^13) / 13 + (21 * upper[i]^16) / 4) * x[i]^3 +
              (147 / 20 + 15 * upper[i]^3 - (3 * upper[i]^6) / 2 - 27 * upper[i]^9 + (111 * upper[i]^12) / 4 -
              (42 * upper[i]^15) / 5) * x[i]^4 +
              (- (729 / 110) - 9 * upper[i]^2 - (72 * upper[i]^5) / 5 + (81 * upper[i]^8) / 2 -
              (360 * upper[i]^11) / 11 + 9 * upper[i]^14) * x[i]^5 +
              (9963 / 1820 + 3 * upper[i] + (75 * upper[i]^4) / 4 - (243 * upper[i]^7) / 7 +
              (249 * upper[i]^10) / 10 - (84 * upper[i]^13) / 13) * x[i]^6 +
              (- (549 / 140) - 12 * upper[i]^3 + 18 * upper[i]^6 - 12 * upper[i]^9 + 3 * upper[i]^12) * x[i]^7 +
              (729 / 440 + (9 * upper[i]^2) / 2 - (27 * upper[i]^5) / 5 + (27 * upper[i]^8) / 8 - (9 * upper[i]^11) / 11) * x[i]^8 +
              (- (81 / 140) - upper[i] + (3 * upper[i]^4) / 4 - (3 * upper[i]^7) / 7 + upper[i]^10 / 10) * x[i]^9 +
              (31 * x[i]^10) / 140 - (57 * x[i]^13) / 20020 + (3 * x[i]^16) / 40040 - x[i]^19 / 923780
      } else if (upper[i] == Inf | upper[i] >= 1) {
        ret[i] = (70 / 81)^2 * (2592 / 1729 - (16 * abs(x[i])) / 5 + (5832 * abs(x[i])^2) /
                  935 - (972 * abs(x[i])^3) / 91 + (66 * abs(x[i])^4) / 5 -
                  (729 * abs(x[i])^5) / 55 + (9963 * abs(x[i])^6) / 910 -
                  (969 * abs(x[i])^7) / 140 + (729 * abs(x[i])^8) / 220 -
                  (81 * abs(x[i])^9) / 70 + (31 * abs(x[i])^10) / 140 -
                  (57 * abs(x[i])^13) / 20020 + (3 * abs(x[i])^16) /
                  40040 - abs(x[i])^19 / 923780)
      } else {ret[i] = 0}
    } else if (x[i] >= -2 & x[i] <= -1) {
      if (upper[i] >= -1 & upper[i] <= x[i] + 1) {
       ret[i] = 1296 / 1729 + upper[i] - (3 * upper[i]^7) / 7 + (3 * upper[i]^13) / 13 - upper[i]^19 / 19 +
                  ((11 / 10) + 3 * upper[i]^3 + (3 * upper[i]^6) / 2 - 2 * upper[i]^9 - (3 * upper[i]^12) / 2 +
                  (3 * upper[i]^15) / 5 + upper[i]^18 / 2) * x[i] +
                  (2916 / 935 - (9 * upper[i]^2) / 2 + (18 * upper[i]^5) / 5 + 9 * upper[i]^8 + (18 * upper[i]^11) / 11 -
                  (9 * upper[i]^14) / 2 - (36 * upper[i]^17) /17) * x[i]^2 +
                  ((486/91) + 3 * upper[i] - (51 * upper[i]^4) / 4 - (87 * upper[i]^7) / 7 + (15 * upper[i]^10) / 2 +
                  (192 * upper[i]^13) / 13 + (21 * upper[i]^16) / 4) * x[i]^3 +
                  ((117 / 20) + 15 * upper[i]^3 + (3 * upper[i]^6) / 2 - 27 * upper[i]^9 - (111 * upper[i]^12) / 4 -
                  (42 * upper[i]^15) / 5) * x[i]^4 +
                  ((729 / 110) - 9 * upper[i]^2 + (72 * upper[i]^5) / 5 + (81 * upper[i]^8) / 2 + (360 * upper[i]^11) / 11 +
                  9 * upper[i]^14) * x[i]^5 +
                  ((9963 / 1820) + 3 * upper[i] - (75 * upper[i]^4) / 4 - (243 * upper[i]^7) / 7 -
                  (249 * upper[i]^10) / 10 - (84 * upper[i]^13) / 13) * x[i]^6 +
                  (3 + 12 * upper[i]^3 + 18 * upper[i]^6 + 12 * upper[i]^9 + 3 * upper[i]^12) * x[i]^7 +
                  (729 / 440 - (9 * upper[i]^2) / 2 - (27* upper[i]^5) / 5 - (27 * upper[i]^8) / 8 -
                  (9 * upper[i]^11)  /11) * x[i]^8 +
                  ((81 / 140) + upper[i] + (3 * upper[i]^4) / 4 + (3 * upper[i]^7) / 7 + upper[i]^10 / 10) * x[i]^9
      } else if (upper[i] == Inf | upper[i] >= x[i] + 1) {
          ret[i] = (70 / 81)^2 * (2592 / 1729 - (16 * abs(x[i])) / 5 + (5832 * abs(x[i])^2) /
                    935 - (972 * abs(x[i])^3) / 91 + (66 * abs(x[i])^4) / 5 -
                    (729 * abs(x[i])^5) / 55 + (9963 * abs(x[i])^6) / 910 -
                    (969 * abs(x[i])^7) / 140 + (729 * abs(x[i])^8) / 220 -
                    (81 * abs(x[i])^9) / 70 + (31 * abs(x[i])^10) / 140 -
                    (57 * abs(x[i])^13) / 20020 + (3 * abs(x[i])^16) /
                    40040 - abs(x[i])^19 / 923780)
      } else {ret[i] = 0}
    } else if (x[i] >= -1 & x[i] <= 0) {
      if (upper[i] >= -1 & upper[i] <= x[i]) {
         ret[i] = 6561 / 13832 + upper[i] + (3 * upper[i]^4) / 2 + (15 * upper[i]^7) / 7 +
          2 * upper[i]^10 + (15 * upper[i]^13) / 13 + (3 * upper[i]^16) / 8 + upper[i]^19 / 19 +
          (- (1/2) - 3 *upper[i]^3 - (15 * upper[i]^6) / 2 - 10 * upper[i]^9 - (15 * upper[i]^12) / 2 -
             3 * upper[i]^15 - upper[i]^18 / 2) * x[i] +
          (- (19683 / 26180) + (9 * upper[i]^2) / 2 + (72 * upper[i]^5) / 5 + (99 * upper[i]^8) / 4 +
             (252 * upper[i]^11) / 11 + (153 * upper[i]^14) / 14 + (36 * upper[i]^17) / 17) * x[i]^2 +
          (-3 * upper[i] - (69 * upper[i]^4) / 4 - 39 * upper[i]^7 - (87* upper[i]^10) / 2 -
          24 * upper[i]^13 - (21 * upper[i]^16) / 4) * x[i]^3 +
          (33 / 20 + 15 * upper[i]^3 + (87 * upper[i]^6)/2 + 57 * upper[i]^9 + (141 * upper[i]^12) / 4 +
             (42 * upper[i]^15) / 5) * x[i]^4 +
          (- 9 * upper[i]^2 - 36 * upper[i]^5 - 54 * upper[i]^8 - 36 * upper[i]^11 - 9 * upper[i]^14) * x[i]^5 +
          (- (729 / 364) + 3 * upper[i] + (93 * upper[i]^4) / 4 + (261 * upper[i]^7) / 7 + (51 * upper[i]^10) / 2 +
             (84 * upper[i]^13) / 13) * x[i]^6 +
          (-3 - 12 * upper[i]^3 - 18 * upper[i]^6 - 12 * upper[i]^9 - 3 * upper[i]^12) * x[i]^7 +
          (- (729 / 440) + (9 * upper[i]^2) / 2 + (27 * upper[i]^5) / 5 + (27 * upper[i]^8) / 8 +
             (9 * upper[i]^11) / 11) * x[i]^8 +
          (- (81 / 140) - upper[i] - (3 * upper[i]^4) / 4 - (3 * upper[i]^7) / 7 - upper[i]^10 / 10) * x[i]^9
      } else if (upper[i] >= c & upper[i] <= 0) {
          ret[i] = 6561 / 13832 + upper[i] - (3 * upper[i]^7) / 7 + (3 * upper[i]^13) / 13 - upper[i]^19 / 19 +
                  (- (1 / 2) + 3 * upper[i]^3 + (3 * upper[i]^6) / 2 - 2 * upper[i]^9 - (3 * upper[i]^12) / 2 +
                  (3 * upper[i]^15) / 5 + upper[i]^18 / 2) * x[i] +
                  (- (19683 / 26180) - (9 * upper[i]^2) / 2 + (18 * upper[i]^5) / 5 + 9 * upper[i]^8 +
                  (18 * upper[i]^11) / 11 - (9 * upper[i]^14) / 2 - (36 * upper[i]^17) / 17) * x[i]^2 +
                  (3 * upper[i] - (51 * upper[i]^4) / 4 - (87 * upper[i]^7) / 7 + (15 * upper[i]^10) / 2 +
                  (192 * upper[i]^13) / 13 + (21 * upper[i]^16) / 4) * x[i]^3 +
                  (3 / 20 + 15 * upper[i]^3 + (3 * upper[i]^6) / 2 - 27 * upper[i]^9 - (111 * upper[i]^12) / 4 -
                  (42 * upper[i]^15) / 5) * x[i]^4 +
                  (- 9 * upper[i]^2 + (72 * upper[i]^5) / 5 + (81 * upper[i]^8) / 2 + (360 * upper[i]^11) / 11 +
                  9 * upper[i]^14) * x[i]^5 +
                  (- (729 / 364) + 3 * upper[i] - (75 * upper[i]^4) / 4 - (243 * upper[i]^7) / 7 - (249 * upper[i]^10) / 10 -
                  (84 * upper[i]^13) / 13) * x[i]^6 +
                  (- (219 / 70) + 12 * upper[i]^3 + 18 * upper[i]^6 + 12 * upper[i]^9 + 3 * upper[i]^12) * x[i]^7 +
                  (- (729 / 440) - (9 * a^2) / 2 - (27 * upper[i]^5) / 5 - (27 * upper[i]^8) / 8 -
                  (9 * upper[i]^11) / 11) * x[i]^8 +
                  (- (81 / 140) + upper[i] + (3 * upper[i]^4) / 4 + (3 * upper[i]^7) / 7 + upper[i]^10 / 10) * x[i]^9 -
                  (31 * x[i]^10) / 140 - (3 * x[i]^13) / 715 - (3 * x[i]^16) / 40040 - x[i]^19 / 461890
      } else if (upper[i] >= 0 & upper[i] <= x[i] + 1) {
          ret[i] = 6561 / 13832 + upper[i] - (3 * upper[i]^4) / 2 + (15 * upper[i]^7) / 7 -
                     2 * upper[i]^10 + (15 * upper[i]^13)/13 - (3 * upper[i]^16) / 8 + upper[i]^19 / 19 +
                    (- (1 / 2) + 3 * upper[i]^3 - (15 * upper[i]^6) / 2 + 10 * upper[i]^9 -
                    (15 * upper[i]^12) / 2 + 3 * upper[i]^15 - upper[i]^18 / 2) * x[i] +
                    (- (19683 / 26180) - (9 * upper[i]^2) / 2 + (72 * upper[i]^5) / 5 -
                    (99 * upper[i]^8) / 4 + (252 * upper[i]^11) / 11 - (153 * upper[i]^14) / 14 +
                    (36 * upper[i]^17) / 17) * x[i]^2 +
                    (3 * upper[i] - (69 * upper[i]^4) / 4 + 39 * upper[i]^7 - (87 * upper[i]^10) / 2 +
                    24 * upper[i]^13 - (21 * upper[i]^16) / 4) * x[i]^3 +
                    (3 / 20 + 15 * upper[i]^3 - (87 * upper[i]^6) / 2 + 57 * upper[i]^9 -
                    (141 * upper[i]^12) / 4 + (42 * upper[i]^15) / 5) * x[i]^4 +
                    (- 9 * upper[i]^2 + 36 * upper[i]^5 - 54 * upper[i]^8 + 36 * upper[i]^11 -
                    9 * upper[i]^14) * x[i]^5 +
                    (- (729 / 364) + 3 * upper[i] - (93 * upper[i]^4) / 4 + (261 * upper[i]^7) / 7 -
                    (51 * upper[i]^10) / 2 + (84 * upper[i]^13) / 13) * x[i]^6 +
                    (- (219 / 70) + 12 * upper[i]^3 - 18 * upper[i]^6 + 12 * upper[i]^9 -
                    3 * upper[i]^12) * x[i]^7 +
                    (- (729 / 440) - (9 * upper[i]^2) / 2 + (27 * upper[i]^5) / 5 - (27 * upper[i]^8) / 8 +
                    (9 * upper[i]^11) / 11) * x[i]^8 +
                    (- (81 / 140) + upper[i] - (3 * upper[i]^4) / 4 + (3 * upper[i]^7) / 7 -
                    upper[i]^10 / 10) * x[i]^9 -
                    (31 * x[i]^10) / 140 - (3 * x[i]^13) / 715 - (3 * x[i]^16) / 40040 - x[i]^19 / 461890
      } else if (upper[i] == Inf | upper[i] >= x[i] + 1) {
        ret[i] = (70 / 81)^2 * (6561 / 6916 - (19683 * x[i]^2) / 13090 + (9 * x[i]^4) / 5 -
                                      (729 * x[i]^6) / 182 +  (747 * abs(x[i])^7) / 140 -
                                      (729 * x[i]^8) /
                                      220 + (81 * abs(x[i])^9) / 70 - (31 * x[i]^10) / 140 +
                                      (111 * abs(x[i])^13) / 20020 - (3 * x[i]^16) / 40040 +
                                      abs(x[i])^19 / 461890)
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
