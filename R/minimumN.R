#' Minimum sample size mapped from effect size (Cohen's d)
#'
#' A wrapper function around power.t.test that maps effect size (Cohen's d) to minimum sample size with the default power of 0.9.
#'
#' User's can change the power from 0.8.
#' @export
#'
#' @param d true difference in means
#' @param power power of test (1 minus Type II error probability), default 0.8
#' @return number of observations (per group)

#'
#' @examples
#' minimumN(d = 1)
minimumN = function(d,power=0.8) power.t.test(delta = 1, power = power)$n
