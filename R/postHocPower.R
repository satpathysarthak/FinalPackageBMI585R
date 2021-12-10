#'  Post-hoc power
#'
#' Estimation of power based on 1000 simulations from  a measure of Cohen’s d and an n per group.
#'
#'
#' @export
#'
#' @param d a numeric value for Cohen's d (effect size)
#' @param n1 an integer for sample size of group 1
#' @param n2 an integer for sample size of group 2
#' @return
#' @examples
#' set.seed(2021)
#' postHocPower(0.2,1000,1500)    # 0.974
postHocPower <- function(d,n1,n2){
  p.value = vector(length=1000)
  for(i in 1:1000){
    g_1 <- rnorm(n1)
    g_2 <- rnorm(n2, mean = d-mean(g_1))
    t = t.test(g_1,g_2)
    p.value[i] = t$p.value
  }
  power = sum(p.value<0.05)/length(p.value)
  return(power)
}
