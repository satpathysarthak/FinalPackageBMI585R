#' Effect size
#'
#
#'One very useful measure of effect size is Cohen’s d. This is the difference in means between two groups divided by some measure of the standard deviation. Effect sizes are one of the most common parameters used in power analysis.
#'
#'
#' @export
#'
#' @param x An atomic vector of numeric data
#' @param g An atomic vector of strings or factors (categorical data)
#' @return a numeric value for effect-size or Cohen's d
#' @examples
#' set.seed(24)
#' x = c(rnorm(50,mean=10,sd=2),rnorm(50,mean=3,sd=6))
#' g = c(rep('M',50),rep('F',50))
#' effectSize(x,g)
effectSize = function(x,g){
  input <- tibble::tibble(x,g)
  #g = as.factor(g)
  int <- input %>% dplyr::group_by(g) %>% dplyr::summarize(mean= mean(x, na.rm = TRUE))
  out <- (int[int$g==unique(g)[1],]$mean - int[int$g==unique(g)[2],]$mean)/sd(input$x)
  return(out)
}
