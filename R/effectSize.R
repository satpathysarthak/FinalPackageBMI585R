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
#' effectSize()
effectSize = function(x,g){
  input <- tibble::tibble(x,g)
  int <- input %>% dplyr::group_by(g) %>% dplyr::summarize(mean= mean(x, na.rm = TRUE))
  out <- (int[int$g==0,]$mean - int[int$g==1,]$mean)/sd(input$x)
  return(out)
}
