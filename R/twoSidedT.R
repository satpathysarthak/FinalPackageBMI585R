#' Area under the t-density
#'
#' Convert a test statistic into the area under the t-densities or P value
#'
#' @export
#'
#' @param t An integer or numeric value for the test-statistic
#' @param n An integer value for degrees of freedom
#' @return a p-value or area under the t-statistic
#' @examples
#' twoSidedT(-.785,14)
twoSidedT = function(t,n){
  t = abs(t)
  return(2 * pt(t, n, lower.tail = FALSE))
}
# twoSidedT(-.785,14)
