#' Convert a test statistic into the area under the z-density
#'
#' Convert a test statistic into the area under the z-densities or the P value
#'
#' @export
#'
#' @param z An integer or numeric value for the test-statistic
#' @return a p-value or area under the z-statistic
#' @examples
#' twoSidedZ(-.785)
twoSidedZ = function(z){
  z = abs(z)
  return(2*pnorm(z, mean = 0, sd = 1, lower.tail = FALSE))
}
