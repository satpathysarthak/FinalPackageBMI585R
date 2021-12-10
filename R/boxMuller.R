#' Standard normal distribution using Box Muller Transformation
#'
#' Creates samples from the standard normal distribution using Box Muller Transformation
#'
#' @export
#'
#' @param n An integer or number
#' @return a vector of \code{n} samples
#' @examples
#' boxMuller(500)
boxMuller <- function(n){
  out = c()
  for( i in 1:100*n){
    uni <- runif(n*2, min = 0, max = 1)
    u <- sample(uni,2)
    x <- c(sqrt(-2*log(u[1]))*cos(2*pi*u[2]),sqrt(-2*log(u[1]))*sin(2*pi*u[2]))
    out = append(out,x)
  }
  out = sample(out,n)
  return(out)
}
