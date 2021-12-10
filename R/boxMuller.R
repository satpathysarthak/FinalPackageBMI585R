#' Standard normal distribution using Box Muller Transformation
#'
#' Creates samples from the standard normal distribution using Box Muller Transformation
#'
#' @export
#'
#' @param n An integer or number
#' @return a vector of \code{n} samples
#' @examples
#' set.seed(1223)
#' test = boxMuller(500)
boxMuller <- function(n){
  out = c()
  for( i in 1:(n)){
    uni <- runif(n*2, min = 0, max = 1) # random number from uniform distribution
    u <- sample(uni,2) # sample 2 from the list
    # Box Muller Transformations step
    x <- c(sqrt(-2*log(u[1]))*cos(2*pi*u[2]),sqrt(-2*log(u[1]))*sin(2*pi*u[2]))
    #print(x)
    out = append(out,x)
  }
  # sample n numbers from the pool
  out = sample(out,n)
  return(out)
}
