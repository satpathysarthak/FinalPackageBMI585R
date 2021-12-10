#' Unscale
#'
#' A function that takes a numerical object from scale and reverses the centering/scaling
#'
#'
#' @export
#'
#' @param x A scaled object with scale.default, the centered, scaled matrix and the numeric centering and scalings (stored as attributes "scaled:center" and "scaled:scale")
#' @return An unscaled numeric vector or matrix
#' @examples
#' set.seed(1)
#' x = matrix(sample(1:12), ncol= 3)
#' xs = scale(x, center = TRUE, scale = TRUE)
#' unscale(xs)
unscale = function(x){
  us = apply(x, 1, function(r)r*attr(x,'scaled:scale') + attr(x, 'scaled:center'))
  if(ncol(x) > 1){
    us = t(us)
  }
  return(us)
}
