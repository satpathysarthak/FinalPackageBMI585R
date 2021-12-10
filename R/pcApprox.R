#' PCA Approximation
#'
#' A function that takes a numerical object from scale and reverses the centering/scaling
#' @export
#'
#' @param x a dataframe or tibble of numerical data.
#' @param npc number of principal compnents
#' @return tibble; approximation to the data \code{x} based on \code{npc} PCs
#' @examples
#' data = iris[,1:4]
#' pcApprox(data,3)
pcApprox = function(x,npc){
  xs = scale(x, center = TRUE, scale = TRUE)
  xpca = prcomp(xs)
  xhat = xpca$x[,1:npc] %*% t(xpca$rotation[,1:npc])
  xus = t(apply(xhat, 1, function(r)r*attr(xs,'scaled:scale') + attr(xs, 'scaled:center')))
  return(xus)
}
