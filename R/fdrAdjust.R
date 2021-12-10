#'  FDR: multiple comparison adjustment
#'
#' Implementation of FDR function for multiple hypothesis testing
#'
#'
#' @export
#'
#' @param p a (non-empty) numeric vector of p-values.
#' @param alpha significance level
#' @return a logical vector of adjusted p-values < alpha.
#' @examples
#' pval <- c(0.0050,0.0025,0.0075,0.0100,0.0125,0.0150,0.0175,0.0200,0.0225,0.0250)
#' fdrAdjust(pval) # TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
fdrAdjust = function(p, alpha=0.05 ){
  b = sort(p)
  #print(b)
  c = vector()
  m = length(b)
  for ( i in 1:m){b[i] = b[i]*m/i}
  k = ifelse(length(which(b < alpha))>0,max(which(b < alpha)),0)
  #print(k)
  for(i in 1:m){
    if(i<=k){c = append(c,TRUE)}
    else{c = append(c,FALSE)}
  }
  #print(b)
  #print(c)
  return(c[order(order(p))])

}
