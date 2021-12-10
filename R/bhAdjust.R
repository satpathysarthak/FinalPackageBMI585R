#'  Bonferroni-Holm: multiple comparison adjustment
#'
#' Implementation of Bonferroni-Holm function
#'
#'
#' @export
#'
#' @param p a (non-empty) numeric vector of p-values.
#' @param alpha significance level
#' @return a logical vector of adjusted p-values < alpha.
#' @examples
#' pval <- c(0.0050,0.0025,0.0075,0.0100,0.0125,0.0150,0.0175,0.0200,0.0225,0.0250)
#' bhAdjust(pval) # TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

bhAdjust = function(p, alpha=0.05 ){
  b = sort(p)
  c = vector()
  m = length(b)
  for ( i in 1:m){
    b[i] = b[i]*(m-i+1)
    if(b[i] < alpha){c = append(c,TRUE)}
    else{
      for (j in i:m){c = append(c,FALSE)}
      break;
    }
    #c[order(order(a))]
  }
  return(c[order(order(p))])

}
