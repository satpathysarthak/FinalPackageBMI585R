#' Welch's t-test
#'
#' Performs Welch's t-test for two group dataset
#'
#'
#' @export
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y a (non-empty) numeric vector of data values.
#' @return a numeric vector with p-value, estimated t-statistic and degrees of freedom
#' @examples
#' welchT(1:10, y = c(7:20)) # 0.000019
#' welchT(1:10, y = c(7:20, 200)) # 0.124513
welchT = function(x,y){
  n1 = length(x)
  n2 = length(y)
  mu1 = mean(x)
  mu2 = mean(y)
  s1 = sd(x)
  s2 = sd(y)
  tstat = abs(mu1-mu2)/sqrt((s1^2 / n1)+ (s2^2 / n2) )
  num = ((s1^2 / n1)+ (s2^2 / n2))^2
  den = ((s1^2 / n1)^2)/(n1-1) + ((s2^2 / n2)^2)/(n2-1)
  dof = num/den
  p.val = pt(tstat,dof, lower.tail = F)*2
  out = c('p.value'=round(p.val,6),'estimate'= round(tstat,3),'dof'=round(dof,1))
  return(out)
}

