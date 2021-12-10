#' chi-square test for count data
#'
#' Implement a chi-squared test that takes a dataset with 2 columns of categorical variable to return the counts matrix and p-value for the chi square test.
#'
#'
#' @export
#'
#' @param tib a dataframe or tibble with 2 columns of interest
#' @return a matrix of counts and p-value of the chi-square test
#' @examples
#' data = tibble::read_csv('https://jlucasmckay.bmi.emory.edu/global/bmi585/demographics.csv')
#' data = subset(data, select = c('sex','group'))
#' chiSquareCounts(data)

chiSquareCounts <- function(tib){
  a = table(tib[[1]],tib[[2]])
  a = as.matrix(a)
  rowsum <- rowSums(a)
  colsum <- colSums(a)
  totsum <- sum(a)
  b = a
  for(i in nrow(b)){
    b[i,] = a[i,]*rowSums(a)[i]/totsum
  }
  a = as.matrix(a)
  b = as.matrix(b)
  print(b)
  est = sum((a-b)^2/b)
  dof = (nrow(a)-1) * (ncol(a)-1)
  return(pchisq(est, df=dof, lower.tail=FALSE))
}
