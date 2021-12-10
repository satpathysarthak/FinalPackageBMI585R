#'  Adjusted R-squared
#'
#' Adjusted R-squared is the fraction of variance explained by the model, similar to R-suared. It also penalizes for the number of independent variables in the regression model.
#'
#'
#' @export
#'
#' @param truth a (non-empty) numeric vector of true values of the dependent variable in regression.
#' @param pred a (non-empty) numeric vector of predicted values of the dependent variable in regression.
#' @param d number of independent variables in the regression model. Default value is 1
#' @return
#' @examples
#'
adjR2 = function(pred,truth,d=1){
  Residuals = truth - (pred)
  RSS = norm(Residuals, type = "2")^2
  y_bar = mean(truth)
  TSS = norm(truth-y_bar, type = "2")^2
  n = length(truth)
  num = RSS/(n-d-1)
  den = TSS/(n-1)
  return(1-RSS/TSS)
}
