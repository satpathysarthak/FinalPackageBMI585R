#'  R-squared
#'
#' R-squared is the fraction of variance explained by the model. It gives you an idea of how many data points fall within the line of the regression equation.
#'
#'
#' @export
#'
#' @param truth a (non-empty) numeric vector of true values of the dependent variable in regression.
#' @param pred a (non-empty) numeric vector of predicted values of the dependent variable in regression.
#' @return
#' @examples
#' model = lm(Sepal.Width ~ Sepal.Length+Petal.Length, data = iris)
#' truth = iris$Sepal.Width
#' pred = model$fitted.values
#' r2(pred,truth)
r2 = function(pred,truth){
  Residuals = truth - (pred)
  RSS = norm(Residuals, type = "2")^2
  y_bar = mean(truth)
  TSS = norm(truth-y_bar, type = "2")^2
  return(1-RSS/TSS)
}
