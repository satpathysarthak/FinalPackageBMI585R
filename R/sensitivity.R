#' Sensitivity
#'
#' Sensitivity(pred,truth) : Sensitivity is defined as # True Positives / # All Positives = # True Positives / (# True Positives + # False Negatives). Sensitivity is also referred to as recall.
#'
#'
#' @export
#'
#' @param truth a logical vector of true values.
#' @param pred a logical vector of predicted values.
#' @return a numerical value for sensitivity
#' @examples
#' truth = c(T,T,T,T,T,F,F,F,F,F)
#' pred = c(T,F,T,T,F,T,T,T,F,F)
#' sensitivity(pred,truth)
sensitivity = function(pred,truth){
  # function takes 2 vectors as input pred and truth
  # predicted and true values respectively
  # using set of logical operators TP, TN, FN, FP were calculated as follows
  TP = sum(truth==pred & truth == T)
  FP = sum(truth!=pred & truth == F)
  FN = sum(truth!=pred & truth == T)
  TN = sum(truth==pred & truth == F)
  sens = TP/(TP+FN) # self explanatory using formula
  return(sens)
}
