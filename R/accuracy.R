#' Accuracy
#'
#' Accuracy is defined as (# TP + # TN) / (# All Positives + # All Negatives).
#'
#'
#' @export
#'
#' @param truth a logical vector of true values.
#' @param pred a logical vector of predicted values.
#' @return a numerical value for accuracy
#' @examples
#' truth = c(T,T,T,T,T,F,F,F,F,F)
#' pred = c(T,F,T,T,F,T,T,T,F,F)
#' accuracy(pred,truth)
accuracy = function(pred,truth){
  # function takes 2 vectors as input pred and truth
  # predicted and true values respectively
  # using set of logical operators TP, TN, FN, FP were calculated as follows
  TP = sum(truth==pred & truth == T)
  FP = sum(truth!=pred & truth == F)
  FN = sum(truth!=pred & truth == T)
  TN = sum(truth==pred & truth == F)
  acc = (TP+TN)/(TP+TN+FP+FN)
  return(acc)
}
