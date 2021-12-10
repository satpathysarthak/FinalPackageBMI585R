#' Positive Predictive Value or Precision
#'
#' Positive Predictive Value, or PPV, is defined as # True Positives / (# True Positives + # False Positives). PPV is also referred to as precision.
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
#' ppv(pred,truth)
ppv = function(pred,truth){
  # function takes 2 vectors as input pred and truth
  # predicted and true values respectively
  # using set of logical operators TP, TN, FN, FP were calculated as follows
  TP = sum(truth==pred & truth == T)
  FP = sum(truth!=pred & truth == F)
  FN = sum(truth!=pred & truth == T)
  TN = sum(truth==pred & truth == F)
  PPV = TP/(TP+FP)
  return(PPV)
}
