#' F1 score
#'
#' F1 score is defined as 2 x (precision x recall) / (precision + recall).
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
#' f1(pred,truth)
f1 = function(pred,truth){
  # function takes 2 vectors as input pred and truth
  # predicted and true values respectively
  # using set of logical operators TP, TN, FN, FP were calculated as follows
  TP = sum(truth==pred & truth == T)
  FP = sum(truth!=pred & truth == F)
  FN = sum(truth!=pred & truth == T)
  TN = sum(truth==pred & truth == F)
  sens = TP/(TP+FN) # self explanatory using formula
  PPV = TP/(TP+FP)
  F1 = 2 * (PPV * sens) / (PPV + sens)
  return(F1)
}
