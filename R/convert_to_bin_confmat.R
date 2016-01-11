#'@title Convert a confusion matrix in a two-cass confusion matrix.
#'@description ... 
#'@param cm A confsion matrix with. Can by a table or a ConfusionMatrix 
#'from caret.
#'@param pos_class The name or column of the class which makes up the 
#'first class of the new confusion matrix. 
#'All other classes will be merged to one "other" class.   
#'@param newnames Charater vector of length two with the classnames 
#'of the returned two-class confusion matrix.
#'@return A two-class table or ConfusionMatrix depending on the input.
#'@examples 
#' \dontrun{
#' cm <- rbind(c(8, 2, 4),
#'             c(1, 7, 0),
#'             c(1, 1, 6))
#' rownames(cm) <- colnames(cm) <- c("a", "b", "c")
#' cm <- as.table(cm)
#' convert_to_bin_confmat(cm, "c")
#' }
#'@export
convert_to_bin_confmat <- function(cm, pos_class, 
                              newnames=c("Positive", "Negative")) {
  return_ConfusionMatrix <- FALSE
  if (class(cm) == "confusionMatrix"){
    cm <- cm$table; 
    return_ConfusionMatrix <- TRUE
  }  
  TP=cm[pos_class, pos_class]
  FN=sum(cm[, pos_class]) - TP
  FP=sum(cm[pos_class,]) - TP
  TN=sum(cm) -(TP+FN+FP)
  cm_bin <- as.table(matrix(c(TP, FN, FP, TN), 2, 2))
  attr(cm_bin, "dimnames") <- list(Prediction=newnames,
                                   Reference=newnames)
  if (return_ConfusionMatrix)
    cm_bin <- confusionMatrix(cm_bin)
  return(cm_bin)
}
