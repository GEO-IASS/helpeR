#'@title Add User's and Producer's Accuracy to a confusion matrix.
#'@description ... 
#'@param cm A confusion matrix (table). 
#'@param as.int If TRUE, accuracies are multiplied with 100 
#'and converted to integers.
#'@return A table with the User's and Producer's Accuracies 
#'added as last column and row.
#'@examples 
#' \dontrun{
#' cm <- rbind(c(8, 2, 4),
#'             c(1, 7, 0),
#'             c(1, 1, 6))
#' rownames(cm) <- colnames(cm) <- c("a", "b", "c")
#' cm <- as.table(cm)
#' add_UAuPA_to_confmat(cm, TRUE)
#' }
#'@export
add_UAuPA_to_confmat <- function(cm, as.int=F) {
  
  cm_out <- cbind(cm, UA=NA)
  cm_out <- rbind(cm_out, PA=NA)
  ua <- (diag(cm) / rowSums(cm))*100
  pa <- (diag(cm) / colSums(cm))*100
  oa <- (sum(diag(cm))/sum(cm)) * 100
  
  if (as.int) {
    ua <- as.integer(round(ua, 0))
    pa <- as.integer(round(pa, 0))
    oa <- as.integer(round(oa, 0))
    }

  cm_out[1:nrow(cm), "UA"] <- ua
  cm_out["PA", 1:nrow(cm)] <- pa
  cm_out["PA", "UA"] <- oa
  
  return(cm_out)
}
