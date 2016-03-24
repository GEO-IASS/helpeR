#' Convert a two-class confusion matrix in a data frame. 
#'
#' @param x A two-class confusion matrix as returned by 
#' \code{\link{[caret]confusionMatrix}}.
#' @param addCols Add columns to the returned data frame. 
#' \code{as.data.frame(as.list(addCols))} must work and return 
#' a data frame with one row. 
#' @param addColsfromAttr Name of the attribute to be used for 
#' \code{addCols}
#' @param stringsAsFactors ...
#' @param long Return a data frame with the accuracies as 
#' key-value pairs
#' @param key If \code{long} is \code{TRUE} the name for the 
#' column with the accuracy measure names. 
#' @param value If \code{long} is \code{TRUE} the name for the 
#' column with the accuracy values. 
#' 
#' @return \code{data.frame}
#' @export
confmat2df <- function(x, addCols=NULL, addColsfromAttr=NULL, 
                       stringsAsFactors=FALSE, 
                       long=T, key="Measure", value="Accuracy") {
  
    if (!is.null(addColsfromAttr))
      addCols <- attr(x, addColsfromAttr)
    
    addCols <- as.list(addCols)
    
    if (!all(sapply(addCols, length)==1)) {
      print(addCols)
      stop("Need single values in added columns but see above what I got.")
    }
    
    F1=as.numeric(2*((x$byClass["Pos Pred Value"]*
                        x$byClass["Sensitivity"])/
                       (x$byClass["Pos Pred Value"]+
                          x$byClass["Sensitivity"])))
    ans <- list(Sens=x$byClass["Sensitivity"],
                PPV=x$byClass["Pos Pred Value"],
                Spec=x$byClass["Specificity"],
                NPV=x$byClass["Neg Pred Value"],
                Acc=x$overall["Accuracy"],
                F1=F1,
                K=x$overall["Kappa"])
    ans <- c(addCols, ans)
    df <- as.data.frame(ans, stringsAsFactors=stringsAsFactors)
    rownames(df) <- NULL
  if (long) {
    require(tidyr)
    df <- gather(df, key=key, value=value, Sens:K)
    colnames(df)[colnames(df)=="key"] <- key
    colnames(df)[colnames(df)=="value"] <- value
  }
  return(df)
}