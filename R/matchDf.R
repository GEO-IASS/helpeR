#' @title matchDf
#' @description Find the positions of (first) matches of the 
#' first argument in the second.
#' @param df1 A data frame.
#' @param df2 A data frame with the same columns as df1.
#' @param ... Other atgumets that can be passed to \code{match}.
#' @examples 
#' \dontrun{
#' df1 = data.frame(a=1:3, b=2:4, c=3:5)
#' df2 <- cbind((df1+1)[3:1, 3:1], x=11:13, y=12:14)
#' matches = matchDf(df1, df2)
#' df2[!is.na(matches), ]
#' }
#' @return For each row in df1 the (first) matching row in 
#' df2, or \code{NA} otherwise.
#' @export 
matchDf <- function(df1, df2, ...) {
  df1p <- do.call("paste", df1)
  df2p <- do.call("paste", df2[, colnames(df1)])
  idx = match(df1p, df2p, ...)
  return(idx)
}
