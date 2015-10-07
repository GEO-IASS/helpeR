#' @name combineDf
#' @title Combine data frames by matching specific columns.
#' @description Find matching columns of two data frames and 
#' combine them to one data frame.
#' @param df1 A data frame.
#' @param df2 A data frame with the same columns as df1.
#' @param commonCols The columns that are present in both 
#' data frames and which are used to identify corresponding  
#' rows. If \code{NULL} the intersection of the column names
#' of the two data frames is used. 
#' @param addCols Columns in dfNew to be added to df. If \code{NULL} 
#' all columns but the \code{commonCols} are added.
#' @param addNewRows Add rows from \code{newDf} to \code{df} 
#' even if there were no matches according to \code{commonCols}?
#' @param ... Other atgumets that can be passed to \code{match}.
#' @examples 
#' \dontrun{
#' df = data.frame(a=0:3, b=1:4, c=2:5, d=letters[1:4])
#' dfNew <- cbind((df1+1)[3:1, 3:1], x=11:13, y=12:14)
#' dfComb <- combineDf(dfNew, df)
#' dfComb <- combineDf(dfNew, df, addNewRows=T)
#' }
#' @return For each row in df1 the (first) matching row in 
#' df2, or \code{NA} otherwise.
#'@export
combineDf <- function(dfNew, df, commonCols=NULL, 
                      addCols=NULL, addNewRows=FALSE,
                      suffixNewCols=".new", verbose=TRUE) {
  if (is.null(commonCols)) {
    commonCols <- intersect(colnames(df), colnames(dfNew))
  } else {
    colsRename <- intersect(
      colnames(df)[!colnames(df) %in% commonCols],
      colnames(dfNew)[!colnames(dfNew) %in% commonCols])
    
    if (verbose & length(colsRename) > 0) {
      cat(paste0("Adding suffix \"", suffixNewCols, 
                 "\" to new columns ",
                 "(from \"dfNew\") with names already ",
                 "existing in \"df\" ",
                 "(but not \"commonCols\"):\n",
                 paste0(colsRename, collapse=", ")))
    }
    for (i in seq_along(colnames(dfNew))) {
      cn <- colnames(dfNew)[i]
      colnames(dfNew)[i] <- ifelse(cn %in% colsRename,
                                    paste0(cn, suffixNewCols), cn)
    }
    
  }
  idx.newInDf <- matchDf(dfNew[, commonCols], df[, commonCols])
  idx.na <- is.na(idx.newInDf)
  if (is.null(addCols))
    addCols <- setdiff(colnames(dfNew), commonCols) 
  
  colnames.df.bak <- colnames(df)
  for (cl in addCols)
    df <- cbind(df, NA)
  colnames(df) <- c(colnames.df.bak, addCols)
  df[idx.newInDf[!idx.na], addCols] <- dfNew[!idx.na, addCols]
  
  addNewRows.str <- ""
  if (addNewRows & any(idx.na)) {
    add2df <- dfNew[idx.na, , drop=F]
    # reorder according to df
    ans <- match(colnames(df), colnames(add2df))
    add2df <- add2df[, ans[!is.na(ans)]]
    
    add2df.ext <- df[1, ]
    add2df.ext[1, ] <- NA
    add2df.ext[nrow(add2df), 1] <- NA # expand 
    add2df.ext[, colnames(add2df)] <- add2df
    df <- rbind(df, add2df.ext)
    addNewRows.str <- " and added to df."
  } 
  notInDfNew <- setdiff(1:nrow(df), idx.newInDf[!idx.na])
  if (verbose & length(notInDfNew)>0)
    cat(sprintf(paste0("Rows in df not found in dfNew", 
                       " (in attribute \"notInDfNew\"):\n%s\n"),
                paste0(notInDfNew, collapse=", ")))
  notInDf <- which(idx.na)
  if (verbose & length(notInDf)>0)
    cat(sprintf(paste0("Rows in newDf not found in df%s", 
                       " (in attribute \"notInDf\"):\n%s\n"),
                addNewRows.str, paste0(notInDf, collapse=", ")))
  attr(df, "notInDfNew") <- notInDfNew
  attr(df, "notInDf") <- notInDf
  
  return(df)
}
