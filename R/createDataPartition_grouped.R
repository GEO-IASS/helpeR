#' Title
#'
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
#' refset = data.frame(class=factor(rep(c(1, 2), c(4, 9))),
#'                     polygon=rep(1:5, c(2, 2, 2, 3, 4)),
#'                     x1=rnorm(13), x2=rnorm(13))
#' 
#' sets <- createDataPartition_grouped(refset, y='class', group='polygon', pTr=.5, seed=0)
#' sets
#' # The following check must be TRUE => pixels from a polygon are not found in both sets
#' length(intersect(sets$train$polygon, sets$test$polygon))==0
createDataPartition_grouped <- function(df, y, group, pTr=.5, seed=NULL) {
  idx_train <- .get_tr_fids_idx(df[, y], df[, group], pTr=pTr, seed=seed)
  return(list(train=df[idx_train, ], test=df[!idx_train, ]))
}
  
  
#' Title
#'
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#' 
#' @examples
.get_tr_fids <-
  function(ids, fids, pTr, seed=NULL) {
    uids <- unique(ids); uids <- sort(uids[!is.na(uids)])
    fids_tr <- lapply(uids, function(id, ids, fids, pTr, seed)
      .get_tr_fids_oneId(id, ids=ids, fids=fids, pTr=pTr, seed=seed),
      ids=ids, fids=fids, pTr=pTr, seed=seed)
    return(fids_tr)
  }
#' Title
#'
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#'
#' @examples
.get_tr_fids_idx <-
  function(ids, fids, pTr, seed=NULL) {
    tr.fids <- .get_tr_fids(ids, fids, pTr, seed)
    len <- sapply(tr.fids, length)
    tr.fids <- unlist(tr.fids)
    idx <- fids %in% tr.fids
    attr(idx, "len") <- len
    return(idx)
  }
#' Title
#'
#' @param id 
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#'
#' @examples
.get_tr_fids_oneId <-
  function(id, ids, fids, pTr, seed=NULL) {
    idx.id <- ids==id
    fids.id <- fids[idx.id]
    ufids <- sort(unique(fids.id))
    if (!is.null(seed))
      set.seed(seed)
    fids.id.tr <- sample(ufids, floor(length(ufids)*pTr))
    return(fids.id.tr)
  }
