#'@title Find the optimums parameters given it occurs in the limits. 
#'@description Grid-search is often used for parameter selection.
#'An optimization criteria is calculated  for pre-defined 
#'modl parameters (e.g. sigma/gamma and C for the SVM).
#'When the optimum occurs at the limits of the parameters it is 
#'possible that extending the parameter range could improve the 
#'model performance. This functions returns the values of the parameter
#'limits given the optimum occurs at the limits. 
#'@param x A data frame with one column for each parameter and 
#'the optimization criteria.
#'@param params The column name(s) of the parameters. 
#'Optional, if \code{NULL} it is assumed that the parameters are 
#'in the columns \code{1:(ncol(x)-1)} and the optimization criteria 
#'in the column \code{ncol(x)}.   
#'are parameter columns and the last one is the column holding 
#'the values of the optimization criteria.
#'@param optcrit The column name of the optimization criteria.
#'Optiona, if \code{NULL} the last column is used.
#'@return List with the optimums parameters given it occures at 
#'the limits of the parameters.
#'@examples {
#'x <- expand.grid(p1=1:3,
#'                      p2=2:5)
#'x$oc <- rnorm(nrow(x))
#'x[8, 3] <- max(x$oc) + 1 # no limit
#'get_limit_params_given_optimum(x)
#'x[4, 3] <- max(x$oc) + 1 # lower limit of p1
#'get_limit_params_given_optimum(x)
#'x[12, 3] <- max(x$oc) + 1 # upper limits of p1 and p2
#'get_limit_params_given_optimum(x)
#'}
#'@export
get_limit_params_given_optimum <- function(x, params=NULL, optcrit=NULL) {
  
  if (is.null(params) & is.null(optcrit)) {
    params <- colnames(x)[1:(ncol(x)-1)]
    optcrit <- colnames(x)[ncol(x)]
  } else if (!is.null(params) & !is.null(optcrit)) {
    # ...
  } else {
    stop("\"params\" and \"optcrit\" must BOTH be \"NULL\" or character/character vectors.")
  }
  
  u.params <- lapply(params, function(cname) sort(unique(x[, cname])))
  names(u.params) <- params
  mx <- max(x[, optcrit])
  idx.mx <- which(x[, optcrit] == mx)
  
  params.mx <- vector(length(params), mode="list")
  names(params.mx) <- params
  params.mx.limit <- vector(length(params), mode="list")
  names(params.mx.limit) <- params
  for (param in params) {
    params.mx[[param]] <- unique(x[idx.mx, param])
    if (min(params.mx[[param]]) == u.params[[param]][1]) {
      params.mx.limit[[param]] <- u.params[[param]][1]
      attr(params.mx.limit[[param]], "limit") <- "lower"
    }
    if (max(params.mx[[param]]) == u.params[[param]][length(u.params[[param]])]) {
      params.mx.limit[[param]] <- u.params[[param]][length(u.params[[param]])]
      attr(params.mx.limit[[param]], "limit") <- "upper"
    }
  }
  return(params.mx.limit)
}