
#'@title Convert the radial basis function kernel free parameter
#'@description The RBF kernel can be defined with the parameter 
#'\code{gamma} or \code{sigma} where \code{gamma = 1/(2*sigma^2)}.
#'@param gamma Value of gamma
#'@return Value of sigma
#'@export
rbf_gamma2sigma <- function(gamma) {
  sqrt(1/(2*gamma))
}

#'@title Convert the radial basis function kernel free parameter
#'@description The RBF kernel can be defined with the parameter 
#'\code{gamma} or \code{sigma} where \code{gamma = 1/(2*sigma^2)}.
#'@param sigma Value of sigma
#'@return Value of gamma
#'@export
rbf_sigma2gamma <- function(sigma) {
  1/(2*sigma^2)
}