#'@title Get the defautl SVM Parameters from ... 
#'@description ...
#'@details 
#' \itemize{ 
#' \item{Hsu: A Practical Guide to SVM (http://www.ee.columbia.edu/~sfchang/course/spr/papers/svm-practical-guide.pdf)}
#' \item{Ben-Hur: A User's guide to SVMs. (http://pyml.sourceforge.net/doc/howto.pdf)}
#' \item{}
#'}
#'@param from 
#'@return List of parameters as suggsted in the references.
#'@export
svm_params_from <- function(from="Hsu") {
  if (from == "Hsu") {
    rtrn <- list(
      C=2^seq(-5, 15, 2),
      gamma=2^seq(-15, 3, 2),
      sigma=sort(rbf_gamma2sigma(2^seq(-15, 3, 2)))
    )
  } else if (from == "Ben-Hur") {
    rtrn <- list(
      C=10^c(-2:5),
      gamma=10^c(-5:1),
      sigma=sort(rbf_gamma2sigma(10^c(-5:1)))
    )
  }
  return(rtrn)
}

