#' @title Isotonic Regression with binary-tree-ordered isotonic constraints
#'
#' @description Isotonic Regression with binary-tree-ordered isotonic constraints
#'
#' @author Xiwen Wang, Daniel P. Palomar
#'
#' @references
#' Wang, X., Ying, J., Cardoso, J. V. D. M., & Palomar, D. P. (2022, June).
#' Efficient Algorithms for General Isotone Optimization.
#' In Proceedings of the \emph{AAAI Conference on Artificial Intelligence}
#' (Vol. 36, No. 8, pp. 8575-8583).
#'
#' @param y data vector
#' @param w weights (default is NULL)
#'
#' @return A list containing the following elements:
#' \item{\code{x_ordered}}{Optimal solution that satisfies the .}
#'
#' @examples
#' library(IsotoneOptimization)
#' set.seed(10)
#' Ordered_Tree(rnorm(10), rep(1,10))
#'
#' @import Rcpp
#' @import igraph
#' @useDynLib IsotoneOptimization
#' @importFrom Rcpp sourceCpp
#' @export
Ordered_Tree <- function(y, w=NULL) {
  # error control
  if (!is.null(w)) {
    if(min(w)<0) {
      stop("The weights w should not be negative.")
    }
    if(length(y)!=length(w)) {
      stop("Length of ", dQuote("y"), " should be equal to length of ", dQuote("w"), ".")
    }
  }
  else {
    w <- rep(1, length(y))
  }
  return(solve_ordered_binary_tree(y,w))
}


#' @title Isotonic Regression with 2d-grid-ordered isotonic constraints
#'
#' @description Isotonic Regression with 2d-grid-ordered isotonic constraints
#'
#' @author Xiwen Wang, Daniel P. Palomar
#'
#' @references
#' Wang, X., Ying, J., Cardoso, J. V. D. M., & Palomar, D. P. (2022, June).
#' Efficient Algorithms for General Isotone Optimization.
#' In Proceedings of the \emph{AAAI Conference on Artificial Intelligence}
#' (Vol. 36, No. 8, pp. 8575-8583).
#'
#' @param y data vector
#' @param w weights (default is NULL)
#'
#' @return A list containing the following elements:
#' \item{\code{x_ordered}}{Optimal solution that satisfies the .}
#'
#' @examples
#' library(IsotoneOptimization)
#' set.seed(10)
#' Ordered_2d_grid(rnorm(10), rep(1,10))
#'
#' @import Rcpp
#' @useDynLib IsotoneOptimization
#' @importFrom Rcpp sourceCpp
#' @export
Ordered_2d_grid <- function(y, w=NULL) {
  # error control
  if (!is.null(w)) {
    if(min(w)<0) {
      stop("The weights w should not be negative.")
    }
    if(length(y)!=length(w)) {
      stop("Length of ", dQuote("y"), " should be equal to length of ", dQuote("w"), ".")
    }
  }
  else {
    w <- rep(1, length(y))
  }
  return(solve_ordered_2d_grid(y,w))
}


#' @title Isotonic Regression with chain-ordered isotonic constraints
#'
#' @description Isotonic Regression with chain-ordered isotonic constraints
#'
#' @author Xiwen Wang, Daniel P. Palomar
#'
#' @references
#' Wang, X., Ying, J., Cardoso, J. V. D. M., & Palomar, D. P. (2022, June).
#' Efficient Algorithms for General Isotone Optimization.
#' In Proceedings of the \emph{AAAI Conference on Artificial Intelligence}
#' (Vol. 36, No. 8, pp. 8575-8583).
#'
#' @param y data vector
#' @param w weights (default is NULL)
#'
#' @return A list containing the following elements:
#' \item{\code{x_ordered}}{Optimal solution that satisfies the .}
#'
#' @examples
#' library(IsotoneOptimization)
#' set.seed(10)
#' Ordered_chain(rnorm(10), rep(1,10))
#'
#' @import Rcpp
#' @useDynLib IsotoneOptimization
#' @importFrom Rcpp sourceCpp
#' @export
Ordered_chain <- function(y, w=NULL) {
  # error control
  if (!is.null(w)) {
    if(min(w)<0) {
      stop("The weights w should not be negative.")
    }
    if(length(y)!=length(w)) {
      stop("Length of ", dQuote("y"), " should be equal to length of ", dQuote("w"), ".")
    }
  }
  else {
    w <- rep(1, length(y))
  }
  return(solve_ordered_chain(y,w))
}
