#' @title Isotonic Regression with isotonic constraints represented by arbitrary directed acyclic graph
#'
#' @description Isotonic Regression with isotonic constraints represented by arbitrary directed acyclic graph
#'
#' @author Xiwen Wang, Daniel P. Palomar
#'
#' @references
#' Wang, X., Ying, J., Cardoso, J. V. D. M., & Palomar, D. P. (2022, June).
#' Efficient Algorithms for General Isotone Optimization.
#' In Proceedings of the \emph{AAAI Conference on Artificial Intelligence}
#' (Vol. 36, No. 8, pp. 8575-8583).
#' @param y regression vector
#' @param w weights (default is NULL)
#'
#' @return A list containing the following elements:
#' \item{\code{x_ordered}}{Optimal solution that satisfies the .}
#'
#' @examples
#' library(IsotoneOptimization)
#' Emat <- t(matrix( c(1,2,  1,3,  1,4,  2,5,  2,6,  3,7,  4,8,  5,8,  6,9,  6,10),2,10))
#' Ordered_DAG(rnorm(10), rep(1,10), Emat)
#'
#' @import Rcpp
#' @useDynLib IsotoneOptimization
#' @importFrom Rcpp sourceCpp
#' @export
Ordered_DAG <- function(y, w=rep(1,length(y)), Emat) {
  # error control
  if (ncol(Emat) != 2) {
    stop("The matrix Emat should be m * 2.")
  }

  if (max(Emat) > length(y)) {
    stop("The matrix Emat contains invalid edges.")
  }

  if(min(Emat) < 1) {
    stop("The index of first node should be 1 instead of 0.")
  }

  if(!is_dag(graph_from_edgelist(Emat))) {
    stop("The matrix Emat should represent an acyclic graph.")
  }

  if (!is.null(w)) {
    if(length(y)!=length(w)) {
      stop("Length of ", dQuote("y"), " should be equal to length of ", dQuote("w"), ".")
    }
  }
  return(solve_ordered_arbitrary_DAG(y, w, Emat-1))
}


