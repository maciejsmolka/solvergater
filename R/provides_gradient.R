#' Check ability to provide objective gradient
#'
#' @param solver object of class `extsolver`
#'
#' @return logical, TRUE if the solver computes the objective gradient
#'
#' @export
#'
#' @examples
#' s <- extsolver_none()
#' provides_gradient(s)
provides_gradient <- function(solver) {
  UseMethod("provides_gradient")
}

#' @export
provides_gradient.solver <- function(solver) {
  NA
}
