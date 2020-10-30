#' Check ability to provide objective gradient
#'
#' @param solver object of class `solver`
#'
#' @return logical, TRUE if the solver computes the objective gradient
#'
#' @export
#'
#' @examples
#' s <- r_solver()
#' provides_gradient(s)
provides_gradient <- function(solver) {
  UseMethod("provides_gradient")
}

#' @export
provides_gradient.solver <- function(solver) {
  NA
}
