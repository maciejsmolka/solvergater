#' Assert that point is not `NULL`
#'
#' @param x numeric vector representing point
#'
#' @export
assert_point_not_null <- function(x) {
  assert_arg_not_null(x, "point")
}

#' Assert that precision is not `NULL`
#'
#' @param precision lenght 1 numeric vector representing precision
#'
#' @export
assert_precision_not_null <- function(precision) {
  assert_arg_not_null(precision, "precision")
}

#' Assert that an arg is not `NULL`
#'
#' @param arg_value argument value
#' @param arg_name argument name
#'
#' @export
assert_arg_not_null <- function(arg_value, arg_name) {
  if (is.null(arg_value)) {
    stop("Argument value not provided: ", arg_name)
  }
}
