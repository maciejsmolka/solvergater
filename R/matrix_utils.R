#' Read matrix from file
#'
#' A function factory to read matrix with given number of columns from a file.
#' The matrix is read row-wise (with `byrow = TRUE`).
#'
#' @param ncol integer, expected number of columns, passed to [base::matrix()].
#'
#' @return function with 1 argument: `file`
#'
#' @export
#'
#' @examples
#' m <- matrix(1:6, ncol = 3)
#' tf <- tempfile()
#' write(t(m), file = tf, ncolumns = 3)
#' rm3 <- read_matrix(ncol = 3)
#' rm3(tf)
read_matrix <- function(ncol) {
  function(file) {
    matrix(scan(file, quiet = TRUE), ncol = ncol, byrow = TRUE)
  }
}

#' Write a matrix to a file
#'
#' For a matrix with N columns, `write_matrix()` and `read_matrix(N)` are
#' thought to be mutual inverse.
#'
#' @seealso [read_matrix()]
#'
#' @param mat matrix to be written
#' @param file file to write to
#'
#' @export
#'
#' @examples
#' m <- matrix(1:6, ncol = 3)
#' tf <- tempfile()
#' write_matrix(m, tf)
#' rm3 <- read_matrix(3)
#' rm3(tf)
write_matrix <- function(mat, file) {
  write(t(mat), file = file, ncolumns = ncol(mat))
}
