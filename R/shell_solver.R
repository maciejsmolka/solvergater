#' Gateway to solver run from command line
#'
#' Gateway to simple external solver that is run from the command line
#' and stores objective value(s) and gradient (or jacobian)
#' in output text files.
#'
#' @param cmd command to run the solver executable
#' @param value_file name of file containing the computed objective value, cannot
#' be `NULL`
#' @param gradient_file name of file containing the computed objective gradient,
#' `NULL` indicates that the solver does not provide gradient info
#' @param value_read_fn function reading the objective value from file
#' @param gradient_read_fn function reading the objective gradient from file
#' @param arg_combine_fn function producing appropriate length-1 character vector from
#' point and precision for solver command line
#' @param ignore.stdout logical, should solver STDOUT be ignored?
#' @param ignore.stderr logical, should solver STDERR be ignored?
#' @param nparams numeric, number of parameters, unspecified if `NULL`
#'
#' @return An object of classes `shell_solver` and `solver`
#'
#' @export
#'
#' @examples
#' rscript_path <- file.path(R.home(), "bin", "Rscript")
#' solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
#' solver_cmd <- paste(rscript_path, solver_path)
#' s <- shell_solver(solver_cmd, value_file = "output_value", gradient_file = "output_gradient")
#' old_wd <- getwd()
#' setwd(tempdir())
#' compute_objective(s, c(20, 5), 10)
#' setwd(old_wd)
shell_solver <- function(
  cmd,
  value_file,
  gradient_file = NULL,
  value_read_fn = function(file) scan(file, quiet = TRUE),
  gradient_read_fn = if (!is.null(gradient_file)) value_read_fn,
  arg_combine_fn = function(x, precision) paste(c(x, precision), collapse = " "),
  ignore.stdout = TRUE,
  ignore.stderr = TRUE,
  nparams = NULL
  ) {
  validate_shell_solver(
    new_shell_solver(
      list(
        cmd = cmd,
        value_file = value_file,
        gradient_file = gradient_file,
        read_value = value_read_fn,
        read_gradient = gradient_read_fn,
        combine_args = arg_combine_fn,
        ignore.stdout = ignore.stdout,
        ignore.stderr = ignore.stderr
        ),
      nparams = nparams,
      provides_gradient = !is.null(gradient_file)
    )
  )
}

new_shell_solver <- function(x, nparams, provides_gradient) {
  new_solver(x, nparams = nparams, provides_gradient = provides_gradient,
             class = "shell_solver")
}

validate_shell_solver <- function(x) {
  nparams <- attr(x, "nparams")
  if (is.null(x$cmd)) {
    stop("Command to run solver must be provided", call. = FALSE)
  }
  if (is.null(x$value_file)) {
    stop("Value file name must be provided", call. = FALSE)
  }
  if (!is.null(nparams)) {
    if (!all(!is.na(nparams) & nparams > 0)) {
      stop("Number of parameters must be NULL or positive numeric", call. = FALSE)
    }
  }
  x
}

read_value <- function(solver) {
  if (!file.exists(solver$value_file)) {
    stop("Value file does not exist: ", solver$value_file, call. = FALSE)
  }
  solver$read_value(solver$value_file)
}

read_gradient <- function(solver) {
  if (!provides_gradient(solver)) {
    return(NA)
  }
  if (!file.exists(solver$gradient_file)) {
    return(NA)
  }
  solver$read_gradient(solver$gradient_file)
}
