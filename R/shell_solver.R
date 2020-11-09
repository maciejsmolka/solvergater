#' Gateway to solver run from command line
#'
#' Gateway to simple external solver that is run from the command line
#' and stores quantity of interest and its jacobian (with respect to problem
#' parameters in output text files.
#'
#' @param cmd command to run the solver executable.
#' @param nparams numeric, number of parameters, must be provided.
#' @param qoi_file name of file containing the computed quantity of interest, can
#' be absolute or relative to `wd`, cannot be `NULL`.
#' @param jacobian_file name of file containing the computed Jacobian matrix
#' of quantity of interest,
#' can be absolute or relative to `wd`, `NULL` indicates that the solver does not
#' provide gradient info.
#' @param qoi_read_fn function reading the quantity of interest from file.
#' @param jacobian_read_fn function reading the Jacobian matrix from file.
#' @param arg_combine_fn function producing appropriate length-1 character vector
#' from point and precision for solver command line
#' @param wd character, working directory, i.e. directory where actual solver exec
#' has to be run, when `NULL` current working directory will be used
#' @param ignore.stdout logical, should solver STDOUT be ignored?
#' @param ignore.stderr logical, should solver STDERR be ignored?
#'
#' @return An object of classes `shell_solver` and `solver`
#'
#' @export
#'
#' @examples
#' rscript_path <- file.path(R.home(), "bin", "Rscript")
#' solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
#' nparams <- 2
#' nqoi <- 5
#' solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
#' s <- shell_solver(solver_cmd, nparams = nparams, qoi_file = "output_qoi",
#' jacobian_file = "output_jacobian", wd = tempdir())
#' run(s, c(20, 5), 10)
shell_solver <- function(
  cmd,
  nparams,
  qoi_file,
  jacobian_file = NULL,
  qoi_read_fn = function(file) scan(file, quiet = TRUE),
  jacobian_read_fn = if (!is.null(jacobian_file)) read_matrix(nparams),
  arg_combine_fn = function(x, precision) paste(c(x, precision), collapse = " "),
  wd = NULL,
  ignore.stdout = TRUE,
  ignore.stderr = TRUE
  ) {
  validate_shell_solver(
    new_shell_solver(
      list(
        cmd = cmd,
        qoi_file = qoi_file,
        jacobian_file = jacobian_file,
        read_qoi = qoi_read_fn,
        read_jacobian = jacobian_read_fn,
        combine_args = arg_combine_fn,
        wd = wd,
        ignore.stdout = ignore.stdout,
        ignore.stderr = ignore.stderr
        ),
      nparams = nparams,
      provides_jacobian = !is.null(jacobian_file)
    )
  )
}

new_shell_solver <- function(x, nparams, provides_jacobian) {
  new_solver(x, nparams = nparams, provides_jacobian = provides_jacobian,
             class = "shell_solver")
}

validate_shell_solver <- function(x) {
  nparams <- attr(x, "nparams")
  if (is.null(nparams)) {
    stop("Number of parameters must be provided", call. = FALSE)
  }
  if (is.null(x$cmd)) {
    stop("Command to run solver must be provided", call. = FALSE)
  }
  if (is.null(x$qoi_file)) {
    stop("Quantity-of-interest file name must be provided", call. = FALSE)
  }
  if (!is.null(nparams)) {
    if (!all(!is.na(nparams) & nparams > 0)) {
      stop("Number of parameters must be NULL or positive numeric", call. = FALSE)
    }
  }
  if (!is.null(x$wd) && !dir.exists(x$wd)) {
    stop("Working directory must exist or be explicitly set to NULL", call. = FALSE)
  }
  x
}
