#' Simple external solver gateway
#'
#' Gateway to simple external solver that stores objective value and gradient
#' in separate files with simple format (real numbers separated with whitespaces).
#'
#' @param cmd command to run the solver executable
#' @param value_file file containing the computed objective value
#' @param gradient_file file containing the computed objective gradient
#'
#' @return An object of classes `extsolver_simple` and `extsolver` with components
#' * `cmd` command to run solver executable
#' * `value_file` path to file containing objective value
#' * `gradient_file` path to file containing objective gradient
#'
#' @export
#'
#' @examples
#' rscript_path <- file.path(R.home(), "bin", "Rscript")
#' solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
#' solver_cmd <- paste(rscript_path, solver_path)
#' s <- extsolver_simple(solver_cmd)
#' old_wd <- getwd()
#' setwd(tempdir())
#' compute_objective(s, c(20, 5), 10)
#' setwd(old_wd)
extsolver_simple <- function(cmd, value_file = "output_value",
                             gradient_file = "output_gradient") {
  structure(
    list(
      cmd = cmd,
      value_file = value_file,
      gradient_file = gradient_file
    ),
    class = c("extsolver_simple", "extsolver")
  )
}

#' @describeIn compute_objective Runs solver executable and reads values from output
#' file(s). If solver process exits with non-zero status code, a warning is issued
#' and list of `NA`'s is returned. In `...` one can pass `ignore.stdout` and
#' `ignore.stderr` that are in turn passed to [base::system()].
#' @export
compute_objective.extsolver_simple <- function(solver, x, precision, ...) {
  cmd <- paste(solver$cmd, paste(x, collapse = " "), precision)
  args <- list(...)
  if (is.null(args$ignore.stdout)) {
    args$ignore.stdout <- FALSE
  }
  if (is.null(args$ignore.stderr)) {
    args$ignore.stderr <- FALSE
  }
  message("Solver command: ", cmd)
  status <- system(cmd, ignore.stdout = args$ignore.stdout,
                   ignore.stderr = args$ignore.stderr)
  if (status != 0) {
    warning("Solver exited with status ", status, call. = FALSE)
    return(list(value = NA, gradient = NA))
  } else {
    message("Solver exited normally")
  }
  if (!file.exists(solver$value_file)) {
    stop("Value file does not exist: ", solver$value_file)
  }
  val <- scan(solver$value_file, quiet = TRUE)
  if (file.exists(solver$gradient_file)) {
    grad <- scan(solver$gradient_file, quiet = TRUE)
  } else {
    grad <- NA
  }
  list(value = val, gradient = grad)
}
