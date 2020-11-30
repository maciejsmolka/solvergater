#' Gateway for `fake_simple.R` mock solver
#'
#' A helper to create `solver` object encapsulating calls to `fake_simple.R`.
#'
#' @param nparams numeric scalar, number of parameters.
#' @param nqoi numeric scalar, number of quantities of interest.
#' @param wd character, working directory.
#' @param ... additional parameters passed to [shell_solver()].
#'
#' @export
#'
#' @examples
#' s <- fake_simple_solver(3, 4)
#' run(s, c(-1, 5.2, 0))
fake_simple_solver <- function(nparams, nqoi, wd = tempdir(), ...) {
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec",
                           "fake_simple.R")
  solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
  shell_solver(
    solver_cmd,
    nparams = nparams,
    qoi_file = "output_qoi",
    jacobian_file = "output_jacobian",
    wd = wd
    )
}

#' Gateway for `fake_adaptive.R` mock solver
#'
#' A helper to create `solver` object encapsulating calls to `fake_adaptive.R`.
#'
#' @param nparams numeric scalar, number of parameters.
#' @param nqoi numeric scalar, number of quantities of interest.
#' @param wd character, working directory.
#' @param ... additional parameters passed to [shell_solver()].
#'
#' @export
#'
#' @examples
#' s <- fake_adaptive_solver(3, 4)
#' run(s, c(-1, 5.2, 0), precision = 100)
fake_adaptive_solver <- function(nparams, nqoi, wd = tempdir(), ...) {
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec",
                           "fake_adaptive.R")
  solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
  adaptive_shell_solver(
    solver_cmd,
    nparams = nparams,
    qoi_file = "output_qoi",
    jacobian_file = "output_jacobian",
    wd = wd
  )
}

# x causing error in fake_simple.R, n is its length
fs_faulty_x <- function(n) {
  rep.int(1000, n)
}
