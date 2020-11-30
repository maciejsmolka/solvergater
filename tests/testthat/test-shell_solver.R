test_that("running external solver works", {
  x <- c(1.2, 3.5, 10.6, 0.1, -2.3)
  nparams <- length(x)
  nqoi <- 3
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
  solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
  s <- shell_solver(
    solver_cmd,
    nparams = length(x),
    qoi_file = "output_qoi",
    jacobian_file = "output_jacobian",
    wd = tempdir()
    )
  expect_silent(obj <- run(s, x, silent = TRUE))
  expect_true(file.exists(output_file(s, "qoi")))
  expect_true(file.exists(output_file(s, "jacobian")))
  expect_length(obj$qoi, nqoi)
  expect_equal(ncol(obj$jacobian), nparams)
  expect_equal(nrow(obj$jacobian), nqoi)
})

test_that("run() handles solver error", {
  nparams <- 10
  err_x <- fs_faulty_x(nparams)
  precision <- 90
  nqoi <- 5
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec",
                           "fake_adaptive.R")
  solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
  s <- shell_solver(
    solver_cmd,
    nparams = nparams,
    qoi_file = "output_qoi",
    jacobian_file = "output_jacobian",
    wd = tempdir()
    )
  expect_warning(obj <- run(s, err_x, precision = precision))
  expect_equal(obj$qoi, NA)
  expect_equal(obj$jacobian, NA)
})

test_that("Invalid parameter values are spotted", {
  expect_error(shell_solver("", nparams = NULL, qoi_file = ""))
  expect_error(shell_solver("", nparams = -2, qoi_file = ""))
  expect_error(shell_solver("", nparams = 2, qoi_file = NULL))
  expect_error(shell_solver(cmd = NULL, nparams = 1, qoi_file = ""))
  solver <- shell_solver("", nparams = 10, qoi_file = "")
  expect_error(run(solver, c(1, 2), 2))
})

test_that("shell_solver handles Jacobian-less solvers", {
  x <- c(1.2, 3.5, 10.6, 0.1, -2.3)
  nparams <- length(x)
  nqoi <- 3
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec",
                           "fake_simple.R")
  solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
  s <- shell_solver(
    solver_cmd,
    nparams = length(x),
    qoi_file = "output_qoi",
    jacobian_file = NULL,
    wd = tempdir()
  )
  obj <- run(s, x, silent = TRUE)
  expect_true(file.exists(output_file(s, "qoi")))
  expect_length(obj$qoi, nqoi)
  expect_equal(obj$jacobian, NA)
})

test_that("Adaptive solver requires precision", {
  s <- fake_adaptive_solver(2, 5)
  expect_error(run(s, c(1, 2)), silent = TRUE)
  expect_silent(run(s, c(1, 2), precision = 90, silent = TRUE))
})
