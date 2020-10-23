test_that("running external solver works", {
  x <- c(1.2, 3.5, 10.6, 0.1, -2.3)
  precision <- 10
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
  solver_cmd <- paste(rscript_path, solver_path)
  s <- extsolver_simple(solver_cmd, value_file = "output_value",
                        gradient_file = "output_gradient")
  old_wd <- getwd()
  setwd(tempdir())
  obj <- compute_objective(s, x, precision, ignore.stderr = TRUE)
  expect_true(file.exists(s$value_file))
  expect_true(file.exists(s$gradient_file))
  setwd(old_wd)
  expect_length(obj$value, 1)
  expect_length(obj$gradient, length(x))
})

test_that("compute_objective handles solver error", {
  err_x <- rep(1000, 10) # Value synchronized with fake_simple.R script
  precision <- 10
  rscript_path <- file.path(R.home(), "bin", "Rscript")
  solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
  solver_cmd <- paste(rscript_path, solver_path)
  s <- extsolver_simple(solver_cmd, value_file = "output_value",
                        gradient_file = "output_gradient")
  expect_warning(obj <- compute_objective(s, err_x, precision, ignore.stderr = TRUE))
  expect_true(is.na(obj$value))
  expect_true(is.na(obj$gradient))
})
