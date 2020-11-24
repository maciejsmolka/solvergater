test_that("objective() and objective_functions() return coherent results", {
  npars <- 5
  nqoi <- 7
  solver <- fake_simple_solver(npars, nqoi)
  solver_output <- run(solver, rep(1, npars), precision = 50, silent = TRUE)
  exact_data <- solver_output$qoi
  solver_funs <- objective_functions(solver, exact_data, precision = 90,
                                     silent = TRUE)
  solver_obj <- objective(solver, exact_data, precision = 90)
  x <- c(1.3, 7.8, -2.32, 0.1, 1)
  expect_equal(solver_funs$value(x), solver_obj(x)$value)
  expect_equal(solver_funs$gradient(x), solver_obj(x)$gradient)
})

test_that("objective_functions() and make_functions() return coherent results",
          {
  npars <- 5
  nqoi <- 7
  solver <- fake_simple_solver(npars, nqoi)
  solver_output <- run(solver, rep(1, npars), precision = 50, silent = TRUE)
  exact_data <- solver_output$qoi
  of_funs <- objective_functions(solver, exact_data, precision = 90,
                                     silent = TRUE)
  obj <- objective(solver, exact_data, precision = 90, silent = TRUE)
  mf_funs <- make_functions(obj)
  x <- c(1.3, 7.8, -2.32, 0.1, 1)
  expect_equal(of_funs$value(x), mf_funs$value(x))
  expect_equal(of_funs$gradient(x), mf_funs$gradient(x))
})

test_that("objective_functions() does not make unnecessary calls", {
 npars <- 5
 nqoi <- 10
 solver <- fake_simple_solver(npars, nqoi)
 solver_output <- run(solver, rep(1, npars), precision = 100, silent = TRUE)
 exact_data <- solver_output$qoi
 of_funs <- objective_functions(solver, exact_data, precision = 100,
                               silent = TRUE)
 x <- rep(2.5, npars)
 runs_before_of <- run_count(solver)
 of_funs$value(x)
 of_funs$gradient(x)
 runs_after_of <- run_count(solver)
 obj <- objective(solver, exact_data, precision = 100, silent = TRUE)
 mf_funs <- make_functions(obj)
 runs_before_mf <- run_count(solver)
 mf_funs$value(x)
 mf_funs$gradient(x)
 runs_after_mf <- run_count(solver)
 runs_delta_of <- runs_after_of - runs_before_of
 runs_delta_mf <- runs_after_mf - runs_before_mf
 expect_equal(runs_delta_of, 1)
 expect_gt(runs_delta_mf, runs_delta_of)
 expect_equal(runs_delta_mf, runs_delta_of + 1)
})

test_that("objective() handles solver errors appropriately", {
  npars <- 5
  nqoi <- 8
  solver <- fake_simple_solver(nparams = npars, nqoi = nqoi)
  exact_data <- rep.int(0, nqoi)
  obj <- objective(solver, exact_data, precision = 90, silent = TRUE)
  x <- fs_faulty_x(npars)
  expect_warning(result <- obj(x))
  expect_equal(result$value, NA)
  expect_equal(result$gradient, NA)
})
