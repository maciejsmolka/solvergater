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
