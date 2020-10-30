test_that("r_solver with linear function works", {
  set.seed(1)
  x <- rnorm(10)
  sum_objective <- function(x) sum(x)
  sum_gradient <- function(x) rep(1, length(x))
  solver <- r_solver(sum_objective, sum_gradient)
  y <- compute_objective(solver, x, NULL)
  expect_equal(sum_objective(x), y$value)
  expect_equal(sum_gradient(x), y$gradient)
})

test_that("r_solver with square function works", {
  set.seed(1)
  x <- rnorm(10)
  square_objective <- function(x) sum(x^2)
  square_gradient <- function(x) 2 * x
  solver <- r_solver(square_objective, square_gradient)
  y <- compute_objective(solver, x, NULL)
  expect_equal(square_objective(x), y$value)
  expect_equal(square_gradient(x), y$gradient)
})
