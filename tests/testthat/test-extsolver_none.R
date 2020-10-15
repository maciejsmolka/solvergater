test_that("extsolver_simple with linear function works", {
  set.seed(1)
  x <- rnorm(10)
  sum_objective <- function(x) sum(x)
  sum_gradient <- function(x) rep(1, length(x))
  solver <- extsolver_none(sum_objective, sum_gradient)
  y <- compute_objective(solver, x, NULL)
  expect_equal(sum_objective(x), y$value)
  expect_equal(sum_gradient(x), y$gradient)
})

test_that("extsolver_simple with square function works", {
  set.seed(1)
  x <- rnorm(10)
  square_objective <- function(x) sum(x^2)
  square_gradient <- function(x) 2 * x
  solver <- extsolver_none(square_objective, square_gradient)
  y <- compute_objective(solver, x, NULL)
  expect_equal(square_objective(x), y$value)
  expect_equal(square_gradient(x), y$gradient)
})
