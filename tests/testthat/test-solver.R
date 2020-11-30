test_that("basic solver internals work", {
  s <- new_solver()
  expect_equal(run_count(s), 0)
  s$add_run()
  expect_equal(run_count(s), 1)
  run(s, x = 1)
  expect_equal(run_count(s), 2)
  expect_equal(nparams(s), NULL)
  nparams(s) <- 4
  expect_equal(nparams(s), 4)
  expect_error(nparams(s) <- "Non-numeric")
  expect_error(nparams(s) <- TRUE)
})

test_that("Required args are really required", {
  s <- new_solver()
  rs <- new_solver(required_args = c("max_time", "min_time"))
  expect_error(run(s), "Point")
  expect_silent(run(s, x = 1))
  expect_error(run(rs, x = 1))
  expect_error(run(rs, x = 1, max_time = 2))
  expect_silent(run(rs, x = 1, max_time = 2, min_time = -1))
})
