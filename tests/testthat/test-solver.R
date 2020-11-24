test_that("basic solver internals work", {
  s <- new_solver()
  expect_equal(run_count(s), 0)
  s$add_run()
  expect_equal(run_count(s), 1)
  run(s)
  expect_equal(run_count(s), 2)
  expect_equal(nparams(s), NULL)
  nparams(s) <- 4
  expect_equal(nparams(s), 4)
  expect_error(nparams(s) <- "Non-numeric")
  expect_error(nparams(s) <- TRUE)
})
