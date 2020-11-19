test_that("basic solver internals work", {
  s <- new_solver()
  expect_equal(run_count(s), 0)
  s$add_run()
  expect_equal(run_count(s), 1)
  run(s)
  expect_equal(run_count(s), 2)
})
