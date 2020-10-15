test_that("default implementation works", {
  obj_nosolver <- compute_objective(x = c(10, 20))
  obj_solvernull <- compute_objective(NULL, x = c(10, 20))
  expect_true(is.na(obj_nosolver$value))
  expect_true(is.na(obj_nosolver$gradient))
  expect_true(is.na(obj_solvernull$value))
  expect_true(is.na(obj_solvernull$gradient))
})
