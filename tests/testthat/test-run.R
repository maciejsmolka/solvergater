test_that("default implementation works", {
  obj_nosolver <- run(x = c(10, 20))
  obj_solvernull <- run(NULL, x = c(10, 20))
  expect_true(is.na(obj_nosolver$qoi))
  expect_true(is.na(obj_nosolver$jacobian))
  expect_true(is.na(obj_solvernull$qoi))
  expect_true(is.na(obj_solvernull$jacobian))
})
