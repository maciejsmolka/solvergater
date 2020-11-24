test_that("Default implementation is appropriately dumb", {
  obj_nosolver <- run(x = c(10, 20))
  obj_solvernull <- run(NULL, x = c(10, 20))
  expect_equal(obj_nosolver$qoi, NA)
  expect_equal(obj_nosolver$jacobian, NA)
  expect_equal(obj_solvernull$qoi, NA)
  expect_equal(obj_solvernull$jacobian, NA)
})
