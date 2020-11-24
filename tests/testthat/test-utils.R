test_that("Parameter verification works", {
  expect_error(assert_point_not_null(NULL))
  expect_error(assert_precision_not_null(NULL))
})

test_that("s_message() can produce silent messages", {
  expect_silent(s_message("Hello", silent = TRUE))
  expect_message(s_message("Hello"))
})
