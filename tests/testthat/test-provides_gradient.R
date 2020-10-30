test_that("bad solver knows nothing about gradient", {
  expect_error(provides_gradient(NULL))
})

test_that("extsolver_none provides gradient if appropriate", {
 s_grad <-  extsolver_none()
 expect_true(provides_gradient(s_grad))
 s_nograd <- extsolver_none(gradient = NULL)
 expect_true(!provides_gradient(s_nograd))
})

test_that("shell_solver provides gradient if appropriate", {
  s_grad <-  shell_solver("command", value_file = "vf",
                              gradient_file = "grad_file")
  expect_true(provides_gradient(s_grad))
  s_nograd <- shell_solver("command", value_file = "vf")
  expect_true(!provides_gradient(s_nograd))
})
