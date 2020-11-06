test_that("bad solver knows nothing about Jacobian", {
  expect_null(provides_jacobian(NULL))
})

test_that("r_solver provides Jacobian if appropriate", {
 s_grad <-  r_solver()
 expect_true(provides_jacobian(s_grad))
 s_nograd <- r_solver(jacobian = NULL)
 expect_true(!provides_jacobian(s_nograd))
})

test_that("shell_solver provides Jacobian if appropriate", {
  s_grad <-  shell_solver("command", nparams = 1, qoi_file = "vf",
                          jacobian_file = "grad_file")
  expect_true(provides_jacobian(s_grad))
  s_nograd <- shell_solver("command", nparams = 1, qoi_file = "vf")
  expect_true(!provides_jacobian(s_nograd))
})
