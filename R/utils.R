# Assert that point is not `NULL`
assert_point_not_null <- function(x) {
  assert_arg_not_null(x, "point")
}

# Assert that precision is not `NULL`
assert_precision_not_null <- function(precision) {
  assert_arg_not_null(precision, "precision")
}

# Assert that an arg is not `NULL`
assert_arg_not_null <- function(arg_value, arg_name) {
  if (is.null(arg_value)) {
    stop("Argument value not provided: ", arg_name)
  }
}

# Run given command in given directory
run_in <- function(cmd, work_dir, ignore.stdout, ignore.stderr) {
  if (!is.null(work_dir)) {
    old_wd <- getwd()
    message("Entering ", work_dir)
    setwd(work_dir)
  }
  status <- system(cmd, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  if (!is.null(work_dir)) {
    message("Exiting ", work_dir)
    setwd(old_wd)
  }
  status
}
