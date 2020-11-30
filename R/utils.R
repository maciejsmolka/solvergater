# Assert that point is not `NULL`
assert_point_not_null <- function(x) {
  if (missing(x) || is.null(x)) {
    stop("Point must be provided", call. = FALSE)
  }
}

# Assert that an arg is not `NULL`
assert_arg_not_null <- function(arg_value, arg_name) {
  if (is.null(arg_value)) {
    stop("Argument value not provided: ", arg_name, call. = FALSE)
  }
}

# Assert that all args from arg_list that have names in arg_names ar not `NULL`
assert_all_args_not_null <- function(arg_names, arg_list) {
  for (arg in arg_names) {
    assert_arg_not_null(arg_list[[arg]], arg)
  }
}

s_message <- function(..., silent = FALSE) {
  if (!silent) {
    message(...)
  }
}
