#!/usr/bin/env Rscript --vanilla

# A fake solver that is run from the command line. It accepts command line args
# (non-numeric args are ignored):
#
# * first arg is the number of problem parameters (N)
# * second is the number of quantities of interest (K)
# * following N args are parameter values
# * additional args are silently ignored
#
# Solver in general computes the value and the Jacobian matrix of quantities
# of interest, where
# k-th quantity of interest is sum(x^k) where x is the vector of parameter values.
# The exception is
# point (1000, 1000, ..., 1000) (in any dimension) where solver exits with
# error code -1.

fake_simple <- function(qoi_file = "output_qoi",
                        jacobian_file = "output_jacobian",
                        cmd_args = commandArgs(TRUE)) {
  tryCatch({
    cat("QOI file:", qoi_file, "\n")
    cat("Jacobian file:", jacobian_file, "\n")
    input <- process_args(cmd_args)
    cat("Number of parameters:", input$nparams, "\n")
    cat("Number of quantities of interest (QOI):", input$nqoi, "\n")
    cat("Point:", input$point, "\n")
    y <- calculate_objective(input$point, input$nparams, input$nqoi)
    cat("QOI value(s):", y$qoi, "\n")
    cat("QOI Jacobian:\n")
    print(y$jacobian)
    write(y$qoi, file = qoi_file)
    write_matrix(y$jacobian, file = jacobian_file)
  },
  error = function(e) {
    warning(conditionMessage(e), call. = FALSE)
    quit(status = -1)
  }
  )
}

process_args <- function(cmd_args) {
  numeric_args <- suppressWarnings(as.numeric(cmd_args))
  numeric_args <- numeric_args[!is.na(numeric_args)]
  nparams <- numeric_args[1]
  if (is.na(nparams) || nparams < 1) {
    stop("Number of parameters must be numeric and not less than 1", call. = FALSE)
  }
  nqoi <- numeric_args[2]
  if (is.na(nqoi) || nqoi < 1) {
    stop("Number of QOIs must be numeric and not less than 1", call. = FALSE)
  }
  point <- numeric_args[3:(2 + nparams)]
  if (any(is.na(point))) {
    stop("Point length must equal number of parameters", call. = FALSE)
  }
  list(nparams = nparams, nqoi = nqoi, point = point)
}

calculate_objective <- function(x, nparams, nqoi) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) == nparams)
  if (is_faulty(x)) {
    stop("Solver error", call. = FALSE)
  }
  qoi <- vapply(1:nqoi, power_fn(x), FUN.VALUE = double(1))
  jac <- vapply(1:nqoi, power_grad(x), FUN.VALUE = double(length(x)))
  list(qoi = qoi, jacobian = t(jac))
}

FAULTY_COORD <- 1000

is_faulty <- function(x) {
  x0 <- rep_len(FAULTY_COORD, length(x))
  is.logical(all.equal(x, x0))
}

# Function factories producing vector power functions with gradient
power_fn <- function(x) {
  stopifnot(is.numeric(x))
  function(k) {
    stopifnot(is.numeric(k), k >= 1, k %% 1 == 0)
    sum(x^k)
  }
}

power_grad <- function(x) {
  stopifnot(is.numeric(x))
  function(k) {
    stopifnot(is.numeric(k), k >= 1, k %% 1 == 0)
    k * x^(k - 1)
  }
}

# From utils.R
write_matrix <- function(mat, file) {
  write(t(mat), file = file, ncolumns = ncol(mat))
}

fake_simple()
