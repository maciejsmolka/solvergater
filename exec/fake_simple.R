#!/usr/bin/env Rscript --vanilla

# A fake solver that is run from the command line. It accepts command line args:
# all-but-last numeric params are interpreted as the point to compute objective at,
# the last numeric arg is the precision. The solver in general computes the square
# function, i.e. the sum of coordinate squares (and its gradient). The exception is
# the ball around point (1000, 1000, ..., 1000) (in any dimension) with radius
# equal to the given precision. In this area the solver exits with error code -1.

fake_simple <- function(value_file = "output_value",
                        gradient_file = "output_gradient",
                        cmd_args = commandArgs(TRUE)) {
  tryCatch({
    message("Value file: ", value_file)
    message("Gradient file: ", gradient_file)
    input <- process_args(cmd_args)
    message("Point: ", toString(input$point))
    message("Precision: ", input$precision)
    y <- calculate_objective(input$point, input$precision)
    message("Objective value: ", y$value)
    message("Objective gradient: ", toString(y$gradient))
    write(y$value, file = value_file)
    write(y$gradient, file = gradient_file)
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
  point <- numeric_args[-length(numeric_args)]
  precision <- numeric_args[length(numeric_args)]
  if (length(point) == 0) {
    stop("No point given to compute objective at", call. = FALSE)
  }
  if (length(precision) == 0) {
    stop("Solver precision not given", call. = FALSE)
  }
  if (precision < 0) {
    stop("Precision must be positive", call. = FALSE)
  }
  list(point = point, precision = precision)
}

PRECISION_CONSTANT <- 10

calculate_objective <- function(x, precision) {
  stopifnot(is.numeric(x))
  for (i in seq_len(PRECISION_CONSTANT / precision)) {
    cat(".")
    Sys.sleep(1)
  }
  if (!is.null(i)) {
    cat("\n")
  }
  if (is_faulty(x, precision)) {
    stop("Solver error", call. = FALSE)
  }
  list(value = sum(x^2), gradient = 2 * x)
}

FAULTY_COORD <- 1000

is_faulty <- function(x, precision) {
  x0 <- rep_len(FAULTY_COORD, length(x))
  all((x - x0)^2 <= precision^2)
}

fake_simple()
