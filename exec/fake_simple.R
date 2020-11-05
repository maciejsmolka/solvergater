#!/usr/bin/env Rscript --vanilla

# A fake solver that is run from the command line. It accepts command line args:
# all-but-last numeric params are interpreted as the point to compute objective at,
# the last numeric arg is the precision. The solver in general computes the square
# function, i.e. the sum of coordinate squares (and its gradient). The exception is
# the ball around point (1000, 1000, ..., 1000) (in any dimension) with radius
# equal to the given precision. In this area the solver exits with error code -1.

fake_simple <- function(qoi_file = "output_qoi",
                        jacobian_file = "output_jacobian",
                        cmd_args = commandArgs(TRUE)) {
  tryCatch({
    message("QOI file: ", qoi_file)
    message("Jacobian file: ", jacobian_file)
    input <- process_args(cmd_args)
    message("Point: ", toString(input$point))
    message("Precision: ", input$precision)
    y <- calculate_objective(input$point, input$precision)
    message("OQOI value: ", y$qoi)
    message("QOI Jacobian: ", toString(y$jacobian))
    write(y$qoi, file = qoi_file)
    write(y$jacobian, file = jacobian_file)
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
  list(qoi = sum(x^2), jacobian = 2 * x)
}

FAULTY_COORD <- 1000

is_faulty <- function(x, precision) {
  x0 <- rep_len(FAULTY_COORD, length(x))
  all((x - x0)^2 <= precision^2)
}

fake_simple()
