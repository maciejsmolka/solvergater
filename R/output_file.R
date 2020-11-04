#' Get solver output file name
#'
#' This accessor function should be used instead of touching the components
#' of `shell_solver` object. It takes into account the working directory.
#'
#' @param solver object of class `shell_solver`
#' @param type character, file type
#'
#' @export
output_file <- function(solver, type = c("value", "gradient")) {
  wd <- solver$wd
  if (is.null(wd)) {
    wd <- getwd()
  }
  outfile <- switch(type,
                    value = solver$value_file,
                    gradient = solver$gradient_file
  )
  file.path(wd, outfile)
}
