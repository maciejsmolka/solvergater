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
