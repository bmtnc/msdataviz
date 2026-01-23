#' Clean Old Log Files
#'
#' Removes log files older than retention period.
#'
#' @param log_dir Directory containing log files
#' @param retention_days Number of days to retain
#'
#' @return Invisible NULL
#' @export
#' @keywords internal
clean_old_logs <- function(log_dir, retention_days) {
  log_files <- list.files(log_dir, pattern = "\\.log$", full.names = TRUE)

  if (length(log_files) == 0) return(invisible(NULL))

  file_info <- file.info(log_files)
  cutoff_time <- Sys.time() - (retention_days * 24 * 60 * 60)
  old_files <- log_files[file_info$mtime < cutoff_time]

  if (length(old_files) > 0) {
    file.remove(old_files)
  }

  invisible(NULL)
}
