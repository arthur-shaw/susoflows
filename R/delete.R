#' Delete everything inside a target directory
#'
#' @description 
#' Delete everything in a target directory:
#' files, sub-directories, and files in those sub-directories.
#'
#' @param dir Character. Path to target directory
#' 
#' @return Side-effect of deleting stale files.
#'
#' @importFrom fs dir_exists dir_ls dir_delete file_delete
#' @importFrom cli cli_abort
#'
#' @export
delete_in_dir <- function(dir) {

  # if folder exists, purge its contents
  if (fs::dir_exists(dir)) {

    # list sub-directories
    directories <- fs::dir_ls(
      path = dir,
      type = "directory",
      recurse = FALSE
    )

    # remove sub-directories (and their contents), if they exist
    fs::dir_delete(directories)

    # remove all files in directory
    files <- fs::dir_ls(
      path = dir,
      type = "file",
      all = TRUE
    )
    fs::file_delete(files)

  } else {

    cli::cli_abort(
      message = c(
        "x" = "Directory specified in {.arg dir} does not exist.",
        "i" = "Please revise the directory path and try again."
      )
    )

  }

}
