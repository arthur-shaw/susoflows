#' Unpack single zip file to a folder bearing its name
#'
#' @description
#' Rather than unpack a file to the directory in which the file sits,
#' create a folder with the file's name (minus extension) and
#' unpack its contents there.
#'
#' @param path Character. Full file path of the zip file.
#'
#' @return Side-effect of creating a folder and unpacking the zip file's
#' contents there.
#'
#' @importFrom fs path_dir path_file path_ext_remove
#' @importFrom zip unzip
#'
#' @noRd
unpack_zip_to_dir <- function(path) {

  # decompose path into parent directory and file components
  parent_dir <- fs::path_dir(path)
  file_name <- fs::path_file(path)

  # create a sub-directory path from the file name
  unpack_dir_name <- fs::path_ext_remove(file_name)
  unpack_dir <- fs::path(parent_dir, unpack_dir_name)

  # unzip the file to same-named directory
  zip::unzip(
    zipfile = path,
    exdir = unpack_dir
  )

}

#' Unpack all zip files found in directory to same-named sub-directory
#'
#' @description
#' Applies `unpack_zip_to_dir()` to all zip files in directory
#'
#' @param dir Character. Directory where zip files can be found
#'
#' @return Side-effect of creating a folder for each zip and
#' unpacking its contents there.
#'
#' @importFrom fs dir_ls
#' @importFrom purrr walk
#' @importFrom cli cli_abort
#'
#' @export
unzip_to_dir <- function(dir) {

  # confirm that directory path exists
  if (!fs::dir_exists(dir)) {

    cli::cli_abort(
      message = c(
        "x" = "Directory specified in {.arg dir} does not exist.",
        "i" = "Please revise the directory path and try again."
      )
    )

  }

  # obtain list of zip files
  files <- fs::dir_ls(
    path = dir,
    type = "file",
    regexp = "\\.zip$",
    recurse = FALSE
  )

  # confirm that zip files found
  if (length(files) == 0) {

    cli::cli_abort(
      message = c(
        "x" = "No zip files found in in the directory.",
        "i" = paste0(
          "This function looks for files with {.file .zip} extensions ",
          "in the root of the directory path."
        )
      )
    )

  }

  # unpack all identified zip files
  purrr::walk(
    .x = files,
    .f = ~ unpack_zip_to_dir(.x)
  )

}
