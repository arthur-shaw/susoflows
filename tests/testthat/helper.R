create_files_and_subdirs <- function(dir) {

  # create files in root
  fs::file_create(fs::path(dir, "file1.txt"))
  fs::file_create(fs::path(dir, "file2.txt"))

  # create directories in root
  dir1 <- fs::dir_create(fs::path(dir, "dir1"))
  dir2 <- fs::dir_create(fs::path(dir, "dir2"))

  # create files in directories
  file_paths_in_dirs <- c(
    fs::path(dir1, c("file1.txt", "file2.txt")),
    fs::path(dir2, c("file1.txt", "file2.txt"))
  )

  fs::file_create(file_paths_in_dirs)

}

create_zip_file <- function(
  dir,
  file_name
) {

  # create files to zip
  paths_files_to_zip <- fs::file_create(
    path = fs::path(
      dir,
      c("file1.txt", "file2.txt")
    )
  )

  # create zip archive of previously created files
  zip::zip(
    zipfile = fs::path(dir, file_name),
    files = paths_files_to_zip,
    mode = "cherry-pick"
  )

  # delete original files
  fs::file_delete(path = paths_files_to_zip)

}

create_zip_files <- function(
  dir,
  file_names
) {

  # create one zip file for each name provided
  purrr::walk(
    .x = file_names,
    .f = ~ create_zip_file(
      dir = dir,
      file_name = .x
    )
  )

}
