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
