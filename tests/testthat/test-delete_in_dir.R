testthat::test_that("deletes files in root of directory path", {

  # create emphemeral file system for testing
  # parent directory
  temp_dir <- withr::local_tempdir()
  # files, and sub-directories
  create_files_and_subdirs(temp_dir)

  # delete everything in directory
  delete_in_dir(temp_dir)

  # confirm files in root of directory were deleted
  testthat::expect_false(fs::file_exists(fs::path(temp_dir, "file1.txt")))
  testthat::expect_false(fs::file_exists(fs::path(temp_dir, "file2.txt")))

})


testthat::test_that("deletes sub-directories in directory path", {

  # create emphemeral file system for testing
  # parent directory
  temp_dir <- withr::local_tempdir()
  # files, and sub-directories
  create_files_and_subdirs(temp_dir)

  # delete everything in directory
  delete_in_dir(temp_dir)

  # confirm files in root of directory were deleted
  testthat::expect_false(fs::dir_exists(fs::path(temp_dir, "dir1")))
  testthat::expect_false(fs::dir_exists(fs::path(temp_dir, "dir2")))

})

testthat::test_that("error if directory doesn't exist", {

  testthat::expect_error(
    delete_in_dir("foo/bar"),
    regexp = "Directory specified"
  )

})
