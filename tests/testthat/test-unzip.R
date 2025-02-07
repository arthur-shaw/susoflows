testthat::test_that("unzips file to same-named subfolder", {

  # create emphemeral file system for testing
  # parent directory
  temp_dir <- withr::local_tempdir()
  # zip files
  create_zip_files(
    dir = temp_dir,
    file_names = c("one.zip", "two.zip")
  )

  # unzip all files to same-named directories
  unzip_to_dir(dir = temp_dir)

  # same-named directories exist
  testthat::expect_true(fs::dir_exists(fs::path(temp_dir, "one")))
  testthat::expect_true(fs::dir_exists(fs::path(temp_dir, "two")))

  # same-named directories contain expected files
  testthat::expect_true(
    all(
      fs::file_exists(
        path = c(
          fs::path(temp_dir, "one", c("file1.txt", "file2.txt")),
          fs::path(temp_dir, "two", c("file1.txt", "file2.txt"))
        )
      )
    )
  )

})

testthat::test_that("error if target directory doesn't exist", {

  testthat::expect_error(
    unzip_to_dir(dir = "foo/bar"),
    regexp = "Directory specified"
  )

})

testthat::test_that("error if no zip found in target directory", {

  # create emphemeral file system for testing
  # parent directory
  temp_dir <- withr::local_tempdir()
  # files and sub-direcctories
  create_files_and_subdirs(dir = temp_dir)

  testthat::expect_error(
    unzip_to_dir(dir = temp_dir),
    regexp = "No zip files"
  )

})
