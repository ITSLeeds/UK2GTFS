# This tests will run without OTP setup.
context("Test the TransXChange Functions")


context("Get the example files")
file_path <- file.path(tempdir(),"gtfs_tests")
dir.create(file_path)
#unzip("inst/extdata/transxchange.zip", exdir = file_path)

test_that("test transxchange2gtfs", {
  transxchange2gtfs(path_in = "extdata/transxchange.zip",
                    path_out = file_path)
  expect_true(file.exists(file.path(file_path,"gtfs.zip")))

})