# This tests will run without OTP setup.
context("Get the example atoc files")
file_path <- file.path(tempdir(),"gtfs_tests")
dir.create(file_path)

test_that("test atoc data is there", {
  expect_true(file.exists(file.path(.libPaths()[1],"UK2GTFS/extdata/atoc.zip")))

})




test_that("test atoc2gtfs singlecore", {
  gtfs <- atoc2gtfs(path_in = file.path(.libPaths()[1],"UK2GTFS/extdata/atoc.zip"),
            path_out = file_path,
            name = "gtfs_atoc",
            ncores = 1)
  expect_true(class(gtfs) == "list")
  expect_true(length(gtfs) == 7)
})





