# This tests will run without OTP setup.
context("Get the example atoc files")
file_path <- file.path(tempdir(),"gtfs_tests")
dir.create(file_path)

test_that("test atoc data is there", {
  expect_true(file.exists(file.path(.libPaths()[1],"UK2GTFS/extdata/atoc.zip")))

})

context("Test the main atoc function")

test_that("test atoc2gtfs singlecore", {
  gtfs <- atoc2gtfs(path_in = file.path(.libPaths()[1],"UK2GTFS/extdata/atoc.zip"),
            path_out = file_path,
            name = "gtfs_atoc",
            ncores = 1)
  expect_true(class(gtfs) == "list")
  expect_true(length(gtfs) == 7)
})

context("Test import functions")

unzip(file.path(.libPaths()[1],"UK2GTFS/extdata/atoc.zip"), exdir = file_path)

# files used
# "example.mca"  "ttisf585.alf" "ttisf585.flf" "ttisf585.msn"

# files ignored
# "ttisf585.ztr" "ttisf585.dat" "ttisf585.set" "ttisf585.tsi"


test_that("test importALF", {
  r1 <- importALF(file.path(file_path,"ttisf585.alf"))
  expect_true(class(r1) == "data.frame")
})

test_that("test importFLF", {
  r1 <- importFLF(file.path(file_path,"ttisf585.flf"))
  expect_true(class(r1) == "data.frame")
})

test_that("test importMSN", {
  r1 <- importMSN(file.path(file_path,"ttisf585.msn"))
  expect_true(class(r1) == "data.frame")
})

test_that("test importFLF", {
  r1 <- importFLF(file.path(file_path,"ttisf585.flf"))
  expect_true(class(r1) == "data.frame")
})


test_that("test importMCA", {
  r1 <- importMCA(file.path(file_path,"example.mca"),
                  full_import = TRUE,
                  silent = FALSE)
  expect_true(class(r1) == "list")
})

unlink(file_path, recursive = TRUE)
