
context("Get the example transXchange files")
file_path <- file.path(tempdir(),"uk2gtfs_tests")
dir.create(file_path)
data_path <- file.path(tempdir(),"uk2gtfs_data")
dir.create(data_path)

test_that("test transxchange data is there", {
  expect_true(dl_example_file(data_path, "transxchange"))
  expect_true(file.exists(file.path(data_path, "transxchange.zip")))
})


# Download Data
cal = get_bank_holidays()
naptan = get_naptan()

test_that("test file downloads", {
  expect_true("data.frame" %in% class(cal))
  expect_true("data.frame" %in% class(naptan))
})


test_that("test transxchange2gtfs multicore", {
  transxchange2gtfs(path_in = file.path(data_path,"transxchange.zip"),
                    path_out = file_path,
                    cal = cal,
                    name = "txc_gtfs",
                    naptan = naptan,
                    ncores = 2)
  expect_true(file.exists(file.path(file_path,"txc_gtfs.zip")))

})

test_that("test transxchange2gtfs singlecore", {
  transxchange2gtfs(path_in = file.path(data_path,"transxchange.zip"),
                    path_out = file_path,
                    cal = cal,
                    name = "txc_gtfs2",
                    naptan = naptan,
                    ncores = 1)
  expect_true(file.exists(file.path(file_path,"txc_gtfs2.zip")))

})
