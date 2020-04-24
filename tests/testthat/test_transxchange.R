
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

context("Test GTFS manipulation")
gtfs <- gtfs_read(file.path(file_path, "txc_gtfs2.zip"))

test_that("Can read GTFS", {
  expect_true(class(gtfs) == "list")
})

test_that("Can clean GTFS", {
  gtfs2 <- gtfs_clean(gtfs)
  expect_true(class(gtfs2) == "list")
})


# test_that("Can find fast GTFS", {
#   gtfs3 <- gtfs_fast_trips(gtfs, maxspeed = 30)
#   expect_true(class(gtfs3) == "list")
# })


test_that("Can subset GTFS", {
  bounds <- sf::st_polygon(list(rbind(c(-0.148989, 52.123243),
                                  c(-0.148989,52.617931),
                                  c(0.760130, 52.617931),
                                  c(0.760130,52.123243),
                                  c(-0.148989, 52.123243))))
  bounds <- sf::st_as_sfc(list(bounds))
  bounds <- sf::st_as_sf(data.frame(id = 1,
                                    geometry = bounds),
                         crs = 4326)

  gtfs4 <- gtfs_clip(gtfs, bounds)
  expect_true(class(gtfs4) == "list")
})


test_that("Can compress GTFS", {
  gtfs5 <- gtfs_compress(gtfs)
  expect_true(class(gtfs5) == "list")
  expect_true(as.numeric(object.size(gtfs5)) < as.numeric(object.size(gtfs)))
})

