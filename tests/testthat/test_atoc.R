
context("Running unit tests before system tests")


fixDates <- function( df )
{
  df$start_date <- as.Date(df$start_date, format = "%d-%m-%Y")
  df$end_date <- as.Date(df$end_date, format = "%d-%m-%Y")
  df$duration <- df$end_date - df$start_date + 1

  return (df)
}

test_that("test makeCalendar.inner:1", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",        "uid1",         "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "01-03-2023",  "11-01-2023",  "08-03-2023",   "23-01-2023" ),
                        end_date=c(  "04-02-2023", "05-02-2023",  "31-03-2023",  "19-01-2023",  "09-03-2023",   "23-01-2023" ),
                        Days=c(      "1111110",    "0000001",     "0011100",     "0011000",     "0011000",      "1000000" ),
                        STP=c(       "P",          "P",           "P",           "O",           "C",            "C" ),
                        rowID=c(     1,            2,             3,             4,             5,              6))

  testData <- fixDates( testData )

  res <- makeCalendar.inner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]
  res.calendar_dates <- res.calendar_dates[!is.na(res.calendar_dates)]

  #this is what the code produces - it is wrong.not applying the overlay correctly
  expectedResult = data.table(UID=c( "uid1 a1",    "uid1 b1",    "uid1 a2",     "uid1 b2",     "uid1 a3",     "uid1 b3",     "uid1 a4"),
                        start_date=c("02-01-2023", "24-01-2023", "08-01-2023",  "24-01-2023",  "01-03-2023",  "10-03-2023",  "11-01-2023"),
                        end_date=c(  "22-01-2023", "04-02-2023", "22-01-2023",  "05-02-2023",  "07-03-2023",  "31-03-2023",  "19-01-2023"),
                        Days=c(      "1111110",    "1111110",    "0000001",     "0000001",     "0011100",     "0011100",     "0011000"),
                        STP=c(       "P",          "P",          "P",           "P",           "P",           "P",           "O"),
                        rowID=c(     1,            1,            2,             2,             3,             3,             4))

  expectedResult <- fixDates( expectedResult )

  expect_true(identical(expectedResult,res.calendar) & 0==length(res.calendar_dates))
})



test_that("test makeCalendar.inner:2", {
browser()
  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",        "uid1",         "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "01-03-2023",  "11-01-2023",  "08-03-2023",   "23-01-2023" ),
                        end_date=c(  "04-02-2023", "05-02-2023",  "31-03-2023",  "19-01-2023",  "16-03-2023",   "23-01-2023" ),
                        Days=c(      "1111110",    "0000001",     "0011100",     "0011000",     "0011000",      "1000000" ),
                        STP=c(       "P",          "P",           "P",           "O",           "C",            "C" ),
                        rowID=c(     1,            2,             3,             4,             5,              6))

  testData <- fixDates( testData )

  res <- makeCalendar.inner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]
  res.calendar_dates <- res.calendar_dates[!is.na(res.calendar_dates)]

  #this is what the code produces - it is wrong. e.g 10/3 service is missing
  expectedResult = data.table(UID=c( "uid1 a1",    "uid1 b1",    "uid1 a2",     "uid1 b2",     "uid1 a3",     "uid1 b3",     "uid1 a4"),
                        start_date=c("02-01-2023", "24-01-2023", "08-01-2023",  "24-01-2023",  "01-03-2023",  "17-03-2023",  "11-01-2023"),
                        end_date=c(  "22-01-2023", "04-02-2023", "22-01-2023",  "05-02-2023",  "07-03-2023",  "31-03-2023",  "19-01-2023"),
                        Days=c(      "1111110",    "1111110",    "0000001",     "0000001",     "0011100",     "0011100",     "0011000"),
                        STP=c(       "P",          "P",          "P",           "P",           "P",           "P",           "O"),
                        rowID=c(     1,            1,            2,             2,             3,             3,             4))

  expectedResult <- fixDates( expectedResult )

  expect_true(identical(expectedResult,res.calendar) & 0==length(res.calendar_dates))
})



test_that("test makeCalendar.inner:3", {

  browser()
  #by convention Sunday service timetables are Sunday only
  #the 'from' date should be the first day the timetable has effect (i.e. should have a 1 in the relevant day column)
  #(and I assume the same is true of the last)

  #mon-sat timetable
  #sun different operating hours on sunday
  #engineering works means having to berth in a different platform for a couple of weeks wed-fri
  #cancel mondays for 2 weeks
  #cancel sundays for 2 weeks
                                    #mon-sat       sun        -march-    wed-fri platform    cancel mon     cancel sun
  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",        "uid1",         "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "01-03-2023",  "11-01-2023",  "09-01-2023",   "15-01-2023" ),
                        end_date=c(  "04-02-2023", "05-02-2023",  "31-03-2023",  "27-01-2023",  "16-01-2023",   "22-01-2023" ),
                        Days=c(      "1111110",    "0000001",     "0011100",     "0011100",     "1000000",      "0000001" ),
                        STP=c(       "P",          "P",           "P",           "O",           "C",            "C" ),
                        rowID=c(     1,            2,             3,             4,             5,              6))

  testData <- fixDates( testData )

  res <- makeCalendar.inner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]
  res.calendar_dates <- res.calendar_dates[!is.na(res.calendar_dates)]

  expectedResult = data.table(UID=c(       "uid1 a1",    "uid1 a2",     "uid1 a3",     "uid1 b3",     "uid1 c3",     "uid1 a4"),
                              start_date=c("02-01-2023", "01-01-2023",  "01-03-2023",  "10-03-2023",  "21-03-2023",  "07-01-2023"),
                              end_date=c(  "08-01-2023", "31-01-2023",  "06-03-2023",  "19-03-2023",  "31-03-2023",  "14-01-2023"),
                              Days=c(      "1111100",    "0000011",     "0011100",     "0011100",     "0011100",     "0011000"),
                              STP=c(       "P",          "P",           "P",           "P",           "P",           "O"),
                              rowID=c(     1,            2,             3,             3,             3,             4))

  expectedResult <- fixDates( expectedResult )

  expect_true(identical(expectedResult,res.calendar) & 0==length(res.calendar_dates))
})




context("Get the example atoc files")
file_path <- file.path(tempdir(),"uk2gtfs_tests")
dir.create(file_path)
data_path <- file.path(tempdir(),"uk2gtfs_data")
dir.create(data_path)



test_that("test atoc data is there", {
  expect_true(dl_example_file(data_path, "atoc"))
  expect_true(file.exists(file.path(data_path, "atoc.zip")))
})

context("Test the main atoc function")

test_that("test atoc2gtfs singlecore", {
  gtfs <- atoc2gtfs(path_in = file.path(data_path,"atoc.zip"),
            ncores = 1)

  expect_true(class(gtfs) == "list")
})




context("Test the main atoc function, with different settings")

test_that("test atoc2gtfs singlecore", {
  gtfs <- atoc2gtfs(path_in = file.path(data_path,"atoc.zip"),
                    ncores = 1,
                    locations = "file")
  expect_true(class(gtfs) == "list")
})


context("Test import functions")

unzip(file.path(data_path,"atoc.zip"), exdir = file_path)

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
  expect_true(class(r1) == "list")
})

test_that("test importFLF", {
  r1 <- importFLF(file.path(file_path,"ttisf585.flf"))
  expect_true(class(r1) == "data.frame")
})


# test_that("test importMCA", {
#   r1 <- importMCA(file.path(file_path,"example.mca"),
#                   full_import = TRUE,
#                   silent = FALSE)
#   expect_true(class(r1) == "list")
# })

unlink(file_path, recursive = TRUE)
