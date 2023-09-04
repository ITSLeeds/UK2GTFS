
context("Running basic unit tests")


fixCalendarDates <- function( df, fixDurations = TRUE, createOriginalUID = TRUE )
{
  df$start_date <- as.Date(df$start_date, format = "%d-%m-%Y")
  df$end_date <- as.Date(df$end_date, format = "%d-%m-%Y")
  if (fixDurations) df$duration <- df$end_date - df$start_date + 1
  if (createOriginalUID && "UID" %in% names(df) && !"originalUID" %in% names(df)) df$originalUID <- df$UID

  return (df)
}

removeOriginalUidField <- function( df )
{
  df$originalUID = NULL
  return (df)
}


printDifferencesDf <- function( df1, df2 )
{
  if (!identical(df1,df2))
  {
    comparison <- sapply(1:nrow(df1), function(i) all.equal(df1[i, ], df2[i, ]))
    print(comparison)
  }
}

printDifferences <- function( v1, v2 )
{
  if (!identical(v1,v2))
  {
    comparison <- all.equal( v1, v2 )
    print(comparison)
  }
}


test_that("test countIntersectingDayPatterns:1", {

  OK = TRUE

  {
    patterns = c("0000001", "1000000", "1000001")
    expectedCounts = c(2,0,0,0,0,0,2)

    counts <- countIntersectingDayPatterns( patterns )

    printDifferences( counts, expectedCounts)
    OK = OK & identical(counts, expectedCounts)
  }
  {
    patterns = c("0000001", "1000000", "0000001")
    expectedCounts = c(1,0,0,0,0,0,2)

    counts <- countIntersectingDayPatterns( patterns )

    printDifferences( counts, expectedCounts)
    OK = OK & identical(counts, expectedCounts)
  }
  {
    patterns = c("4000001", "1001100", "0000001")
    expectedCounts = c(5,0,0,1,1,0,2)

    counts <- countIntersectingDayPatterns( patterns )

    printDifferences( counts, expectedCounts)
    OK = OK & identical(counts, expectedCounts)
  }

  expect_true( OK )
})



test_that("test intersectingDayPatterns:1", {

  OK = TRUE

  {
    base = c("0000001")
    overlay = c("0000001", "1000000", "1000001", "0000010", "0000000")
    expectedResult = c(TRUE, FALSE, TRUE, FALSE, FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("0000000")
    overlay = c("0000001", "1000000", "1000001")
    expectedResult = c(FALSE, FALSE, FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("1111111")
    overlay = c("0000001", "1000000", "1000001", "0000000")
    expectedResult = c(TRUE, TRUE, TRUE, FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("1010101")
    overlay = c("0000001", "1000000", "0101010", "0000000")
    expectedResult = c(TRUE, TRUE, FALSE, FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("0000000")
    overlay = c("0000000")
    expectedResult = c(FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("0000000")
    overlay = c("")
    expectedResult = c(FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("0000000")
    overlay = c("","0000000","1111111")
    expectedResult = c(FALSE, FALSE, FALSE)

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c("0000000")
    overlay = c()
    expectedResult = NULL

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c()
    overlay = c("0000000")
    expectedResult = NULL

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }
  {
    base = c()
    overlay = c()
    expectedResult = NULL

    res = intersectingDayPatterns( base, overlay )

    printDifferences( res, expectedResult)
    OK = OK & identical(res, expectedResult)
  }

  expect_true( OK )
})





test_that("test checkOperatingDayActive:1", {

  OK = TRUE

  testData = data.table(
    start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "22-01-2023",  "26-01-2023" ),
    end_date=c(  "01-02-2023", "05-02-2023",  "31-03-2023",  "23-01-2023",  "26-01-2023" ),
    Days=c(      "1110000",    "0001001",     "0011100",     "1000000",     "0001000" ))
  expectedResult = c(TRUE,     TRUE,          TRUE,          TRUE,          TRUE)

  testData =  rbind(testData, data.table(
    start_date=c("02-09-2023", "14-09-2023",  "15-09-2023",  "11-09-2023",  "20-09-2023" ),
    end_date=c(  "03-09-2023", "15-09-2023",  "20-09-2023",  "17-09-2023",  "27-09-2023" ),
    Days=c(      "1111100",    "0010010",     "0001000",     "0000000",     "0000000" )))
                                                            #not valid data but edge case to cover off
  expectedResult = c(expectedResult,
                  c(FALSE,     FALSE,         FALSE,          FALSE,         FALSE))

  testData <- fixCalendarDates( testData )

  for (i in seq(1, length(expectedResult) ) )
  {
    OK = OK & expectedResult[i] == checkOperatingDayActive( testData[i,] )
  }

  OK = OK & all(expectedResult == checkOperatingDayActive( testData ))

  expect_true( OK )
})




test_that("test intersectingDayPattern:1", {

  OK = TRUE

  pattern1 = c("0000001", "1000000", "1000001", "0000000", "1000001")
  pattern2 = c("0000001", "1000000", "1001001", "0000000", "0100010")
  expectedResult = c(TRUE, TRUE,      TRUE,     FALSE,     FALSE)

  for (i in seq(1, length(pattern1) ) )
  {
    OK = OK & expectedResult[i] == intersectingDayPattern( pattern1[i], pattern2[i] )
  }

  expect_true( OK )
})



test_that("test intersectingDayPatterns:1", {

  patternOverlay = c("0000001", "1000000", "1000001", "0000000", "1000001", "0110110", "0100000")
  patternBase = c("1001001")
  expectedResult = c(TRUE,       TRUE,      TRUE,     FALSE,     TRUE,      FALSE,    FALSE)

  res = intersectingDayPatterns( patternBase, patternOverlay )

  names(res) <- NULL

  printDifferences(expectedResult,res)

  expect_true( identical(expectedResult,res ) )
})






test_that("test duplicate_stop_times:1", {

  testCalendar = data.table(
            rowID=c(     1,            1,             1,             2,             2,             3), #row id identifies original data
            trip_id=c(   11,           12,            13,            21,            22,            31))#trip_id is the new value assigned to duplicated stop_times

  testStopTimes = data.table(schedule=c(      1,       1,        1,        2,        2,        4), #schedule joins to row id in calendar
                             stop_sequence=c( 1,       2,        3,        1,        2,        1), #all the other columns just need to exist
                             stop_id=c(       0,       0,        0,        0,        0,        0),
                             pickup_type=c(   0,       0,        0,        0,        0,        0),
                             drop_off_type=c( 0,       0,        0,        0,        0,        0),
                             arrival_time=c(  0,       0,        0,        0,        0,        0),
                             departure_time=c(0,       0,        0,        0,        0,        0))

  duplicates <- duplicate_stop_times(testCalendar, testStopTimes, ncores = 1) #hangs / crashes with more than one thread

  expectedResult = data.table(trip_id=c(       11,      11,       11,       21,       21,       12,     12,     12,   13,     13,     13,     22,     22),
                              arrival_time=c(  0,       0,        0,        0,        0,        0,       0,      0,    0,      0,      0,      0,      0),
                              departure_time=c(0,       0,        0,        0,        0,        0,       0,      0,    0,      0,      0,      0,      0),
                              stop_id=c(       0,       0,        0,        0,        0,        0,       0,      0,    0,      0,      0,      0,      0),
                              stop_sequence=c( 1,       2,        3,        1,        2,        1,       2,      3,    1,      2,      3,      1,      2),
                              pickup_type=c(   0,       0,        0,        0,        0,        0,       0,      0,    0,      0,      0,      0,      0),
                              drop_off_type=c( 0,       0,        0,        0,        0,        0,       0,      0,    0,      0,      0,      0,      0),
                              schedule=c(      1,       1,        1,        2,        2,        1,       1,      1,    1,      1,      1,      2,      2))

  printDifferencesDf(expectedResult,duplicates)

  expect_true( identical(expectedResult,duplicates) )
})




test_that("test validateCalendarDates:1", {

    ok = TRUE

    testData = data.table(UID=c(     "uid1",       "uid2",        "uid3",        "uid4",        "uid5"),
                          start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "23-01-2023",  "26-01-2023" ),
                          end_date=c(  "01-02-2023", "05-02-2023",  "31-03-2023",  "23-01-2023",  "26-01-2023" ),
                          Days=c(      "1110000",    "0001001",     "0011100",     "1000000",     "0001000" ))
    testData <- fixCalendarDates( testData )

    ok = ok & all(validateCalendarDates( testData ))


    #uid4 wrong
    testData = data.table(UID=c(     "uid1",       "uid2",        "uid3",        "uid4",        "uid5"),
                          start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "22-01-2023",  "26-01-2023" ),
                          end_date=c(  "01-02-2023", "05-02-2023",  "31-03-2023",  "23-01-2023",  "26-01-2023" ),
                          Days=c(      "1110000",    "0001001",     "0011100",     "1000000",     "0001000" ))
    testData <- fixCalendarDates( testData )

    ok = ok & !all(validateCalendarDates( testData ))

    #uid2 wrong
    testData = data.table(UID=c(     "uid1",       "uid2",        "uid3",        "uid4",        "uid5"),
                          start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "23-01-2023",  "26-01-2023" ),
                          end_date=c(  "01-02-2023", "04-02-2023",  "31-03-2023",  "23-01-2023",  "26-01-2023" ),
                          Days=c(      "1110000",    "0001001",     "0011100",     "1000000",     "0001000" ))
    testData <- fixCalendarDates( testData )

    ok = ok & !all(validateCalendarDates( testData ))

    #uid1 wrong
    testData = data.table(UID=c(     "uid1",       "uid2",        "uid3",        "uid4",        "uid5"),
                          start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "23-01-2023",  "26-01-2023" ),
                          end_date=c(  "31-01-2023", "05-02-2023",  "31-03-2023",  "23-01-2023",  "26-01-2023" ),
                          Days=c(      "1110000",    "0001001",     "0011100",     "1000000",     "0001000" ))
    testData <- fixCalendarDates( testData )

    ok = ok & !all(validateCalendarDates( testData ))


    expect_true( ok )
})




test_that("test makeAllOneDay:1", {

    testData = data.table(UID=c(     "uid1",       "uid2",        "uid3",        "uid4",        "uid5"),
                        start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "23-01-2023",  "26-01-2023" ),
                        end_date=c(  "01-02-2023", "05-02-2023",  "31-03-2023",  "23-01-2023",  "26-01-2023" ),
                        Days=c(      "1110000",    "0001001",     "0011100",     "1000000",     "0001000" ),
                        STP=c(       "P",          "C",           "P",           "C",           "C" ),
                        rowID=c(     1,            2,             3,             6,             7))
  testData <- fixCalendarDates( testData )

  res <- makeAllOneDay( testData )

  #TODO check the contents more thoroughly

  ok = TRUE

  ok = ok & all(res$start_date == res$end_date)

  summary <- as.data.frame( res %>%
    dplyr::group_by(UID) %>%
    dplyr::summarise(count = dplyr::n()) )

  expectedResult = data.frame(UID=c( "uid1",    "uid2",    "uid3",     "uid4",     "uid5"),
                              count=c( 15,      10,         15,         1,         1))
  expectedResult$count <- as.integer( expectedResult$count )

  printDifferencesDf(expectedResult,summary)

  expect_true( identical(expectedResult,summary) & ok )
})


#bizarrely had to add special glue to make it work correctly when duplicating one object
test_that("test makeAllOneDay:2", {

  testData = data.table(UID=c(     "uid1"),
                        start_date=c("02-01-2023"),
                        end_date=c(  "18-01-2023"),
                        Days=c(      "1110000"),
                        STP=c(       "P"),
                        rowID=c(     3))
  testData <- fixCalendarDates( testData )

  res <- makeAllOneDay( testData )

  ok = TRUE

  ok = ok & all(res$start_date == res$end_date)


  expectedResult = data.table(
    UID=c(       "uid1",       "uid1",       "uid1",       "uid1",       "uid1",       "uid1"),
    start_date=c("02-01-2023", "03-01-2023", "04-01-2023", "09-01-2023", "10-01-2023", "11-01-2023"),
    end_date=c(  "02-01-2023", "03-01-2023", "04-01-2023", "09-01-2023", "10-01-2023", "11-01-2023"),
    Days=c(      "1000000",    "0100000",    "0010000",    "1000000",    "0100000",    "0010000"),
    STP=c(       "P",          "P",          "P",          "P",          "P",          "P"),
    rowID=c(     3,            3,            3,            3,            3,            3))

  expectedResult = rbind(expectedResult, data.table(
    UID=c(       "uid1",       "uid1",       "uid1"),
    start_date=c("16-01-2023", "17-01-2023", "18-01-2023"),
    end_date=c(  "16-01-2023", "17-01-2023", "18-01-2023"),
    Days=c(      "1000000",    "0100000",    "0010000"),
    STP=c(       "P",          "P",          "P"),
    rowID=c(     3,            3,            3)))

  expectedResult <- fixCalendarDates( expectedResult )

  printDifferencesDf(expectedResult,res)

  ok = ok & identical(expectedResult,res)


  summary <- as.data.frame( res %>%
                              dplyr::group_by(UID) %>%
                              dplyr::summarise(count = dplyr::n()) )

  expectedCount = data.frame(UID=c( "uid1"),
                              count=c( 9))
  expectedCount$count <- as.integer( expectedCount$count )

  printDifferencesDf(expectedCount,summary)

  expect_true( identical(expectedCount,summary) & identical(expectedResult,res) & ok )
})




test_that("test expandAllWeeks:1", {

  testData = data.table(UID=c(       "uid1",       "uid2",        "uid3",        "uid4"),
                        start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "23-01-2023"),
                        end_date=c(  "18-01-2023", "29-01-2023",  "03-03-2023",  "23-01-2023"),
                        Days=c(      "1110000",    "0001001",     "0011100",     "1000000"),
                        STP=c(       "P",          "C",           "P",           "C"),
                        rowID=c(     1,            2,             3,             6))
  testData <- fixCalendarDates( testData )

  res <- expandAllWeeks( testData )

  expectedResult = data.table(UID=c( "uid1",       "uid1",       "uid1",       "uid2",        "uid2",        "uid2",        "uid2",        "uid3",        "uid4"),
                        start_date=c("02-01-2023", "09-01-2023", "16-01-2023", "05-01-2023",  "12-01-2023",  "19-01-2023",  "26-01-2023",  "01-03-2023",  "23-01-2023"),
                        end_date=c(  "04-01-2023", "11-01-2023", "18-01-2023", "08-01-2023",  "15-01-2023",  "22-01-2023",  "29-01-2023",  "03-03-2023",  "23-01-2023"),
                        Days=c(      "1110000",    "1110000",    "1110000",    "0001001",     "0001001",     "0001001",     "0001001",     "0011100",     "1000000"),
                        STP=c(       "P",          "P",          "P",          "C",           "C",           "C",           "C",           "P",           "C"),
                        rowID=c(     1,            1,            1,            2,             2,             2,             2,             3,             6))
  expectedResult <- fixCalendarDates( expectedResult )

  printDifferencesDf(expectedResult,res)

  expect_true( identical(expectedResult,res) )
})



test_that("test expandAllWeeks:2", {

  testData = data.table(UID=c(       "uid1"),
                        start_date=c("02-01-2023"),
                        end_date=c(  "18-01-2023"),
                        Days=c(      "1110000"),
                        STP=c(       "P"),
                        rowID=c(     1))
  testData <- fixCalendarDates( testData )

  res <- expandAllWeeks( testData )

  expectedResult = data.table(UID=c(       "uid1",       "uid1",       "uid1"),
                              start_date=c("02-01-2023", "09-01-2023", "16-01-2023"),
                              end_date=c(  "04-01-2023", "11-01-2023", "18-01-2023"),
                              Days=c(      "1110000",    "1110000",    "1110000"),
                              STP=c(       "P",          "P",          "P"),
                              rowID=c(     1,            1,            1))
  expectedResult <- fixCalendarDates( expectedResult )

  printDifferencesDf(expectedResult,res)

  expect_true( identical(expectedResult,res) )
})



test_that("test hasGapInOperatingDays:1", {

  testData = c("0000000", "1000000", "0000001", "0100000", "1100000", "0000011", "0011100", "0101000", "1000001", "0001001")
  expectedResult = c(FALSE, FALSE,    FALSE,    FALSE,     FALSE,     FALSE,     FALSE,     TRUE,      TRUE,      TRUE)

  res = hasGapInOperatingDays( testData )

  expect_true( identical(expectedResult,res) )
})



#when running for real, this hangs if ncores>1 having trouble reproducing
test_that("test duplicateItems:1", {

  sourceDuplication = 99
  repetitions = 110
  expectedCount=sourceDuplication * repetitions

  testData = data.table(UID=c(       "uid1",       "uid2",       "uid3"),
                              start_date=c("02-01-2023", "09-01-2023", "16-01-2023"),
                              end_date=c(  "04-01-2023", "11-01-2023", "18-01-2023"),
                              Days=c(      "1110000",    "1110000",    "1110000"),
                              STP=c(       "P",          "P",          "P"),
                              rowID=c(     1,            2,            3))
  testData <- fixCalendarDates( testData )

  testData <- testData[rep(seq_len(.N), times = sourceDuplication)]

  testData$`_reps` = repetitions

  res = duplicateItems( testData, "UID", ncores=4 )

  summary <- as.data.frame( res %>%
                              dplyr::group_by(UID) %>%
                              dplyr::summarise(count = dplyr::n()) )

  expectedResult = data.frame(UID=c( "uid1",    "uid2",    "uid3"),
                              count=c( expectedCount,   expectedCount,     expectedCount))
  expectedResult$count <- as.integer( expectedResult$count )

  printDifferencesDf(expectedResult,summary)


  expect_true( identical(expectedResult,summary) )
})








context("Running calendar overlay unit tests")


test_that("0:fixOverlappingDates -based on priority", {
  #TODO add more test cases

  testData = data.table(
    UID=c(       "uid1",       "uid1",        "uid1",         "uid1",         "uid1",       NA,           "uid1"),
    STP=c(       "P",          "C",           "O",            "P",            "P",          "C",          "N"),
    start_date=c("02-01-2023", "11-01-2023",  "11-01-2023",   "20-01-2023",   "25-01-2023", "27-01-2023", "20-02-2023" ),
    end_date=c(  "11-01-2023", "11-01-2023",  "20-01-2023",   "25-02-2023",   "27-01-2023", "20-03-2023", "31-03-2023"))
  testData <- fixCalendarDates( testData )

  res = fixOverlappingDates(testData)

  expectedResult = data.table(                                #equal priority but overlapping
                                                              #current behaviour is to leave unchanged
    UID=c(       "uid1",       "uid1",        "uid1",         "uid1",         "uid1",       NA,           "uid1"),
    STP=c(       "P",          "C",           "O",            "P",            "P",          "C",          "N"),
    start_date=c("02-01-2023", "11-01-2023",  "12-01-2023",   "21-01-2023",   "25-01-2023", "27-01-2023", "20-02-2023" ),
    end_date=c(  "10-01-2023", "11-01-2023",  "20-01-2023",   "25-02-2023",   "27-01-2023", "20-03-2023", "31-03-2023" ))
  expectedResult <- fixCalendarDates( expectedResult )
  expectedResult$duration = testData$duration

  printDifferencesDf(expectedResult,res)

  expect_true( identical(expectedResult,res) )
})



test_that("1:test allocateCancellationsAcrossCalendars", {

  calendar = data.table(UID=c(       "uid1 a",     "uid1 b",      "uid2 c",      "uid2 d",      "uid3 e"),
                        originalUID=c("uid1",      "uid1",        "uid2",        "uid2",        "uid3"),
                        start_date=c("02-01-2023", "05-01-2023",  "01-03-2023",  "07-03-2023",  "26-01-2023" ),
                        end_date=c(  "01-02-2023", "05-02-2023",  "31-03-2023",  "26-03-2023",  "26-01-2023" ),
                        Days=c(      "1110000",    "0001001",     "0011100",     "0100001",     "0001000" ),
                        STP=c(       "P",          "C",           "P",           "C",           "C" ),
                        rowID=c(     1,            2,             3,             4,             5))
  calendar <- fixCalendarDates( calendar )
  calendar <- splitAndRebindBitmask( calendar )

  #TODO - discuss. the GTFS spec allows cancellations/ additions with no associates calendar - we're currently
  #       filtering these out, which is probably the right thing to do ?
  cancellations = data.table(                                                                   #these columns get removed
                        UID=c(       "aaaaa",      "bbbbbb",      "ccccccc",     "ddddddd",     "eeeeee",      "fffffff"),
                        originalUID=c("uid1",      "uid1",        "uid1",        "uid2",        "uid2",        "uid4"),
                        start_date=c("02-01-2023", "03-01-2023",  "06-01-2023",  "14-03-2023",  "15-01-2023",  "26-01-2023" ),
                        end_date=c(  "02-01-2023", "03-01-2023",  "06-01-2023",  "14-03-2023",  "15-01-2023",  "26-01-2023" ),
                        Days=c(      "1000000",    "0100000",     "0000100",     "0100000",     "0010000",     "0001000" ),
                        STP=c(       "C",          "C",           "C",           "C",           "C",           "C" ),
                        rowID=c(     6,            7,             8,             9,             10,            11))
  cancellations <- fixCalendarDates( cancellations )
  cancellations <- splitAndRebindBitmask( cancellations )

  res <- allocateCancellationsAcrossCalendars( calendar, cancellations )

  expectedResult = data.table(
    UID=c(       "uid1 a",     "uid1 a",      "uid2 d"),
    originalUID=c("uid1",      "uid1",        "uid2"),
    start_date=c("02-01-2023", "03-01-2023",  "14-03-2023"),
    end_date=c(  "02-01-2023", "03-01-2023",  "14-03-2023"),
    Days=c(      "1000000",    "0100000",     "0100000"),
    STP=c(       "C",          "C",           "C"),
    rowID=c(     6,            7,             9),
    duration=c(  1,            1,             1),
    monday=c(    TRUE,         FALSE,         FALSE),
    tuesday=c(   FALSE,        TRUE,          TRUE),
    wednesday=c( FALSE,        FALSE,         FALSE),
    thursday=c(  FALSE,        FALSE,         FALSE),
    friday=c(    FALSE,        FALSE,         FALSE),
    saturday=c(  FALSE,        FALSE,         FALSE),
    sunday=c(    FALSE,        FALSE,         FALSE)
   )
  expectedResult <- fixCalendarDates( expectedResult, createOriginalUID=FALSE )

  printDifferencesDf(expectedResult,res)

  expect_true( identical(expectedResult,res) )
})


test_that("2:test makeCalendarInner:one calendar entry for service", {

  testData = data.table(UID=c(       "uid1"),
                        start_date=c("02-01-2023"),
                        end_date=c(  "04-02-2023"),
                        Days=c(      "1111110"),
                        STP=c(       "P"),
                        rowID=c(     1))

  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(UID=c( "uid1"),
                              start_date=c("02-01-2023"),
                              end_date=c(  "04-02-2023"),
                              Days=c(      "1111110"),
                              STP=c(       "P"),
                              rowID=c(     1))

  expectedResult <- fixCalendarDates( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})



test_that("1.1:test makeCalendarInner:all identical patterns - more than single day overlay", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                        start_date=c("02-01-2023", "09-01-2023",  "09-01-2023"),
                        end_date=c(  "04-02-2023", "28-01-2023",  "14-01-2023"),
                        Days=c(      "1111110",    "1111110",     "1111110"),
                        STP=c(       "P",          "O",           "C"),
                        rowID=c(     1,            2,             3))

  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(
    UID=c(       "uid1 a",     "uid1 b",      "uid1 c"),
    start_date=c("02-01-2023", "15-01-2023",  "29-01-2023"),
    end_date=c(  "08-01-2023", "28-01-2023",  "04-02-2023"),
    Days=c(      "1111110",    "1111110",     "1111110"),
    STP=c(       "P",          "O",           "P"),
    rowID=c(     1,            2,             1))

  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})



test_that("3:test makeCalendarInner:one base: one day cancellations", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",         "uid1"),
                        start_date=c("02-01-2023", "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                        end_date=c(  "04-02-2023", "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                        Days=c(      "1111110",    "0010000",     "0001000",      "1000000" ),
                        STP=c(       "P",          "C",           "C",            "C" ),
                        rowID=c(     1,            4,             5,              6))

  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(UID=c(       "uid1"),
                              start_date=c("02-01-2023"),
                              end_date=c(  "04-02-2023"),
                              Days=c(      "1111110"),
                              STP=c(       "P"),
                              rowID=c(     1))
  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)


  expectedResultDates = data.table(UID=c(       "uid1",        "uid1",         "uid1"),
                                   start_date=c("11-01-2023",  "09-03-2023",   "23-01-2023" ),
                                   end_date=c(  "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                                   Days=c(      "0010000",     "0001000",      "1000000" ),
                                   STP=c(       "C",           "C",            "C" ),
                                   rowID=c(     4,             5,              6))
  expectedResultDates <- fixCalendarDates( expectedResultDates )

  res.calendar_dates = removeOriginalUidField( res.calendar_dates )
  expectedResultDates = removeOriginalUidField( expectedResultDates )

  printDifferencesDf(expectedResultDates,res.calendar_dates)


  expect_true(identical(expectedResult,res.calendar)
              & identical(expectedResultDates,res.calendar_dates))
})





test_that("4:test makeCalendarInner:one day cancellations(old)", {

  #there are multiple valid ways to process this - because of cancellations being handled at a higher level this
  #test case no longer applies - but quite a bit of work to create the test case, so keep it for now.
  expect_true(TRUE)

  if(FALSE)
  {
    #all overlays 1 day cancellations

    testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",        "uid1",         "uid1"),
                          start_date=c("02-01-2023", "08-01-2023",  "01-03-2023",  "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                          end_date=c(  "04-02-2023", "05-02-2023",  "31-03-2023",  "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                          Days=c(      "1111110",    "0000001",     "0011100",     "0010000",     "0001000",      "1000000" ),
                          STP=c(       "P",          "P",           "P",           "C",           "C",            "C" ),
                          rowID=c(     1,            2,             3,             4,             5,              6))

    testData <- fixCalendarDates( testData )

    res <- makeCalendarInner( testData )

    res.calendar <- res[[1]]
    res.calendar_dates <- res[[2]]

    expectedResult = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                                start_date=c("02-01-2023", "08-01-2023",  "01-03-2023"),
                                end_date=c(  "04-02-2023", "05-02-2023",  "31-03-2023"),
                                Days=c(      "1111110",    "0000001",     "0011100"),
                                STP=c(       "P",          "P",           "P"),
                                rowID=c(     1,            2,             3))
    expectedResult <- fixCalendarDates( expectedResult )

    res.calendar = removeOriginalUidField( res.calendar )
    expectedResult = removeOriginalUidField( expectedResult )

    printDifferencesDf(expectedResult,res.calendar)


    expectedResultDates = data.table(UID=c("uid1",        "uid1",         "uid1"),
                          start_date=c("11-01-2023",  "09-03-2023",   "23-01-2023" ),
                          end_date=c(  "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                          Days=c(      "0010000",     "0001000",      "1000000" ),
                          STP=c(       "C",           "C",            "C" ),
                          rowID=c(     4,             5,              6))
    expectedResultDates <- fixCalendarDates( expectedResultDates )

    res.calendar_dates = removeOriginalUidField( res.calendar_dates )
    expectedResultDates = removeOriginalUidField( expectedResultDates )

    expect_true(identical(expectedResult,res.calendar)
                & identical(expectedResultDates,res.calendar_dates))
  }

})





test_that("5:test makeCalendarInner:one day cancellations(current)", {

  # all overlays 1 day cancellations
  # this method splits up the base timetable, leaving gaps where there are cancellation days

  # this can create schedule entries which are by the CIF rules incorrect, because we don't
  # validate that the new start/end dates align with the day pattern bitmask

  # while the cancellation part is no longer current, this is still a good test for all the date setting logic

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",         "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                        end_date=c(  "04-02-2023", "05-02-2023",  "11-01-2023",  "09-03-2023",   "23-01-2023" ),
                        Days=c(      "1111110",    "0000001",     "0010000",     "0001000",      "1000000" ),
                        STP=c(       "P",          "P",           "C",           "C",            "C" ),
                        rowID=c(     1,            2,             4,             5,              6))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(UID=c( "uid1 a1",    "uid1 b1",    "uid1 c1",    "uid1 a2",     "uid1 b2",     "uid1 c2"),
                        start_date=c("02-01-2023", "12-01-2023", "24-01-2023", "08-01-2023",  "12-01-2023",  "24-01-2023"),
                        end_date=c(  "10-01-2023", "22-01-2023", "04-02-2023", "10-01-2023",  "22-01-2023",  "05-02-2023"),
                        Days=c(      "1111110",    "1111110",    "1111110",    "0000001",     "0000001",     "0000001"),
                        STP=c(       "P",          "P",          "P",          "P",           "P",           "P"),
                        rowID=c(     1,            1,            1,            2,             2,             2))
  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)


  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})




test_that("6:test makeCalendarInner:overlay -matching base pattern", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "09-01-2023"),
                        end_date=c(  "04-02-2023", "05-02-2023",  "21-01-2023"),
                        Days=c(      "1111110",    "0000001",     "1111110"),
                        STP=c(       "P",          "P",           "O"),
                        rowID=c(     1,            2,             3))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(UID=c(       "uid1 a1",    "uid1 b1",    "uid1 c1",    "uid1 a2"),
                              start_date=c("02-01-2023", "09-01-2023", "22-01-2023", "08-01-2023"),
                              end_date=c(  "08-01-2023", "21-01-2023", "04-02-2023", "05-02-2023"),
                              Days=c(      "1111110",    "1111110",    "1111110",    "0000001"),
                              STP=c(       "P",          "O",          "P",          "P"),
                              rowID=c(     1,            3,            1,            2))
  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)


  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))

})


test_that("6.1:test makeCalendarInner:bases with different patterns, no overlay", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                        start_date=c("22-05-2023", "25-09-2023",  "02-10-2023"),
                        end_date=c(  "22-09-2023", "26-09-2023",  "13-10-2023"),
                        Days=c(      "1111100",    "1100000",     "1111100"),
                        STP=c(       "P",          "P",           "P"),
                        rowID=c(     1,            2,             3))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  res.calendar = removeOriginalUidField( res.calendar )
  testData = removeOriginalUidField( testData )

  printDifferencesDf(testData,res.calendar)

  expect_true(identical(testData,res.calendar) & is.na(res.calendar_dates))
})


test_that("6.2:test makeCalendarInner:base is N (STP) with different patterns, no overlay", {

  testData = data.table(UID=c(       "uid1",       "uid1"),
                        start_date=c("26-06-2023", "31-07-2023"),
                        end_date=c(  "29-07-2023", "03-08-2023"),
                        Days=c(      "1111110",    "1111000"),
                        STP=c(       "N",          "N"),
                        rowID=c(     1,            2))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  res.calendar = removeOriginalUidField( res.calendar )
  testData = removeOriginalUidField( testData )

  printDifferencesDf(testData,res.calendar)

  expect_true(identical(testData,res.calendar) & is.na(res.calendar_dates))
})



test_that("7:test makeCalendarInner:overlay -different to base pattern", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "10-01-2023"),
                        end_date=c(  "04-02-2023", "05-02-2023",  "21-01-2023"),
                        Days=c(      "1111110",    "0000001",     "0111110"),
                        STP=c(       "P",          "P",           "O"),
                        rowID=c(     1,            2,             3))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(UID=c(       "uid1 a1",     "uid1 b1",   "uid1 c1",    "uid1 d1",    "uid1 e1",    "uid1"),
                              start_date=c("02-01-2023", "10-01-2023", "15-01-2023", "17-01-2023", "22-01-2023", "08-01-2023"),
                              end_date=c(  "09-01-2023", "14-01-2023", "16-01-2023", "21-01-2023", "04-02-2023", "05-02-2023"),
                              Days=c(      "1111110",    "0111110",    "1111110",    "0111110",    "1111110",    "0000001"),
                              STP=c(       "P",          "O",          "P",          "O",          "P",          "P"),
                              rowID=c(     1,            3,            1,            3,            1,            2))
  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})


test_that("8:test makeCalendarInner:overlay -different to base pattern-gap in pattern", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "10-01-2023"),
                        end_date=c(  "04-02-2023", "05-02-2023",  "20-01-2023"),
                        Days=c(      "1111110",    "0000001",     "0110100"),
                        STP=c(       "P",          "P",           "O"),
                        rowID=c(     1,            2,             3))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(
    UID=c(       "uid1 a1",    "uid1 b1",    "uid1 c1",    "uid1 d1",    "uid1 e1",    "uid1 f1"),
    start_date=c("02-01-2023", "10-01-2023", "11-01-2023", "12-01-2023", "13-01-2023", "14-01-2023"),
    end_date=c(  "09-01-2023", "10-01-2023", "11-01-2023", "12-01-2023", "13-01-2023", "16-01-2023"),
    Days=c(      "1111110",    "0100000",    "0010000",    "1111110",    "0000100",    "1111110"),
    STP=c(       "P",          "O",          "O",          "P",          "O",          "P"),
    rowID=c(     1,            3,            3,            1,            3,            1))

  expectedResult = rbind(expectedResult, data.table(
    UID=c(       "uid1 g1",    "uid1 h1",    "uid1 i1",    "uid1 j1",    "uid1 k1",    "uid1"),
    start_date=c("17-01-2023", "18-01-2023", "19-01-2023", "20-01-2023", "21-01-2023", "08-01-2023"),
    end_date=c(  "17-01-2023", "18-01-2023", "19-01-2023", "20-01-2023", "04-02-2023", "05-02-2023"),
    Days=c(      "0100000",    "0010000",    "1111110",    "0000100",    "1111110",    "0000001"),
    STP=c(       "O",          "O",          "P",          "O",          "P",          "P"),
    rowID=c(     3,            3,            1,            3,            1,            2)))

  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})


test_that("9:test makeCalendarInner:overlay -different to base pattern-gap in pattern -creating base fragments to be skipped", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "10-01-2023"),
                        end_date=c(  "03-02-2023", "05-02-2023",  "20-01-2023"),
                        Days=c(      "0111100",    "0000001",     "0110100"),
                        STP=c(       "P",          "P",           "O"),
                        rowID=c(     1,            2,             3))
  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(
    UID=c(       "uid1 a1",    "uid1 b1",    "uid1 c1",    "uid1 d1",    "uid1 e1"),     #the 'f' calendar gets thrown away
    start_date=c("02-01-2023", "10-01-2023", "11-01-2023", "12-01-2023", "13-01-2023"),
    end_date=c(  "09-01-2023", "10-01-2023", "11-01-2023", "12-01-2023", "13-01-2023"),
    Days=c(      "0111100",    "0100000",    "0010000",    "0111100",    "0000100"),
    STP=c(       "P",          "O",          "O",          "P",          "O"),
    rowID=c(     1,            3,            3,            1,            3))

  expectedResult = rbind(expectedResult, data.table(
    UID=c(       "uid1 g1",    "uid1 h1",    "uid1 i1",    "uid1 j1",    "uid1 k1",    "uid1"),
    start_date=c("17-01-2023", "18-01-2023", "19-01-2023", "20-01-2023", "21-01-2023", "08-01-2023"),
    end_date=c(  "17-01-2023", "18-01-2023", "19-01-2023", "20-01-2023", "03-02-2023", "05-02-2023"),
    Days=c(      "0100000",    "0010000",    "0111100",    "0000100",    "0111100",    "0000001"),
    STP=c(       "O",          "O",          "P",          "O",          "P",          "P"),
    rowID=c(     3,            3,            1,            3,            1,            2)))

  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})




test_that("10:test makeCalendarInner", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",        "uid1",         "uid1",         "uid1"),
                        start_date=c("02-01-2023", "08-01-2023",  "01-03-2023",  "11-01-2023",  "12-01-2023",   "08-03-2023",   "23-01-2023" ),
                        end_date=c(  "03-02-2023", "05-02-2023",  "31-03-2023",  "19-01-2023",  "12-01-2023",   "09-03-2023",   "23-01-2023" ),
                        Days=c(      "1111100",    "0000001",     "0011100",     "0011000",     "0001000",      "0011000",      "1000000" ),
                        STP=c(       "P",          "P",           "P",           "O",           "C",            "C",            "C" ),
                        rowID=c(     1,            2,             3,             4,             5,              6,              7))

  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  #this is a more complex expansion than strictly necessary - could add more logic to see if the base / overlay patterns
  #currently we just test if the patterns collide from a operating day mask perspective, but not if they overlap for operating period.
  #e.g. if we have base timetables for march and april with different operating patterns it will expand on a week-by-week basis
  #     instead of going 'oh that's fine, march and april don't overlap

  expectedResult = data.table(
    UID=c(       "uid1 a1",    "uid1 b1",    "uid1 c1",    "uid1 d1",    "uid1 e1",     "uid1 f1"),
    start_date=c("02-01-2023", "11-01-2023", "13-01-2023", "18-01-2023", "20-01-2023",  "24-01-2023"),
    end_date=c(  "10-01-2023", "11-01-2023", "17-01-2023", "19-01-2023", "22-01-2023",  "03-02-2023"),
    Days=c(      "1111100",    "0011000",    "1111100",    "0011000",    "1111100",     "1111100"),
    STP=c(       "P",          "O",          "P",          "O",          "P",           "P"),
    rowID=c(     1,            4,            1,            4,            1,             1))

  expectedResult = rbind(expectedResult, data.table(
    UID=c(       "uid1",       "uid1 c3",    "uid1 d3"    ),
    start_date=c("08-01-2023", "01-03-2023", "10-03-2023" ),
    end_date=c(  "05-02-2023", "07-03-2023", "31-03-2023" ),
    Days=c(      "0000001",    "0011100",    "0011100"    ),
    STP=c(       "P",          "P",          "P"          ),
    rowID=c(     2,            3,            3            )))

  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})




test_that("11:test makeCalendarInner: overlay matching pattern of a base that is offset temporaly", {

  testData = data.table(UID=c(       "uid1",       "uid1",        "uid1",        "uid1",        "uid1"),
                        start_date=c("04-01-2023", "08-01-2023",  "01-03-2023",  "11-01-2023",  "08-03-2023"),
                        end_date=c(  "02-02-2023", "05-02-2023",  "30-03-2023",  "19-01-2023",  "16-03-2023"),
                        Days=c(      "0011000",    "0000001",     "0011000",     "0011000",     "0011000"),
                        STP=c(       "P",          "P",           "P",           "O",           "C"),
                        rowID=c(     1,            2,             3,             4,             5))

  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  #this is what the code produces - it is wrong. e.g 10/3 service is missing
  expectedResult = data.table(UID=c( "uid1 a1",    "uid1 b1",    "uid1 c1",     "uid1 d1",     "uid1 e1",     "uid1 a2"),
                        start_date=c("04-01-2023", "11-01-2023", "20-01-2023",  "01-03-2023",  "17-03-2023",  "08-01-2023"),
                        end_date=c(  "10-01-2023", "19-01-2023", "02-02-2023",  "07-03-2023",  "30-03-2023",  "05-02-2023"),
                        Days=c(      "0011000",    "0011000",    "0011000",     "0011000",     "0011000",     "0000001"),
                        STP=c(       "P",          "O",          "P",           "P",           "P",           "P"),
                        rowID=c(     1,            4,            1,             3,             3,             2))

  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})


test_that("12: test makeCalendarInner", {

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

  testData <- fixCalendarDates( testData )

  res <- makeCalendarInner( testData )

  res.calendar <- res[[1]]
  res.calendar_dates <- res[[2]]

  expectedResult = data.table(
    UID=c(       "uid1 a1",    "uid1 b1",    "uid1 c1",    "uid1 d1",    "uid1 e1",    "uid1 f1"),
    start_date=c("02-01-2023", "10-01-2023", "11-01-2023", "14-01-2023", "17-01-2023", "18-01-2023"),
    end_date=c(  "08-01-2023", "10-01-2023", "13-01-2023", "15-01-2023", "17-01-2023", "20-01-2023"),
    Days=c(      "1111110",    "1111110",    "0011100",    "1111110",    "1111110",    "0011100"),
    STP=c(       "P",          "P",          "O",          "P",          "P",          "O"),
    rowID=c(     1,            1,            4,            1,            1,            4))

  expectedResult = rbind(expectedResult, data.table(
    UID=c(       "uid1 g1",    "uid1 h1",    "uid1 i1",    "uid1 a2",    "uid1 c2",    "uid1 d3"),
    start_date=c("21-01-2023", "25-01-2023", "28-01-2023", "08-01-2023", "23-01-2023", "01-03-2023"),
    end_date=c(  "24-01-2023", "27-01-2023", "04-02-2023", "14-01-2023", "05-02-2023", "31-03-2023"),
    Days=c(      "1111110",    "0011100",    "1111110",    "0000001",    "0000001",    "0011100"),
    STP=c(       "P",          "O",          "P",          "P",          "P",          "P"),
    rowID=c(     1,            4,            1,            2,            2,            3)))

  expectedResult <- fixCalendarDates( expectedResult )

  res.calendar = removeOriginalUidField( res.calendar )
  expectedResult = removeOriginalUidField( expectedResult )

  printDifferencesDf(expectedResult,res.calendar)

  expect_true(identical(expectedResult,res.calendar) & is.na(res.calendar_dates))
})


