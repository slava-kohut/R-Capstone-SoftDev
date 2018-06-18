context("eq_clean_data")

test_that('eq_clean_data can read the NOAA data set properly', {
  expect_that(eq_clean_data('./', fileName = 'basic.txt'), is_a('data.frame'))

  testData <- eq_clean_data('./', fileName = 'basic.txt')
  expect_that(testData$LATITUDE, is_a('numeric'))
  expect_that(testData$LONGITUDE, is_a('numeric'))
  expect_that(testData$LOCATION_NAME, is_a('character'))
  expect_that(testData$DATE, is_a('Date'))
})
