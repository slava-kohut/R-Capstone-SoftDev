context("eq_map")

test_that('eq_map produces maps',{

  testData <- data.frame(LATITUDE = c(35, 36),
                         LONGITUDE = c(-100, 101),
                         DATE = c('Dec 02', 'Nov 11'),
                         EQ_PRIMARY = c(3,5))

  expect_that(eq_map(testData), is_a('leaflet'))
})
