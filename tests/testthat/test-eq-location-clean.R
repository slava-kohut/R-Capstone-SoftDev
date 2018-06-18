context("eq_location_clean")

test_that('eq_location_clean works as intended',{
  testString <- 'CANADA: OTTAWA'
  testString2 <- c('CANADA: OTTAWA', 'MEXICO: MEXICO CITY')

  expect_that(eq_location_clean(testString), is_identical_to('Canada'))
  expect_that(eq_location_clean(testString2), is_identical_to(c('Canada', 'Mexico')))
})
