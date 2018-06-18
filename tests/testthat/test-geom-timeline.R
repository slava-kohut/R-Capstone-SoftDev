context("geom_timeline")

test_that("geom_timeline produces plots", {

  data("eq_data")
  p <- ggplot(data = eq_data %>% dplyr::filter(COUNTRY == "ITALY"),
         aes(x = DATE, size = EQ_PRIMARY, color = TOTAL_DEATHS,
             xmin = as.Date('1950-01-01'),
             xmax = as.Date('2015-01-01'))) +
    geom_timeline() +
    theme_eq

  expect_that(p, is_a('ggplot'))
  expect_equal(p$mapping$x, as.name('DATE'))
  expect_equal(length(p$layers), 1)

})
