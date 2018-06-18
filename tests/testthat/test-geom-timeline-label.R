context("geom_timeline_label")

test_that('geom_timeline_label works correctly', {

  data("eq_data")
  p <- ggplot(data = eq_data %>% dplyr::filter(COUNTRY %in% c('COLOMBIA', 'MEXICO', 'USA')),
         aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY,
             color = TOTAL_DEATHS, xmin = as.Date('1970-01-01'),
             xmax = as.Date('2015-01-01'))) +
    geom_timeline_label(aes(label = as.character(DATE)), nmax = 2) +
    geom_timeline() +
    theme_eq

  expect_that(p, is_a('ggplot'))
  expect_equal(p$mapping$x, as.name('DATE'))
  expect_equal(length(p$layers), 2)

})
