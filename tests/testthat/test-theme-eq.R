context("theme_eq")

test_that('earthquake theme works',{

  p <- ggplot() + geom_histogram(data = data.frame( x = rnorm(100)), aes(x), bins = 10)+ theme_eq

  expect_identical(p$theme$legend.position, "bottom")
  expect_equal(p$theme$axis.line.y, element_blank())
})
