context("eq_create_label")

test_that('eq_create_label creates labels',{
  expect_that(eq_create_label('Mexico', 5, 100),
              is_identical_to('<b>Location:</b> Mexico<br /><b>Magnitude:</b> 5<br /><b>Total deaths:</b> 100'))
  }
)
