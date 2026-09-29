test = list(
  name = "q1_2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(what_cant_colon_do, 4)
      }
    )
  )
)