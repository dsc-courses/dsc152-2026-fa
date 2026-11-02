test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        testthat::expect_equal(as.numeric(non_parallel_interp), 4)

      }
    )
  )
)