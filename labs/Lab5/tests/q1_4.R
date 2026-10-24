test = list(
  name = "q1_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(as.numeric(joint_test), 3)

      }
    )
  )
)