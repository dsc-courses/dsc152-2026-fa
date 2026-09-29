test = list(
  name = "q1_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(my_dat$StudyID, c(334, 335, 336, 337))
      }
    )
  )
)