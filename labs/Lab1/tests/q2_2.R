test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(dim(my_dat), c(4, 4))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(my_dat$Year, c("Sophomore", "Junior", "Junior", "Sophomore"))
      }
    )
  )
)