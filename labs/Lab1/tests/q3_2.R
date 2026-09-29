test = list(
  name = "q3_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(grad_status, c("You can graduate!", "rip", "You can graduate!", "You can graduate!"))
      }
    )
  )
)