test = list(
  name = "q2_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            testthat::expect_true(is.numeric(final_statements))
            testthat::expect_true(all(final_statements %in% seq_len(6)))
            testthat::expect_true(!anyDuplicated(final_statements))
            testthat::expect_setequal(intersect(final_statements, c(1, 2, 3)), c(2, 3))
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            testthat::expect_true(is.numeric(final_statements))
            testthat::expect_true(all(final_statements %in% seq_len(6)))
            testthat::expect_true(!anyDuplicated(final_statements))
            testthat::expect_setequal(intersect(final_statements, c(4, 5, 6)), 6)
        })
      }
    )
  )
)