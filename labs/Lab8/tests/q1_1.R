test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
            tables <- list(rr_counts_lab8,matrix(c(12,18,4,36),2,byrow=TRUE),
                matrix(c(3,7,12,8),2,byrow=TRUE),matrix(c(0,9,2,6),2,byrow=TRUE),matrix(c(4,8,0,13),2,byrow=TRUE))
            for(counts in tables) {
                got <- table_risks_lab8(counts)
                testthat::expect_true(is.numeric(got)&&length(got)==3L)
                testthat::expect_true(setequal(names(got),c("exposed","unexposed","relative_risk")))
                expected <- c(exposed=counts[1,1]/sum(counts[1,]),unexposed=counts[2,1]/sum(counts[2,]))
                expected <- c(expected,relative_risk=unname(expected[1]/expected[2]))
                actual <- unname(got[names(expected)])
                testthat::expect_equal(is.infinite(actual),is.infinite(unname(expected)))
                finite <- is.finite(expected)
                testthat::expect_true(all(is.finite(actual[finite])))
                testthat::expect_true(all(abs(actual[finite]-unname(expected[finite]))<=5.1e-5))
            }
        })

      }
    )
  )
)