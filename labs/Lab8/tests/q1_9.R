test = list(
  name = "q1_9",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            fixtures <- list(sweetener_cc_lab8,matrix(c(12,28,9,51),2,byrow=TRUE),matrix(c(3,27,11,19),2,byrow=TRUE),matrix(c(19,7,2,18),2,byrow=TRUE))
            required <- c("cases_odds","controls_odds","odds_ratio","cross_product_or")
            for(counts in fixtures) {
                got <- odds_summary_lab8(counts)
                testthat::expect_true(is.numeric(got)&&length(got)==4L)
                testthat::expect_true(setequal(names(got),required))
                a <- counts[1,1];b <- counts[1,2];c <- counts[2,1];d <- counts[2,2]
                expected <- c(a/b,c/d,(a/b)/(c/d),a*d/(b*c))
                testthat::expect_true(all(abs(unname(got[required])-expected)<=5.1e-5))
            }
        })

      }
    )
  )
)