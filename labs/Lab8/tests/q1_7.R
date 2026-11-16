test = list(
  name = "q1_7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            fixtures <- list(
                list(values=rep(c(.4,.6,1.1,2),137),observed=1.5),
                list(values=rep(c(.25,.5,.75,1,1.25,1.5,2),91),observed=.5),
                list(values=seq(.2,2.4,length.out=601),observed=1.25))
            for(s in fixtures) {
                got <- rr_perm_pvalue_lab8(s$values,s$observed)
                expected <- mean(abs(s$values-1)>=abs(s$observed-1))
                testthat::expect_true(is.numeric(got)&&length(got)==1L&&is.finite(got)&&got>=0&&got<=1)
                testthat::expect_lte(abs(unname(got)-expected),.0051)
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            values <- rep(c(0,.5,1,1.5,2,Inf),103)
            for(observed in c(0,1,1.5,Inf)) {
                got <- rr_perm_pvalue_lab8(values,observed)
                expected <- mean(abs(values-1)>=abs(observed-1))
                testthat::expect_true(is.numeric(got)&&length(got)==1L&&is.finite(got)&&got>=0&&got<=1)
                testthat::expect_lte(abs(unname(got)-expected),.0051)
            }
        })

      }
    )
  )
)