test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
          for (d in list(data.frame(x=c(-4,-1,0,.3,3,7),y=c(2,0,3,-1,4,5)),
                         data.frame(x=c(0,0,1,1,2,2,4,5),y=c(4,3,2,4,0,1,-2,-1)))) {
            result <- fit_slr(d); reference <- lm(y~x,data=d)
            testthat::expect_true(inherits(result$model,"lm"))
            testthat::expect_equal(unname(fitted(result$model)),unname(fitted(reference)),tolerance=1e-8)
            testthat::expect_equal(unname(residuals(result$model)),unname(residuals(reference)),tolerance=1e-8)
          }
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
          for (d in list(data.frame(x=c(-4,-1,0,.3,3,7),y=c(2,0,3,-1,4,5)), solar_data)) {
            result <- fit_slr(d); reference <- summary(lm(y~x,data=d))$coefficients["x",]
            testthat::expect_equal(as.numeric(result$beta1),unname(reference[1]),tolerance=1e-8)
            testthat::expect_equal(as.numeric(result$se),unname(reference[2]),tolerance=1e-8)
            testthat::expect_equal(as.numeric(result$p_value),unname(reference[4]),tolerance=1e-8)
          }
          testthat::expect_equal(as.numeric(solar_fit$beta1),unname(coef(lm(y~x,data=solar_data))["x"]),tolerance=1e-8)
        })
      }
    )
  )
)