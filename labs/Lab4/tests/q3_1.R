test = list(
  name = "q3_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            testthat::expect_true(is.function(fit_solar_models))
            set.seed(44201)
            for (j in seq_len(4)) {
                data <- data.frame(Solar.R=rnorm(17+3*j), Temp=rnorm(17+3*j))
                data$Temp <- data$Temp + (j-2.5)*data$Solar.R
                data$Ozone <- (3-j) + (2*j-5)*data$Solar.R + (4-j)*data$Temp + rnorm(nrow(data), sd=0.4)
                result <- fit_solar_models(data)
                testthat::expect_true(is.list(result))
                testthat::expect_true("unadj" %in% names(result))
                testthat::expect_true(inherits(result$unadj, "lm"))
                reference <- lm(Ozone ~ Solar.R, data=data)
                testthat::expect_equal(unname(fitted(result$unadj)), unname(fitted(reference)), tolerance=1e-8)
                testthat::expect_equal(unname(coef(result$unadj)[c("(Intercept)", "Solar.R")]),
                                       unname(coef(reference)[c("(Intercept)", "Solar.R")]), tolerance=1e-8)
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
            testthat::expect_true(is.function(fit_solar_models))
            set.seed(44201)
            for (j in seq_len(4)) {
                data <- data.frame(Solar.R=rnorm(17+3*j), Temp=rnorm(17+3*j))
                data$Temp <- data$Temp + (j-2.5)*data$Solar.R
                data$Ozone <- (3-j) + (2*j-5)*data$Solar.R + (4-j)*data$Temp + rnorm(nrow(data), sd=0.4)
                result <- fit_solar_models(data)
                testthat::expect_true(is.list(result))
                testthat::expect_true("adj" %in% names(result))
                testthat::expect_true(inherits(result$adj, "lm"))
                reference <- lm(Ozone ~ Solar.R + Temp, data=data)
                testthat::expect_equal(unname(fitted(result$adj)), unname(fitted(reference)), tolerance=1e-8)
                testthat::expect_equal(unname(coef(result$adj)[c("(Intercept)", "Solar.R", "Temp")]),
                                       unname(coef(reference)[c("(Intercept)", "Solar.R", "Temp")]), tolerance=1e-8)
            }
        })
      }
    )
  )
)