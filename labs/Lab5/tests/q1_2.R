test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            testthat::expect_true(is.function(fit_species_models))
            data_sets <- list(iris)
            for (j in 1:3) {
                n <- c(63, 87, 114)[j]
                i <- seq_len(n)
                data <- data.frame(
                    Petal.Width = 0.2 + ((i * (7 + j)) %% (n + 1)) / (n + 1) * (2 + j / 3),
                    Species = factor(rep(c("setosa", "versicolor", "virginica"), length.out = n), levels = levels(iris$Species)))
                species_effect <- c(-0.3, 0.5, -0.1)[as.integer(data$Species)]
                data$Petal.Length <- 1 + (0.4 + 0.3 * j) * data$Petal.Width + j * species_effect + 0.17 * sin(i * 1.7)
                data_sets[[length(data_sets) + 1]] <- data
            }
            for (data in data_sets) {
                result <- fit_species_models(data)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("full", "reduced") %in% names(result)))
                expected <- lm(Petal.Length ~ Species + Petal.Width, data = data)
                actual <- result[["full"]]
                testthat::expect_s3_class(actual, "lm")
                testthat::expect_equal(nobs(actual), nrow(data))
                testthat::expect_equal(df.residual(actual), df.residual(expected))
                testthat::expect_equal(as.numeric(model.response(model.frame(actual))), data$Petal.Length, tolerance = 1e-8)
                testthat::expect_equal(as.numeric(fitted(actual)), as.numeric(fitted(expected)), tolerance = 1e-8)
                probe <- data.frame(Petal.Width = rep(c(0.3, 1.2, 2.3), each = 3),
                    Species = factor(rep(levels(data$Species), 3), levels = levels(data$Species)))
                testthat::expect_equal(as.numeric(predict(actual, newdata = probe)), as.numeric(predict(expected, newdata = probe)), tolerance = 1e-8)
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            testthat::expect_true(is.function(fit_species_models))
            data_sets <- list(iris)
            for (j in 1:3) {
                n <- c(63, 87, 114)[j]
                i <- seq_len(n)
                data <- data.frame(
                    Petal.Width = 0.2 + ((i * (7 + j)) %% (n + 1)) / (n + 1) * (2 + j / 3),
                    Species = factor(rep(c("setosa", "versicolor", "virginica"), length.out = n), levels = levels(iris$Species)))
                species_effect <- c(-0.3, 0.5, -0.1)[as.integer(data$Species)]
                data$Petal.Length <- 1 + (0.4 + 0.3 * j) * data$Petal.Width + j * species_effect + 0.17 * sin(i * 1.7)
                data_sets[[length(data_sets) + 1]] <- data
            }
            for (data in data_sets) {
                result <- fit_species_models(data)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("full", "reduced") %in% names(result)))
                expected <- lm(Petal.Length ~ Petal.Width, data = data)
                actual <- result[["reduced"]]
                testthat::expect_s3_class(actual, "lm")
                testthat::expect_equal(nobs(actual), nrow(data))
                testthat::expect_equal(df.residual(actual), df.residual(expected))
                testthat::expect_equal(as.numeric(model.response(model.frame(actual))), data$Petal.Length, tolerance = 1e-8)
                testthat::expect_equal(as.numeric(fitted(actual)), as.numeric(fitted(expected)), tolerance = 1e-8)
                probe <- data.frame(Petal.Width = rep(c(0.3, 1.2, 2.3), each = 3),
                    Species = factor(rep(levels(data$Species), 3), levels = levels(data$Species)))
                testthat::expect_equal(as.numeric(predict(actual, newdata = probe)), as.numeric(predict(expected, newdata = probe)), tolerance = 1e-8)
            }
        })

      }
    )
  )
)