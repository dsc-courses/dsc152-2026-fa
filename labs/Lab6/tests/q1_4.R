test = list(
  name = "q1_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({

        make_sleep_cases <- function() {
            cases <- list(sleep_df)
            for (j in 1:3) {
                data <- expand.grid(blue_light = c("no", "yes"),
                                    caffeine_cutoff = c("no", "yes"),
                                    replicate = seq_len(5 + j), stringsAsFactors = FALSE)
                i <- seq_len(nrow(data))
                b <- as.numeric(data$blue_light == "yes")
                c <- as.numeric(data$caffeine_cutoff == "yes")
                data$sleep_score <- 4.4 + c(-0.7, 0.8, 0)[j] * b +
                    c(0.2, -0.6, 0.4)[j] * c + c(1.1, -0.9, 0.3)[j] * b * c +
                    0.19 * sin(i * (1.2 + j / 9))
                data$blue_light <- factor(data$blue_light, levels = c("no", "yes"))
                data$caffeine_cutoff <- factor(data$caffeine_cutoff, levels = c("no", "yes"))
                data <- data[-c(2 + j, 9 + j), c("sleep_score", "blue_light", "caffeine_cutoff")]
                data <- data[rev(seq_len(nrow(data))), ]
                if (j == 3) {
                    data$blue_light <- relevel(data$blue_light, "yes")
                    data$caffeine_cutoff <- relevel(data$caffeine_cutoff, "yes")
                }
                cases[[length(cases) + 1]] <- data
            }
            cases
        }

            normalize_names <- function(x) vapply(strsplit(x, ":", fixed = TRUE), function(parts) paste(sort(parts), collapse = ":"), character(1))
            testthat::expect_true(is.function(fit_sleep_model))
            for (data in make_sleep_cases()) {
                actual <- fit_sleep_model(data)
                expected_data <- data
                expected_data$blue_light <- factor(expected_data$blue_light, levels = c("no", "yes"))
                expected_data$caffeine_cutoff <- factor(expected_data$caffeine_cutoff, levels = c("no", "yes"))
                expected <- lm(sleep_score ~ blue_light * caffeine_cutoff, expected_data)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("model", "coeffs") %in% names(actual)))
                testthat::expect_s3_class(actual$model, "lm")
                testthat::expect_equal(nobs(actual$model), nrow(data))
                testthat::expect_equal(actual$model$rank, 4L)
                testthat::expect_equal(as.numeric(model.response(model.frame(actual$model))), as.numeric(data$sleep_score), tolerance = 1e-7)
                testthat::expect_equal(as.numeric(fitted(actual$model)), as.numeric(fitted(expected)), tolerance = 1e-7)
                probe <- expand.grid(blue_light = c("no", "yes"), caffeine_cutoff = c("no", "yes"), stringsAsFactors = FALSE)
                testthat::expect_equal(as.numeric(predict(actual$model, newdata = probe)), as.numeric(predict(expected, newdata = probe)), tolerance = 1e-7)
                testthat::expect_true(is.numeric(actual$coeffs))
                testthat::expect_length(actual$coeffs, 4L)
                testthat::expect_true(!is.null(names(actual$coeffs)))
                testthat::expect_true(!anyDuplicated(names(actual$coeffs)))
                testthat::expect_setequal(names(actual$coeffs), names(coef(actual$model)))
                testthat::expect_equal(as.numeric(actual$coeffs[names(coef(actual$model))]), as.numeric(coef(actual$model)), tolerance = 1e-7)
                actual_names <- normalize_names(names(actual$coeffs))
                expected_names <- normalize_names(names(coef(expected)))
                testthat::expect_setequal(actual_names, expected_names)
                testthat::expect_equal(as.numeric(actual$coeffs[match(expected_names, actual_names)]), as.numeric(coef(expected)), tolerance = 1e-7)
            }
        })

      }
    )
  )
)