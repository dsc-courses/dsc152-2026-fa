test = list(
  name = "q1_8",
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

            testthat::expect_true(is.function(filter_effect))
            for (data in make_sleep_cases()) {
                for (blue_reference in c("no", "yes")) {
                    for (caffeine_reference in c("no", "yes")) {
                        data$blue_light <- relevel(data$blue_light, blue_reference)
                        data$caffeine_cutoff <- relevel(data$caffeine_cutoff, caffeine_reference)
                        model <- lm(sleep_score ~ caffeine_cutoff * blue_light, data = data)
                        for (cutoff in c("no", "yes")) {
                            actual <- filter_effect(model, cutoff)
                            means <- predict(model, newdata = data.frame(blue_light = c("no", "yes"), caffeine_cutoff = rep(cutoff, 2)))
                            testthat::expect_true(is.numeric(actual))
                            testthat::expect_length(actual, 1L)
                            testthat::expect_equal(as.numeric(actual), as.numeric(means[2] - means[1]), tolerance = 1e-7)
                        }
                    }
                }
            }
        })

      }
    )
  )
)