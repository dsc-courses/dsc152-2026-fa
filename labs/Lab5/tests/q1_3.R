test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            testthat::expect_true(is.function(species_contrast))
            models <- list(
                lm(Petal.Length ~ Species + Petal.Width, iris),
                lm(Petal.Length ~ Petal.Width + Species, transform(iris, Species = relevel(Species, "versicolor"))),
                lm(Petal.Length ~ Species + Petal.Width, transform(iris, Species = relevel(Species, "virginica"))))
            for (j in 1:3) {
                n <- 45 + 12 * j
                i <- seq_len(n)
                data <- data.frame(Species = factor(rep(c("setosa", "versicolor", "virginica"), length.out = n),
                                   levels = levels(iris$Species)),
                                   Petal.Width = ((i * (3 + j)) %% (n + 1)) / (n + 1) * 3)
                data$Petal.Length <- 0.7 + (j - 2) * data$Petal.Width +
                    c(0.1 * j, -0.2 * j, 0.4 * j)[as.integer(data$Species)] + sin(i * 1.31) / 5
                models[[length(models) + 1]] <- lm(Petal.Length ~ Species + Petal.Width, data)
            }
            for (model in models) {
                for (a in c("setosa", "versicolor", "virginica")) {
                    for (b in c("setosa", "versicolor", "virginica")) {
                        for (width in c(0.6, 1.5, 2.1)) {
                            actual <- species_contrast(model, a, b, width)
                            target <- predict(model, newdata = data.frame(Species = c(a, b), Petal.Width = rep(width, 2)))
                            testthat::expect_true(is.numeric(actual))
                            testthat::expect_length(actual, 1)
                            testthat::expect_equal(as.numeric(actual), as.numeric(target[1] - target[2]), tolerance = 1e-8)
                        }
                    }
                }
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(sort(as.numeric(contrast_statements)), c(1, 2, 4))

      }
    )
  )
)