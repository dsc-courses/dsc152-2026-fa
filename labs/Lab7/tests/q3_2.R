test = list(
  name = "q3_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.75,
      code = {
        local({
        transform_oracle <- function(n, beta1, sigma, alpha=.05, draws=24000L) {
          counts <- c(raw=0, logged=0, both=0)
          for (offset in seq.int(1L, draws, by=800L)) {
            b <- min(800L, draws-offset+1L)
            x <- matrix(runif(n*b, 1, 24), nrow=n)
            log_y <- beta1*x + matrix(rnorm(n*b, sd=sigma), nrow=n)
            xc <- sweep(x, 2, colMeans(x), '-')
            sxx <- colSums(xc^2)
            reject <- function(y) {
              yc <- sweep(y, 2, colMeans(y), '-')
              sxy <- colSums(xc*yc)
              rss <- pmax(colSums(yc^2)-sxy^2/sxx, .Machine$double.eps)
              t <- sxy/sqrt(sxx*rss/(n-2))
              abs(t)>qt(1-alpha/2,n-2)
            }
            a <- reject(exp(log_y)); b_log <- reject(log_y)
            counts <- counts+c(sum(a),sum(b_log),sum(a&b_log))
          }
          p <- counts/draws
          list(rate=p[1:2], difference_variance=unname(p[1]+p[2]-2*p[3]-(p[1]-p[2])^2),draws=draws)
        }

            check_settings <- function(settings) {
                for (i in seq_along(settings)) {
                    s <- settings[[i]]
                    set.seed(8700+i)
                    expected <- transform_oracle(s$n,s$beta1,s$sigma,s$alpha)
                    set.seed(9720+i)
                    answer <- do.call(sim_power_logy,s)
                    testthat::expect_true(is.list(answer))
                    testthat::expect_true(all(c("power_untransformed","power_logged") %in% names(answer)))
                    values <- unname(unlist(answer[c("power_untransformed","power_logged")]))
                    testthat::expect_true(is.numeric(values)&&length(values)==2L&&all(is.finite(values))&&all(values>=0&values<=1))
                    target <- unname(expected$rate)
                    tolerance <- 6*sqrt(target*(1-target)*(1/s$reps+1/expected$draws))+.011
                    testthat::expect_true(all(abs(values-target)<=tolerance))
                    delta_tolerance <- 6*sqrt(expected$difference_variance*(1/s$reps+1/expected$draws))+.022
                    testthat::expect_lte(abs((values[2]-values[1])-(target[2]-target[1])),delta_tolerance)
                }
            }
        check_settings(list(
            list(reps=701L,n=30L,beta1=.06,sigma=.8,alpha=.05),
            list(reps=809L,n=50L,beta1=.08,sigma=1.5,alpha=.05),
            list(reps=601L,n=20L,beta1=0,sigma=.9,alpha=.2)))
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
        transform_oracle <- function(n, beta1, sigma, alpha=.05, draws=24000L) {
          counts <- c(raw=0, logged=0, both=0)
          for (offset in seq.int(1L, draws, by=800L)) {
            b <- min(800L, draws-offset+1L)
            x <- matrix(runif(n*b, 1, 24), nrow=n)
            log_y <- beta1*x + matrix(rnorm(n*b, sd=sigma), nrow=n)
            xc <- sweep(x, 2, colMeans(x), '-')
            sxx <- colSums(xc^2)
            reject <- function(y) {
              yc <- sweep(y, 2, colMeans(y), '-')
              sxy <- colSums(xc*yc)
              rss <- pmax(colSums(yc^2)-sxy^2/sxx, .Machine$double.eps)
              t <- sxy/sqrt(sxx*rss/(n-2))
              abs(t)>qt(1-alpha/2,n-2)
            }
            a <- reject(exp(log_y)); b_log <- reject(log_y)
            counts <- counts+c(sum(a),sum(b_log),sum(a&b_log))
          }
          p <- counts/draws
          list(rate=p[1:2], difference_variance=unname(p[1]+p[2]-2*p[3]-(p[1]-p[2])^2),draws=draws)
        }

            check_settings <- function(settings) {
                for (i in seq_along(settings)) {
                    s <- settings[[i]]
                    set.seed(8700+i)
                    expected <- transform_oracle(s$n,s$beta1,s$sigma,s$alpha)
                    set.seed(9720+i)
                    answer <- do.call(sim_power_logy,s)
                    testthat::expect_true(is.list(answer))
                    testthat::expect_true(all(c("power_untransformed","power_logged") %in% names(answer)))
                    values <- unname(unlist(answer[c("power_untransformed","power_logged")]))
                    testthat::expect_true(is.numeric(values)&&length(values)==2L&&all(is.finite(values))&&all(values>=0&values<=1))
                    target <- unname(expected$rate)
                    tolerance <- 6*sqrt(target*(1-target)*(1/s$reps+1/expected$draws))+.011
                    testthat::expect_true(all(abs(values-target)<=tolerance))
                    delta_tolerance <- 6*sqrt(expected$difference_variance*(1/s$reps+1/expected$draws))+.022
                    testthat::expect_lte(abs((values[2]-values[1])-(target[2]-target[1])),delta_tolerance)
                }
            }
        check_settings(list(
            list(reps=709L,n=45L,beta1=-.035,sigma=.65,alpha=.025),
            list(reps=809L,n=60L,beta1=.025,sigma=.8,alpha=.05),
            list(reps=607L,n=18L,beta1=.04,sigma=1.6,alpha=.1),
            list(reps=701L,n=80L,beta1=.06,sigma=1.5,alpha=.005)))
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            for(reps in c(1L,3L)) {
                set.seed(2031+reps)
                answer <- sim_power_logy(reps,n=27,beta1=.06,sigma=1,alpha=.15)
                for(name in c("power_untransformed","power_logged")) {
                    value <- answer[[name]]
                    testthat::expect_true(is.numeric(value)&&length(value)==1L&&is.finite(value)&&value>=0&&value<=1)
                    testthat::expect_lte(abs(value*reps-round(value*reps)),.011*reps)
                }
            }
        })

      }
    )
  )
)