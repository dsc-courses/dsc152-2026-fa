test = list(
  name = "q2_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
          testthat::expect_true(is.function(perm_test_slope))
          permutations <- function(z) {
            if (length(z)==1L) return(matrix(z,nrow=1))
            do.call(rbind,lapply(seq_along(z),function(j) cbind(z[j],permutations(z[-j]))))
          }
          x <- c(-3,-sqrt(2),-.7,.2,sqrt(3),2.4,4.1)
          y <- c(0,4,1,6,2,5,3)
          xc <- x-mean(x)
          slopes <- as.numeric(permutations(y) %*% xc)/sum(xc^2)
          observed <- sum(xc*(y-mean(y)))/sum(xc^2)
          exact_tail <- mean(abs(slopes)>=abs(observed)-1e-10)
          set.seed(41023)
          for (sign in c(-1,1)) {
            p <- perm_test_slope(x=x,y=sign*y,n_perm=3001)
            testthat::expect_true(is.numeric(p) && length(p)==1L && is.finite(p))
            testthat::expect_gte(p,0)
            testthat::expect_lte(p,1)
            testthat::expect_lte(abs(p-exact_tail),6*sqrt(exact_tail*(1-exact_tail)/3001)+.004)
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
        set.seed(41431)
        for (sign in c(-1,1)) {
         x <- seq(-2,3,length.out=12)
         y <- sign*(2*x+c(.03,-.01,.02,-.03,.01,.03,-.02,.02,-.01,.01,-.03,.02))
         p <- perm_test_slope(x,y,n_perm=317)
         testthat::expect_gte(as.numeric(p),0)
         testthat::expect_lte(as.numeric(p),.03)
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
        set.seed(41432)
        x <- c(-3,-1,1,3); y <- c(1,-1,-1,1)
        testthat::expect_equal(as.numeric(perm_test_slope(x,y,n_perm=113)),1,tolerance=1e-8)
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
          permutations <- function(z) {
            if (length(z)==1L) return(matrix(z,nrow=1))
            do.call(rbind,lapply(seq_along(z),function(j)cbind(z[j],permutations(z[-j]))))
          }
          base_x <- c(-2,-1,0,1,3,100)
          base_y <- c(4,3,2,1,0,100)
          weights <- length(base_x)*base_x-sum(base_x)
          observed <- sum(weights*base_y)
          null_values <- as.numeric(permutations(base_y)%*%weights)
          target <- mean(abs(null_values)>=abs(observed))
          scenarios <- list(
            list(x=50+2*base_x,y=-20+3*base_y,B=751),
            list(x=-10+.4*base_x,y=30-2*base_y,B=1103)
          )
          set.seed(41433)
          for (scenario in scenarios) {
            estimate <- perm_test_slope(scenario$x,scenario$y,n_perm=scenario$B)
            testthat::expect_true(is.numeric(estimate))
            testthat::expect_length(estimate,1)
            testthat::expect_true(all(is.finite(estimate)))
            testthat::expect_gte(as.numeric(estimate),0)
            testthat::expect_lte(as.numeric(estimate),1)
            margin <- 6*sqrt(target*(1-target)/scenario$B)+.004
            testthat::expect_lte(abs(as.numeric(estimate)-target),margin)
          }
        })
      }
    )
  )
)