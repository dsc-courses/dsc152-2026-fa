test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        set.seed(41421)
        for (sigma in c(.7,3.5)) {
            d <- gen_violation(n=16000,sigma=sigma,condition="skew")
            testthat::expect_length(d$x,16000)
            testthat::expect_length(d$y,16000)
            testthat::expect_true(is.numeric(d$x) && is.numeric(d$y))
            testthat::expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
            testthat::expect_gte(min(d$x),-5)
            testthat::expect_lte(max(d$x),5)
            testthat::expect_lte(abs(mean(d$x)),.16)
            testthat::expect_lte(abs(sd(d$x)-10/sqrt(12)),.12)
            z <- (d$y-2)/sigma
            testthat::expect_gte(min(z),-1/sqrt(2)-1e-10)
            testthat::expect_lte(abs(mean(z)),.10)
            testthat::expect_lte(abs(sd(z)-1),.12)
            testthat::expect_lte(abs(cor(d$x,z)),.07)
            for (cutoff in c(-.5,0,1)) testthat::expect_lte(abs(mean(z<=cutoff)-(2*pnorm(sqrt(1+sqrt(2)*cutoff))-1)),.03)
        }
        for (n in c(9,31)) {
         d <- gen_violation(n=n,sigma=1.3,condition="skew")
         testthat::expect_length(d$x,n); testthat::expect_length(d$y,n)
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
        set.seed(41421)
        for (sigma in c(.7,3.5)) {
            d <- gen_violation(n=16000,sigma=sigma,condition="unequal")
            testthat::expect_length(d$x,16000)
            testthat::expect_length(d$y,16000)
            testthat::expect_true(is.numeric(d$x) && is.numeric(d$y))
            testthat::expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
            testthat::expect_gte(min(d$x),-5)
            testthat::expect_lte(max(d$x),5)
            testthat::expect_lte(abs(mean(d$x)),.16)
            testthat::expect_lte(abs(sd(d$x)-10/sqrt(12)),.12)
            z <- (d$y-2)/(sigma*(.5+(d$x+5)/5))
            for (keep in list(d$x<0,d$x>=0)) {
                testthat::expect_lte(abs(mean(z[keep])),.10)
                testthat::expect_lte(abs(sd(z[keep])-1),.10)
                testthat::expect_lte(abs(mean(z[keep]<=1)-pnorm(1)),.035)
            }
            testthat::expect_lte(abs(cor(d$x,z)),.07)
        }
        for (n in c(9,31)) {
         d <- gen_violation(n=n,sigma=1.3,condition="unequal")
         testthat::expect_length(d$x,n); testthat::expect_length(d$y,n)
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
        set.seed(41421)
        for (sigma in c(.7,3.5)) {
            d <- gen_violation(n=16000,sigma=sigma,condition="dependent")
            testthat::expect_length(d$x,16000)
            testthat::expect_length(d$y,16000)
            testthat::expect_true(is.numeric(d$x) && is.numeric(d$y))
            testthat::expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
            testthat::expect_gte(min(d$x),-5)
            testthat::expect_lte(max(d$x),5)
            testthat::expect_equal(as.numeric(d$x),seq(-5,5,length.out=16000),tolerance=1e-10)
            z <- (d$y-2)/sigma
            testthat::expect_lte(abs(mean(z)),.18)
            testthat::expect_lte(abs(sd(z)-1),.12)
            testthat::expect_lte(abs(cor(head(z,-1),tail(z,-1))-.8),.06)
        }
        for (n in c(9,31)) {
         d <- gen_violation(n=n,sigma=1.3,condition="dependent")
         testthat::expect_length(d$x,n); testthat::expect_length(d$y,n)
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
        set.seed(41421)
        for (sigma in c(.7,3.5)) {
            d <- gen_violation(n=16000,sigma=sigma,condition="quadratic")
            testthat::expect_length(d$x,16000)
            testthat::expect_length(d$y,16000)
            testthat::expect_true(is.numeric(d$x) && is.numeric(d$y))
            testthat::expect_true(all(is.finite(d$x)) && all(is.finite(d$y)))
            testthat::expect_gte(min(d$x),-5)
            testthat::expect_lte(max(d$x),5)
            testthat::expect_lte(abs(mean(d$x)),.16)
            testthat::expect_lte(abs(sd(d$x)-10/sqrt(12)),.12)
            z <- (d$y-2-.25*d$x^2)/sigma
            for (keep in list(d$x<0,d$x>=0)) {
                testthat::expect_lte(abs(mean(z[keep])),.10)
                testthat::expect_lte(abs(sd(z[keep])-1),.10)
                testthat::expect_lte(abs(mean(z[keep]<=1)-pnorm(1)),.035)
            }
            testthat::expect_lte(abs(cor(d$x,z)),.07)
        }
        for (n in c(9,31)) {
         d <- gen_violation(n=n,sigma=1.3,condition="quadratic")
         testthat::expect_length(d$x,n); testthat::expect_length(d$y,n)
        }
        })
      }
    )
  )
)