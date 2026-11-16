test = list(
  name = "q1_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
            make_data <- function(n1,n0,total,exposed_cases) data.frame(
                eat_meats=c(rep(1,n1),rep(0,n0)),
                colon_cancer=c(rep(1,exposed_cases),rep(0,n1-exposed_cases),rep(1,total-exposed_cases),rep(0,n0-total+exposed_cases)))
            check_null <- function(data,reps,seed) {
                n1 <- sum(data$eat_meats==1);n0 <- sum(data$eat_meats==0);m <- sum(data$colon_cancer)
                set.seed(seed)
                got <- permute_rr_lab8(data,reps)
                testthat::expect_true(is.numeric(got)&&length(got)==reps)
                testthat::expect_true(all(!is.na(got))&&all(got>=0))
                estimated_k <- rep(m,length(got))
                finite <- is.finite(got)
                estimated_k[finite] <- m*n1*got[finite]/(n0+n1*got[finite])
                k <- round(estimated_k)
                testthat::expect_true(all(k>=max(0,m-n0)&k<=min(m,n1)))
                feasible <- (k/n1)/((m-k)/n0)
                testthat::expect_equal(unname(is.infinite(got)),unname(is.infinite(feasible)))
                testthat::expect_true(all(abs(got[finite]-feasible[finite])<=.0051+1e-8*abs(feasible[finite])))
                cutoffs <- unique(c(max(0,m-n0),qhyper(c(.1,.3,.5,.7,.9),m,nrow(data)-m,n1)))
                if (reps >= 100L) for(cutoff in cutoffs) {
                    target <- phyper(cutoff,m,nrow(data)-m,n1)
                    allowance <- 6*sqrt(target*(1-target)/reps)+.01
                    testthat::expect_lte(abs(mean(k<=cutoff)-target),allowance)
                }
            }
        check_null(meat_df_lab8,809L,471)
        check_null(make_data(31,45,23,11),1009L,472)
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
            make_data <- function(n1,n0,total,exposed_cases) data.frame(
                eat_meats=c(rep(1,n1),rep(0,n0)),
                colon_cancer=c(rep(1,exposed_cases),rep(0,n1-exposed_cases),rep(1,total-exposed_cases),rep(0,n0-total+exposed_cases)))
            check_null <- function(data,reps,seed) {
                n1 <- sum(data$eat_meats==1);n0 <- sum(data$eat_meats==0);m <- sum(data$colon_cancer)
                set.seed(seed)
                got <- permute_rr_lab8(data,reps)
                testthat::expect_true(is.numeric(got)&&length(got)==reps)
                testthat::expect_true(all(!is.na(got))&&all(got>=0))
                estimated_k <- rep(m,length(got))
                finite <- is.finite(got)
                estimated_k[finite] <- m*n1*got[finite]/(n0+n1*got[finite])
                k <- round(estimated_k)
                testthat::expect_true(all(k>=max(0,m-n0)&k<=min(m,n1)))
                feasible <- (k/n1)/((m-k)/n0)
                testthat::expect_equal(unname(is.infinite(got)),unname(is.infinite(feasible)))
                testthat::expect_true(all(abs(got[finite]-feasible[finite])<=.0051+1e-8*abs(feasible[finite])))
                cutoffs <- unique(c(max(0,m-n0),qhyper(c(.1,.3,.5,.7,.9),m,nrow(data)-m,n1)))
                if (reps >= 100L) for(cutoff in cutoffs) {
                    target <- phyper(cutoff,m,nrow(data)-m,n1)
                    allowance <- 6*sqrt(target*(1-target)/reps)+.01
                    testthat::expect_lte(abs(mean(k<=cutoff)-target),allowance)
                }
            }
        check_null(make_data(7,9,4,2),1001L,599)
        check_null(make_data(23,13,10,7),701L,600)
        for(reps in c(1L,7L)) check_null(make_data(9,12,5,2),reps,601+reps)
        })

      }
    )
  )
)