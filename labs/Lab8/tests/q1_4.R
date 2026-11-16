test = list(
  name = "q1_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            make_data <- function(a,b,c,d) data.frame(eat_meats=c(rep(1,a+b),rep(0,c+d)),colon_cancer=c(rep(1,a),rep(0,b),rep(1,c),rep(0,d)))
            fixtures <- list(meat_df_lab8,make_data(4,6,3,17),make_data(1,12,7,4),make_data(0,8,3,5),make_data(3,4,0,9))
            for(data in fixtures) {
                expected <- sum(data$colon_cancer[data$eat_meats==1])/sum(data$eat_meats==1) /
                            (sum(data$colon_cancer[data$eat_meats==0])/sum(data$eat_meats==0))
                got <- calc_rr_lab8(data)
                testthat::expect_true(is.numeric(got)&&length(got)==1L&&!is.na(got))
                testthat::expect_equal(unname(is.infinite(got)),unname(is.infinite(expected)))
                if(is.finite(expected)) testthat::expect_lte(abs(unname(got)-unname(expected)),5.1e-5)
                shuffled <- data[rev(seq_len(nrow(data))),c("colon_cancer","eat_meats")]
                shuffled_result <- calc_rr_lab8(shuffled)
                testthat::expect_equal(unname(is.infinite(shuffled_result)),unname(is.infinite(expected)))
                if(is.finite(expected)) testthat::expect_lte(abs(unname(shuffled_result)-unname(expected)),5.1e-5)
            }
        })

      }
    )
  )
)