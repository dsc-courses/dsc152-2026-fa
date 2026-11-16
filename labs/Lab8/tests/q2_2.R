test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
            bmi <- seq(17,48,length.out=120)
            u <- ((seq_len(120)*37)%%127+.5)/127
            fixtures <- list(diabetes_lab8,
                data.frame(BMI=bmi,Outcome=as.integer(u<plogis(-2+.06*bmi))),
                data.frame(BMI=rev(bmi),Outcome=as.integer(u<plogis(2-.065*rev(bmi)))))
            for(data in fixtures) {
                answer <- fit_logistic(data)
                testthat::expect_true(is.list(answer))
                testthat::expect_true(all(c("model","coefficients","bmi_inference") %in% names(answer)))
                model <- answer$model
                reference <- glm(Outcome~BMI,data=data,family=binomial())
                testthat::expect_true(inherits(model,"glm"))
                testthat::expect_equal(model$family$family,"binomial")
                testthat::expect_equal(model$family$link,"logit")
                testthat::expect_equal(nobs(model),nrow(data))
                testthat::expect_equal(model$rank,reference$rank)
                testthat::expect_equal(unname(predict(model,newdata=data,type="response")),unname(predict(reference,newdata=data,type="response")),tolerance=1e-6)
                probe <- data.frame(BMI=c(19,30,43))
                testthat::expect_equal(unname(predict(model,newdata=probe,type="response")),unname(predict(reference,newdata=probe,type="response")),tolerance=1e-6)
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
            bmi <- seq(17,48,length.out=120)
            u <- ((seq_len(120)*37)%%127+.5)/127
            fixtures <- list(diabetes_lab8,
                data.frame(BMI=bmi,Outcome=as.integer(u<plogis(-2+.06*bmi))),
                data.frame(BMI=rev(bmi),Outcome=as.integer(u<plogis(2-.065*rev(bmi)))))
            for(data in fixtures) {
                answer <- fit_logistic(data)
                reference <- glm(Outcome~BMI,data=data,family=binomial())
                testthat::expect_true(is.numeric(answer$coefficients)&&length(answer$coefficients)==2L&&all(is.finite(answer$coefficients)))
                testthat::expect_true(setequal(names(answer$coefficients),c("(Intercept)","BMI")))
                testthat::expect_true(all(abs(unname(answer$coefficients[c("(Intercept)","BMI")])-unname(coef(reference)[c("(Intercept)","BMI")]))<=5.1e-5))
                inference <- answer$bmi_inference
                testthat::expect_true(is.numeric(inference)&&length(inference)==2L&&all(is.finite(inference)))
                testthat::expect_true(setequal(names(inference),c("se","p_value")))
                expected <- summary(reference)$coefficients["BMI",]
                testthat::expect_lte(abs(unname(inference["se"])-unname(expected["Std. Error"])),5.1e-5)
                testthat::expect_lte(abs(unname(inference["p_value"])-unname(expected["Pr(>|z|)"])),5.1e-5)
            }
        })

      }
    )
  )
)