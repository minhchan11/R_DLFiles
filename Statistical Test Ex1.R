statisticalTestingExercise_1=function() {
  require(utils)
  require(car)
  data(Duncan)
  dataset=Duncan[c(2,4)]

  #' The data set in inputs (data) has only 45 rows.
  #' Since this is so few, we will do all-but-one cross-validation.
  #' Split it in 35 rows of training and 10 rows of test data.
  rnd_indices=sample(nrow(dataset))
  train=dataset[rnd_indices[1:35],]
  test=dataset[rnd_indices[36:45],]

  #' Perform all-but-one cross-validation with
  #' an OLS, Poisson regression and 2nd order polynomial
  #' regression model of prestige~income using
  #' the train data in inputs. Record the cross-validated
  #' residuals (not just the mean squared error).
  #'
  #' Return these vectors as ols_res, pois_res and poly_res
  ols_res=sapply(1:nrow(train),function(i) {
      model=lm(prestige~income,train[-i,])
      p=predict(model,train[i,])
      p-train$prestige[i]
  })
  pois_res=sapply(1:nrow(train),function(i) {
    model=glm(prestige~income,"poisson",train[-i,])
    p=predict(model,train[i,],type="response")
    p-train$prestige[i]
  })
  poly_res=sapply(1:nrow(train),function(i) {
    model=lm(prestige~poly(income,2),train[-i,])
    p=predict(model,train[i,])
    p-train$prestige[i]
  })

  #' Find the best and second best algorithms according to MSE
  #' using the cross-validation residuals
  #' (ols_res, pois_res and poly_res). Create a model
  #' of using the best algorithm on the whole train data.
  #'
  #' Then perform a t-test on the squared residuals of the
  #' these two models. Store the p-value as p. Also store the
  #' best model as best.
  #'
  #' You will find the p-value of the t-test in the field $p.value
  #' of the list returned by the t.test function.
  res=list(ols_res,pois_res,poly_res)
  names=c("OLS","Poisson","Polynomial")
  mses=c(
    mean(ols_res^2),
    mean(pois_res^2),
    mean(poly_res^2)
  )
  best_mse=which.min(mses)
  alternative=which(mses==min(mses[-best_mse]))
  worst=setdiff(1:3,c(best_mse,alternative))

  cat("\nThe best algorithm was:",names[best_mse],"with",mses[best_mse],"MSE.")
  cat("\nThe second best algorithm was:",
      names[alternative],"with",mses[alternative],"MSE.")
  cat("\nThe worst algorithm was:",
      names[worst],"with",mses[worst],"MSE.")

  t_res=t.test(res[[best_mse]]^2,res[[alternative]]^2,"less")
  cat("\nThe p-value for the t-test between",names[best_mse],"and",
      names[alternative],"was",t_res$p.value,"\n")

  best=NULL
  if (best_mse==1)
    best=lm(prestige~income,train)  else if (best_mse==2)
    best=lm(prestige~poly(income,2),train)  else
    best=glm(prestige~income,"poisson",train)

  p=t_res$p.value

  #' Now use the best model to predict the prestige values
  #' of the test data. Save the residuals.
  #' Use the residuals to calculate the MSE of this model
  #' on the test data and return this as test_mse.
  #' Also use the squared residuals to estimate 99.5% confidence
  #' intervals for this expected MSE estimate. Return these
  #' as MSE_conf_ints.
  #'
  #' You will find the confidence intervals in the $conf.int
  #' field of the list returned by the t.test function. The
  #' confidence intervals are 95% by default, but you can
  #' change this to 99.5% by passing conf.level=.995 when
  #' you call the t.test function.
  #'
  #' You can test the class of the best model (to see if it is
  #' glm and so needs a special predict function call) by using
  #' if (class(inputs$best)[1]=="glm")
  pr=c()
  if (class(best)[1]=="glm")
    pr=predict(best,test,type="response") else
    pr=predict(best,test)

  res=pr-test$prestige
  best_mse=mean(res^2)
  cat("\nThe test MSE of the best algorithm was:",best_mse)

  t_res=t.test(res^2,conf.level = .995)
  cat("\nThe 99.5% confidence intervals for expected MSE of the best model are:",
      t_res$conf.int,"\n")
}
