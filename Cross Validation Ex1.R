#' What are hyper-parameters?
#'
#' Hyper-parameters are parameters of the model building algorithm.
#' Parameters are parameters of the model.
#'
#' Examples so far:
#'   - Order of polynomial regression.
#'   - Number of hidden nodes in neural network
#'
#' How can we work out good values for hyper-parameters?
#' Build models with different values, and evaluate the
#' resulting models!
#'
#' In this example we do this with polynomial regression: We
#' built a number of different order polynomial regression models
#' and evaluated the models on the validation data to see
#' which performed best.

crossValidationExample = function () {
  # Prepare libaries and data
  require(stats)
  set.seed(1)
  n <- 10000
  x <- runif(n, -2, 12)
  y <- (0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) +
    10 + rnorm(n, 0, 80)
  dataset <- data.frame(X=x,Y=y)

  # Take the dataset and:
  #   1. Randomize the order of the rows
  #   2. Divide the data into two:
  #      - A training set with 8000 rows
  #      - A test set with 2000 rows
  rand_indices=sample(nrow(dataset))
  train=dataset[rand_indices[1:8000],]
  test=dataset[rand_indices[8001:10000],]

  # We are going to evaluate using polynomial regression
  # models of order 3,4,5 and 6 using 10-fold cross-validation.
  # We will proceed by creating 10 models of each order.
  # The first will be trained on rows 801:8000 of the train data
  # and tested on rows 1:800. The second trained on
  # rows 1:800,1601:8000 and tested on rows 801:1600, and so
  # on until the last which will be trained on rows 1:7200 and tested
  # on rows 7201:8000.
  # We will sum the squared error of models of each order over all
  # 10 of the test sets to find which order performs best.
  tses_=sapply(3:6,function(order) {
    firsts=(0:9*800)+1
    lasts=1:10*800
    sapply(1:10,function(i) {
      train_=train[-(firsts[i]:lasts[i]),]
      test_=train[firsts[i]:lasts[i],]
      model=lm(Y~poly(X,order),train_)
      p=predict(model,test_)
      sum((p-test_$Y)^2)
    })
  })
  tses=colSums(tses_)

  # Find which order got the lowest total squared error
  best=(3:6)[which.min(tses)]

  # Now create a model of that order using the whole training data
  model=lm(Y~poly(X,best),train)

  # Now calculate its MEAN square error
  # as mse (divide the total square error by 8000)
  mse=tses[which.min(tses)]/8000

  # Use model to estimate Y values for test
  p=predict(model,test)
  # With the results calculate MSE on the test set and the
  # standard deviation of the residuals on the test data
  mse=mean((p-test$Y)^2)
  sd=sd(p-test$Y)
  # Here you would check that the mse on the test data is similar
  # to the cross-validated mse. In this case it will be: We are
  # not selecting a model that scored well merely by luck.

  # Plot Y~X using inputs$data using smoothScatter.
  # Give it the title "Smooth Scatter", and make sure the axes are
  # labelled X and Y
  smoothScatter(dataset,main="Smooth Scatter")
  # Now plot the regression curve and +/- 2 test residual
  # standard deviations (inputs$sd) upper at lower confidence
  # intervals for inputs$model. Plot the regression curve in black
  # and the confidence intervals in red.
  # Plot all these functions from -2 to 12
  xseq=seq(-2,12,.1)
  p=predict(model,data.frame(X=xseq))
  upper=p+2*sd
  lower=p-2*sd
  points(xseq,p,type="l",col="black")
  points(xseq,upper,type="l",col="red")
  points(xseq,lower,type="l",col="red")
}

AIC_BIC_Example = function () {

}
