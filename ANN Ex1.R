#' We have already worked with some simple neural networks in previous
#' exercies. So here we add some error bars/confidence intervals to
#' a basic regression neural network.
neuralNetworksExample_1=function() {
  # Prepare data and libraries
  library(datasets)
  library(nnet)
  library(graphics)
  n=6000
  x = runif(n, -2, 10)
  y = (0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) +
    10 + rnorm(n, 0, 80)
  dataset=data.frame(X=x,Y=y)

  #' Split the data into 65% training and 35% test
  rndIndices=sample(nrow(dataset))
  sepPnt=round(.65*nrow(dataset))
  train=dataset[rndIndices[1:sepPnt],]
  test=dataset[rndIndices[(sepPnt+1):length(rndIndices)],]

  #' Create an neural network model with 3 hidden
  #' nodes of Y~X using the training data.
  #' Store this model as 'model'
  model=nnet(Y~X,train,size=3,linout=T)

  #' Use this model to estimate the Y values of the test data
  pre=predict(model,test)

  #' Calculate the MSE of the model on the test data and output
  #' it using the print or cat functions
  mse=mean((test$Y-pre)^2)
  cat("The MSE of the model on the test data is: ",mse,"\n")

  # Now calculate the standard devition of the residuals, where
  # these Are the predicted values minus the actual values.
  # You can use the buily in sd function.
  resSD=sd(test$Y-pre)

  #' Now we plot the regression curve with basic error bars/confidence
  #' intervals. We will look at two methods for doing the error bars.
  #' In this process, we assume:
  #' - Model error is normally distributed with mean = 0.
  #' - Model error is independent of input values.

  #' Plot Y vs X from data
  #' Give the axes the names of the variables, and the main
  #' title "Neural Network with Basic Error Bars"
  plot(Y~X,dataset,main="Neural Network with Basic Error Bars")

  #' Now we will plot the regression curve and error bars.
  #' First, create a sequence of values from the minimum X value minus 1
  #' to the maximum X value plus 1, with step .1.
  xseq=seq(min(dataset$X)-1,max(dataset$X)+1,.1)

  # Estimate Y values for X values in xseq
  f=function(x)
    predict(model,data.frame(X=x))
  regCurve=f(xseq)

  # Use points to plot the regression curve using xseq as
  # the x values and your Y estimates as the y values
  # Make the curve blue.
  points(xseq,regCurve,type="l",col="blue")

  #' We will now calculate and plot error bars/confidence intervals.

  #' For the first method, create vectors called upper and lower
  #' by adding and subtracting 2*inputs$sd from the estimated values
  upper=regCurve+2*resSD
  lower=regCurve-2*resSD

  # Plot the upper (and then lower) approximately 95% confidence
  # intervals using points, with xseq as the x values and upper
  # (then lower) as the y values. Make these curves red.
  points(xseq,upper,type="l",col="red")
  points(xseq,lower,type="l",col="red")

  # It is easy to use this method to get confidence
  # intervals that are approximately .68, .95 and .995.
  # They correspond to adding/subtracting 1, 2 and 3 standard
  # deviations. (For more precision, 1,2 and 3 std dev gives us
  # confidence intervals approximately .6827, .9545, and .9973)

  #' We now look at the second method. This allows precise calculations
  #' any confidence level. (The calculations are 'precise' given
  #' the estimates of standard deviation.)

  #' Using the second method, for a confidence interval of n% with
  #' a std deviation of s, we calculate the sum to be added and removed
  #' from our estimates using qnorm(1-(1-n)/2,0,s)

  #' So let's make 90% confidence intervals using this second method
  #' and plot them in green.
  dif=qnorm(.95,0,resSD)
  upper2=regCurve+dif
  lower2=regCurve-dif
  points(xseq,upper2,type="l",col="green")
  points(xseq,lower2,type="l",col="green")
}
