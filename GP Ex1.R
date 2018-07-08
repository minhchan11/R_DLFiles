#' To allow interested students to get a grip of the
#' mathematics, we implement a Gaussian process class.
#' The functions arguments are:
#'
#'    Function: gp
#'    Arguments: formula, data, kernel function, l2 penalty
#'    Returns: Gaussian process model
#'    Discussion: Constructor
#'
#'    Function: predict (predict.gp)
#'    Argument: model, new data
#'    Returns: mean and variance for output for each row in new data
#'    Discussion: Normal predict function
#'
#'    Function: generate
#'    Argument: model, feature points
#'    Returns: value for each row in new data
#'    Discussion: Samples a function from the Gaussian process,
#'                returning values of the function at each specified
#'                point.
#'
#' You will find them below the example function in this
#' script file. Make sure to source the file in order
#' to use the GP class.
#'
#' Less interested students can ignore the implementation
#' and use the GP like any third-party implementation.
gaussianProcessesExample=function(){
  #' Prepare data and libraries
  library(stats)
  n=20
  x = c( runif(ceiling(n/2), -5, 3), runif(floor(n/2), 7, 14))
  y = ((0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) +
    10 + rnorm(n, 0, 80))/100
  dataset=data.frame(X=x,Y=y)

  #' First, split the data into training and test in the ratio 3:1
  randomIndices=sample(nrow(dataset))
  n=length(randomIndices)
  train=dataset[randomIndices[1:(.75*n)],]
  test=dataset[randomIndices[((.75*n)+1):n],]

  #' Build a GP model from the training data.
  #' To do so, you need a kernel and a L2 regularization penalty
  #' (lambda value).
  #'
  #' Here is a simple Laplacian kernel using the l2 norm
  #' (i.e. Euclidian distance) and sigma=2.
  k=function(X1,X2) {
    exp(-sqrt((sum((X1-X2)^2)/2)))
  }
  #' Here is an arbitrary L2 regularization penalty.
  lambda=.5
  #' In real projects we would spend time tuning the hyper-parameters,
  #' but here we will be happy to just investigate the model generated
  #' by this particular kernel/lambda pair.

  #' Create the model
  model=gp(Y~X,train,k,lambda)

  #' Calculate the MSE of the model for the test data,
  #' using the mean values are the regression curve.
  pre=predict(model,test)
  mse=mean((pre[1,]-test$Y)^2)
  #' Calculate the log likelihood of the model given the
  #' test data (ie the log probability of the test data given the
  #' model). This is useful since the log probability
  #' of validation/test data is also often used to evalute models. So
  #' is the log likelihood of the model given the training data,
  #' through penalized maximum likelihood methods such as the AIC or BIC.
  logProb=sum(sapply(1:ncol(pre),function(i){
    dnorm(test$Y[i],pre[1,i],pre[2,i],T)
  }))
  #' Output these values to the console
  cat("The MSE of the GP model on the test data was: ",mse,"\n")
  cat("The likelihood of the GP model on the test data was: ",logProb,"\n")

  #' For comparison, make an OLS model.
  #' To get probabilities, we will also train an error distribution
  #' on the training data. (This will be optimistic, since it is done
  #' on the training data, leading to worse performance when evaluated
  #' using test data likelihood)
  ols_model=lm(Y~X,train)
  #' Find the residuals for the training data in the model,
  #' and use them to calculate the variance of the error distribution.
  ols_errVar=sum(ols_model$residuals^2)/(nrow(train)-1)
  #' Calculate the MSE of the ols model for the test data.
  pre=predict(ols_model,test)
  ols_mse=mean((pre-test$Y)^2)
  #' And the likelihood of the model given the test data
  ols_logProb=sum(sapply(1:nrow(test),function(i){
    dnorm(test$Y[i],pre[i],ols_errVar,T)
  }))
  #' Output these values to the console
  cat("The MSE of the OLS model on the test data was: ",ols_mse,"\n")
  cat("The likelihood of the OLS model on the test data was: ",ols_logProb,"\n")

  #' Now we will make three plots
  par(mfrow=c(3,1))

  #' In the first, plot the regression curve from -6 to 15
  #' in blue. Also plot confidence intervals using the variance outputs
  #' of the GP over the same range in red. Make them at 2 standard
  #' deviations. Make the vertical limits -5 to 7.
  plot(Y~X,train,ylim=c(-5,7))
  points(test$X,test$Y,col="red")
  xseq=seq(-6,15,.05)
  pre=predict(model,data.frame(X=xseq))
  points(xseq,pre[1,],type="l",col="blue")
  points(xseq,pre[1,]+2*sqrt(pre[2,]),type="l",col="red")
  points(xseq,pre[1,]-2*sqrt(pre[2,]),type="l",col="red")

  #' In the second, do the same for the OLS model
  plot(Y~X,train,ylim=c(-5,7))
  points(test$X,test$Y,col="red")
  pre2=predict(ols_model,data.frame(X=xseq))
  points(xseq,pre2,type="l",col="blue")
  points(xseq,pre2+2*sqrt(ols_errVar),type="l",col="red")
  points(xseq,pre2-2*sqrt(ols_errVar),type="l",col="red")

  #' In the third, redo the first plot, but then add 3
  #' generated functions to it in black, gray and green.
  plot(Y~X,train,ylim=c(-5,7))
  points(test$X,test$Y,col="red")
  points(xseq,pre[1,],type="l",col="blue")
  points(xseq,pre[1,]+2*sqrt(pre[2,]),type="l",col="red")
  points(xseq,pre[1,]-2*sqrt(pre[2,]),type="l",col="red")
  # Here are the three generated functions
  xseq=seq(-6,15,.1) # Reduce the number of points used for speed
  xSeqDF=data.frame(X=xseq)
  points(xseq,generate(model,xSeqDF),type="l")
  points(xseq,generate(model,xSeqDF),type="l",col="gray")
  points(xseq,generate(model,xSeqDF),type="l",col="green")

  # It is good practice to reset the plotting parameters
  par(mfrow=c(1,1))
}


#-----------------------------
# START OF GP CLASS DEFINITION
#-----------------------------
#' If you do go through these functions, make sure you
#' have a good grasp of the math from the Gaussian process
#' article - the math here appear intimidating unless you
#' have a good idea what is going on. :)
#'
#' Again, we keep it basic, assuming good inputs, and a scaler
#' output. The mean of the process will be 0, and for this
#' to be reasonable, we center the target variable inside
#' the model, and decenter resulting outputs from the model.
#'
#' (These functions assume scaler output, but should work with
#' multiple dimensional input, though they have not been tested!)
gp=function(formula,data,kernel,lambda) {

  #' We use the formula to divide the data into input feature(s)
  #' and target variable(s).
  features=data[all.vars(formula[[3]])]
  targets=data[all.vars(formula[[2]])]

  # We are going to build in a centering of the target
  targetOffset=mean(targets[,1])
  targets=targets-targetOffset

  #' Then we create the gram  matrix.
  gram=apply(features,1,function(row1) {
    apply(features,1,function(row2) {
      kernel(row1,row2)
    })
  })

  #' Now we add the lambda value to the diagonal
  #' to get the covariance matrix
  covariance=gram+diag(lambda,nrow(gram))

  #' We store everything we need in a list, which we will return.
  #' For a minimalist implementation, we just need the feature
  #' data, the dual coefficients, the kernel function,
  #' and the feature names.
  out=list(
    features=features,
    targets=targets,
    covariance=covariance,
    featureNames=all.vars(formula[[3]]),
    kernel=kernel,
    lambda=lambda,
    targetOffset=targetOffset
  )

  #' And give it the class name 'gp'
  class(out)="gp"

  #' And return it
  return (out)
}
#' Now do the predict function for the class
predict.gp=function(model,data) {
  # We extract the features from the new data
  input=data[model$featureNames]

  #' And calculate the covariance inverse
  invSigma22=solve(model$covariance)

  # We want to do regression on all rows in data, so use apply
  out=apply(input,1,function(datum) {
    # Assume:
    # sigma = S22 S21   mean = M2 M1
    #         S12 S11
    # And we want P(X1|X2=v)
    sigma21=apply(model$features,1,function(other) {
      model$kernel(datum,other)
    })
    sigma12=t(sigma21)
    sigma11=model$kernel(datum,datum)+model$lambda
    #' Calculate the conditional distribution mean & covariance
    #' Full equations:
    #'   mu=mu1+sigma12%*%invSigma22%*%(datum-mu2)
    #'   sigma=sigma11-sigma12%*%invSigma22%*%sigma21
    #' But we can simplify mu equation since means are 0
    mu=sigma12%*%invSigma22%*%model$targets[,1]
    sigma=sigma11-sigma12%*%invSigma22%*%sigma21

    # Return mean and variance
    c(mu,sigma)
  })

  #' Finally we decenter the output
  out[1,]=out[1,]+model$targetOffset

  # And return the results
  return (out)
}
#' We will also make a (programmatic) function that will generate
#' (mathematical) functions from a Gaussian process model
generate=function(model,featurePnts) {
  # Ignore irrelevant columns, if any
  featurePnts=featurePnts[model$featureNames]

  # Prepare output
  out=NULL
  for (i in 1:nrow(featurePnts)) {
    # Find next value (see predict function for notes)
    invSigma22=solve(model$covariance)
    sigma21=apply(model$features,1,function(other) {
      model$kernel(featurePnts[i,],other)
    })
    sigma12=t(sigma21)
    sigma11=model$kernel(featurePnts[i,],featurePnts[i,])+model$lambda
    mu=sigma12%*%invSigma22%*%model$targets[,1]
    sigma=sigma11-sigma12%*%invSigma22%*%sigma21
    newPnt=rnorm(1,mu,sigma)
    out=c(out,newPnt)
    # Update model
    model$features=rbind(model$features,featurePnts[i,])
    model$targets=rbind(model$targets,data.frame(Y=newPnt))
    model$covariance=rbind(
      cbind(model$covariance,sigma21),
      cbind(sigma12,sigma11)
    )
  }

  # We decenter output
  out=out+model$targetOffset

  # And return it
  return (out)
}
#----------------------------
# END OF GP CLASS DEFINITION
#----------------------------
