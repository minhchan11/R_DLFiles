#' To allow interested students to get a grip of the
#' mathematics, we implement a kernel regression class.
#' The functions arguments are:
#'
#'    Function: kernReg
#'    Arguments: formula, data, kernel function, l2 penalty
#'    Returns: kernel regression model
#'    Discussion: Constructor
#'
#'    Function: predict (predict.kernReg)
#'    Argument: model, new data
#'    Returns: target variable estimate for each row in new data
#'    Discussion: Normal predict function
#'
#' You will find them below the example function in this
#' script file. Make sure to source the file in order
#' to use the GP class.
#'
#' Less interested students can ignore the implementation
#' and use the GP like any third-party implementation.
kernelRegressionExample=function(){
  #' Prepare data and libraries
  n=160
  x = runif(n, -2, 12)
  y = (0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) +
    10 + rnorm(n, 0, 80)
  dataset=data.frame(X=x,Y=y)

  #' Now we are ready to work with the data
  #' First, split the data into training and test in the ratio 3:1
  randomIndices=sample(nrow(dataset))
  n=length(randomIndices)
  train=dataset[randomIndices[1:(.75*n)],]
  test=dataset[randomIndices[((.75*n)+1):n],]

  #' To use our kernel regression class, we need to decide on a kernel
  #' to use. Here is a simple Laplacian kernel using the l2 norm
  #' (i.e. Euclidian distance) and sigma=1.
  k=function(X1,X2) {
    exp(-sqrt(sum((X1-X2)^2)))
  }

  #' We must also decide on a L2 regularization penalty.
  lambda=.01

  #' Now we train the model using the training data:
  krModel=kernReg(Y~X,train,k,lambda)

  #' Let's also train a OLS model for comparison
  olsModel=lm(Y~X,train)

  #' Evaluate their performance on the test data:
  kr_mse=mean((predict(krModel,test)-test$Y)^2)
  ols_mse=mean((predict(olsModel,test)-test$Y)^2)

  #' Output these results
  cat("Kernel regression MSE: ",kr_mse,"\n")
  cat("OLS MSE: ",ols_mse,"\n")

  #' Now plot the data and the regression lines. Do the kernel
  #' regression in blue, and the OLS regression in green.
  #' Plot the functions from -3 to 13.
  plot(Y~X,dataset)
  f_kr=function(x) predict(krModel,data.frame(X=x))
  f_ols=function(x) predict(olsModel,data.frame(X=x))
  plot(f_kr,from=-3,to=13,add=T,col="blue")
  plot(f_ols,from=-3,to=13,add=T,col="green")

  #' If you look at this model, you will see it has overfitted.
  #' (Typically it does better than the OLS on MSE, but not always).
  #' Let's try again with a wider kernel (we will divide the exponent
  #' by 4, effectively reducing the distance between points)
  #' and a larger lambda:
  k2=function(X1,X2) {
    exp(-sqrt(sum(((X1-X2)^2)/4)))
  }
  lambda2=2
  krModel2=kernReg(Y~X,train,k2,lambda2)
  kr_mse2=mean((predict(krModel2,test)-test$Y)^2)
  cat("Kernel regression 2 MSE: ",kr_mse2,"\n")
  f_kr2=function(x) predict(krModel2,data.frame(X=x))
  plot(f_kr2,from=-3,to=13,add=T,col="red")
  #' It still looks a little overfitted, so we could keep tuning.
  #' But that's good enough for this exercise :)
}

#-----------------------------
# START OF KR CLASS DEFINITION
#-----------------------------
#' Here we implement a kernel regression model class.
#' To keep it simple we assume proper inputs through out
#' so as to avoid lots of checks.
#'
#' If you are interested in S3 object oriented programming, or the
#' math of kernel regression, this should be interesting. If you
#' are not, feel free to just skip over the class function
#' definitions are use them like any other third-party
#' implementation.

#' First do the constructor
kernReg=function(formula,data,kernel,lambda) {
  #' We use the formula to divide the data into input feature(s)
  #' and target variable(s).
  features=data[all.vars(formula[[3]])]
  targets=data[all.vars(formula[[2]])]

  #' Then we create the 'gram' matrix.
  gram=apply(features,1,function(row1) {
    apply(features,1,function(row2) {
      kernel(row1,row2)
    })
  })

  #' Now we calculate the dual coefficients
  A=solve(gram+diag(lambda,nrow(gram)))%*%as.matrix(targets)

  #' We store everything we need in a list, which we will return.
  #' For a minimalist implementation, we just need the feature
  #' data, the dual coefficients, the kernel function,
  #' and the feature names.
  out=list(
    features=features,
    A=A,
    featureNames=all.vars(formula[[3]]),
    kernel=kernel
  )

  #' And give it the class name 'kernReg'
  class(out)="kernReg"

  #' And return it
  return (out)
}

#' Now do the predict function for the class
predict.kernReg=function(model,data) {
  # We extract the features from the new data
  input=data[model$featureNames]

  # We want to do regression on all rows in data, so use apply
  apply(input,1,function(datum) {
    # This does the regression for each row
    sum (sapply(1:nrow(model$features),function(i){
      model$A[i]*model$kernel(datum,model$features[i,])
    }))
  })
}
#-----------------------------
# END OF KR CLASS DEFINITION
#-----------------------------
