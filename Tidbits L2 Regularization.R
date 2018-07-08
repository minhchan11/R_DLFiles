#' Polynomial regression allows a very interpretable example of
#' regularization. We have created our own basic L2 regularized
#' polynomial regression class, called basicPR. The code is commented
#' and allows those who are interested to examine it to get an idea
#' how simple (S3) object oriented programming works in R.
#' You will learn, for example:
#'   - What S3 classes 'are' and how to make them
#'   - How predict and other generic functions know the code to execute      when called with a particular class of statistical model.
#' The code is provided at the bottom of this file, and you will need to
#' load the functions into your environment to run the command below.
#' This can be done by highlighting it and clicking run in RStudio.
#'
#' Now let's look at the effect of regularization... we will explain
#' the graphs that appear.

l2regDemo()

#' First picture:
#' As you see, we have a high (sixth) order polynomial being fit to a
#' very small number of data points. The left half of the plot looks
#' particularly overfit.
#'
#' Second picture:
#' One way to address this overfitting is to lower the order of the
#' polynomial we use. Here is a fourth order polynomial, and we see
#' that the overfitting on the left is much reduced.
#' We have achieved this by reducing the number of parameters in the
#' model, from 7 to 5. If we think of the projections (X, X^2, X^3,...)
#' as features, we have taken a subset of them by throwing away the
#' last two.
#'
#' Third picture:
#' But another way of addressing this overfitting is to use
#' regularization. Here we impose an L2 regularization penalty.
#' So the model is no longer just seeking to find parameters that
#' minimize training MSE. Instead it is seeking to balance this
#' with a penalty on the sum of squared parameters. This contrains
#' the ability of the parameters to freely take on values. The result,
#' we see, is a smoothing of the overfit section, though it still
#' appears (by superfiscial visual analysis) to be a case of overfitting.
#'
#' Fourth picture
#' We now see what happens when we increase the regularization penalty.
#' The left side becomes much smoother, and this looks a much better model.
#'
#' Fifth picture
#' Here we see what happens when we massively increase the regularization
#' penalty. The left side goes even smoother. Note that there has not
#' been visable deterioration of the right side.
#'
#' Sixth picture
#' In fact, to really damage the performance of the model we need to
#' increase the regularization penalty to huge levels. But it can be done!
#'
#' We can look at the coefficients for these 6 models:
#' 6,0      4,0    6,.1     6,1   6,1000    6,1e10
#' ----------------------------------------------------------
#' Inter   -227.8    -58.2   107.0   -18.2    .0065 -9.05e-08
#' X        -16.7   -27.64   -22.9   -20.4   -.0077 -6.71e-07
#' X^2      131.1    -34.0    64.7    15.1      .19 -4.88e-06
#' X^3      -51.0     -7.7   -21.5    -.84      .24 -3.45e-05
#' X^4        7.7      .43    26.9    -.69     -.24 -2.21e-04
#' X^5       -.54            -.016    .086     .013 -1.08e-03
#' X^6       .015            .0043  -.0025   .00042 -5.69e-06
#' ----------------------------------------------------------
#' Ab.Sum   434.8    128.0   218.9    55.3      .69     .0014
#'
#' As expected, as the lambda tuning parameter increases, the parameters
#' decrease. But note they do not do so regularly: While the sum of the
#' absolute value goes down monotonically with an increase in lambda,
#' some individual parameters increase their absolute value as lambda
#' increases. Note also that some parameters pass through 0 and out the
#' other side.
#'
#' Notice that we are also shriking the intercept. This is seldom helpful,
#' so it can be better to center the data before using regularization
#' with models that contain an intercept parameter.



#' -----------------------------------
#' Simple polynomial regression class
#' -----------------------------------
#'
#' We start with a constructor for our class.
#' We want it to take the standard model constructor
#' arguments: A formula, a data frame, and hyper-parameters.
#' The hyper-parameters in this case are the order of the
#' polynomial regression and the L2 regularization tuning
#' parameter.
#' Our contructor calls a number of helper functions, which we
#' will fill out below.
#' Notice that we return a simple list, but one where
#' we have set the class attribute to a special value:
#' 'basicPR'.
basicPR=function(form,df,order,lambda) {
  # Make sure R knows form is a formula
  form=as.formula(form)
  # Extract variable indices from formula and df
  indices=getColumns(form,df)
  # Split and project data
  X=splitAndProject(df,indices$features,order)
  Y=df[[indices$target]]
  # Numeric sanity check
  numericCheck(X,Y)
  # Calculate coefficients
  coef=solve(crossprod(X)+diag(lambda,ncol(X)))%*%crossprod(X,Y)
  # Create and return basicPR object
  out=list(form=form,coef=coef,order=order,
           train=df[c(indices$features,indices$target)])
  class(out)="basicPR"
  return (out)
}
#' Here are the helper functions:
#' The first finds the column index for a given name.
getColumn=function(name,df,throwOnAbsent) {
  i=which(colnames(df)==name)
  if (length(i)==0) {
    if (throwOnAbsent)
      stop(paste("Could not find",name,"in data."))
    else
      return(NA)
  }
  else if (length(i)>1)
    stop(paste("Multiple columns match",name,"in data."))
  return (i)
}
#' The second finds column indices for all variables in
#' a formula. It distinguishes between the
#' inputs/features/independent variables and the
#' target/dependent variable.
getColumns=function(form,df,getTarget) {
  target=NA                 # For output
  features=c()              # For output
  form=as.formula(form)     # Make R treat form as formula
  vars=all.vars(form)       # Extract variables
  target=getColumn(vars[1],df,getTarget)   # Find target index
  vars=vars[2:length(vars)]
  if ('.' %in% vars) {
    if (is.na(target))
      features=(1:ncol(df))
    else
      features=(1:ncol(df))[-target] # All others are features
  }
  else {
    features=sapply(vars,getColumn,df,T) # Find column indices
  }
  list(target=target,features=features) # Return target and features
}
#' This function extracts the features (using their column
#' indices) from a data frame, and then projects them onto
#' their polynomials up to the specified order.
#' Note that we do not calculate cross-products. Also, we
#' do 'raw' projection. This is where the 'basic' comes into
#' our basicPR model. The latter will mean that we have trouble
#' dealing with data with anything but small feature values.
#'
splitAndProject=function(df,features,order) {
  features_=rep(features,each=order)
  orders_=rep(1:order,length(features))
  features_=c(-1,features_)
  orders_=c(-1,orders_)
  apply(cbind(features_,orders_),1,function(row){
    if (row[1]==-1)
      rep(1,nrow(df))
    else
      df[,row[1]]^row[2]
  })
}
#' Another simplification with our models is that they
#' only accept numeric data (no factors).
#' Here We check that we are dealing with numeric data.
numericCheck=function(X,Y=NULL){
  if (length(X)==0)
    stop("Data is empty.")
  else if (class(X[1])!="numeric" && class(X[1])!="integer" )
    stop("basicPR requires numeric features.")
  if (!is.null(Y)) {
    if (nrow(X)!=length(Y))
      stop("Data length mismatch.")
    else if (class(Y)!="numeric" && class(Y)!="integer" )
      stop("basicPR requires a numeric target.")
  }
}
#' Here is the predict function for our class. Note that it is
#' just predict, dot, our class name. When you call predict
#' a generic function looks at the class of the first argument
#' and then redirects it to the appropriate class predict function
#' using this schema.
predict.basicPR=function(model,df) {
  # Extract variable indices from formula and df
  indices=getColumns(model$form,df,F)
  # Split and project data
  X=splitAndProject(df,indices$features,model$order)
  numericCheck(X)
  # Calculate and return estimates
  X%*%model$coef
}
#' For fun we also provide a plot function
plot.basicPR=function(model,xlim=NULL,ylim=NULL,...){
  if (ncol(model$train)!=2)
    stop("Can only plot 2d.")

  f=function(x) {
    df=data.frame(x)
    colnames(df)=colnames()[1:(ncol(model$train)-1)]
    predict(model,df)
  }

  xmin=min(model$train[,1])
  xmax=max(model$train[,1])
  if (!is.null(xlim)) {
    xmin=xlim[1]
    xmax=xlim[2]
  }
  xseq=seq(min(model$train[,1]),max(model$train[,1]),
           (max(model$train[,1])-min(model$train[,1]))/100)
  df=data.frame(xseq)
  colnames(df)=colnames(model$train)[1]
  p=predict(model,df)

  if (is.null(ylim)) {
    ymin=min(c(model$train[,2],p))
    ymax=max(c(model$train[,2],p))
    ylim=c(ymin,ymax)
  }

  plot(model$form,model$train,xlim=xlim,ylim=ylim,...)
  points(xseq,p,type="l",col="blue")
  est=predict(model,model$train)
  segments(model$train[,1],model$train[,2],model$train[,1],est,
           col="red")
}
#' ... and a summary function
summary.basicPR=function(model) {
  cat("\n\nBasic Polynomial Regression Model")
  cat("\n\nFormula:",model$form)
  cat("\n\nCoefficients:",model$coef,"\n")
}

#' To allow us to play around with these models
#' we provide some more functions.
#' First, we provide a synthetic data generator.
getPoly4=function(n=100,spread=1,seed=NA){
  require(stats)
  if (is.na(seed))
    seed=sample(10000,1)
  set.seed(seed)
  x = runif(n, -2, 12)*spread
  y = (0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) +
    10 + rnorm(n, 0, 80)
  data.frame(X=x,Y=y)
}
#' We also provide a means of quickly getting
#' synthetic data, making a model from it, and
#' plotting the results.
l2regDemoCase=function(n,order,lambda,spread=1,seed=NA) {
  poly4=getPoly4(n,spread,seed)
  model=basicPR(Y~X,poly4,order,lambda)
  plot(model,main=paste("Order =",order,"\nLambda =",lambda))
}
#' Finally we create a function that shows some interesting
#' cases, designed to illustrate the effect of L2
#' regularization.
l2regDemo=function() {
  l2regDemoCase(11,6,0,1,6)
  readline("Press Enter to Continue...")
  l2regDemoCase(11,4,0,1,6)
  readline("Press Enter to Continue...")
  l2regDemoCase(11,6,.1,1,6)
  readline("Press Enter to Continue...")
  l2regDemoCase(11,6,1,1,6)
  readline("Press Enter to Continue...")
  l2regDemoCase(11,6,1000,1,6)
  readline("Press Enter to Continue...")
  l2regDemoCase(11,6,10000000000,1,6)
}
