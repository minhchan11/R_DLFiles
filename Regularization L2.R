regularizationL2Exercise=function(){
  # Prepare data and libraries
  library(stats)
  library(rgl)
  library(nnet)
  library(gdata)
  set.seed(1)
  n=1000
  x1=runif(n,0,10)
  x2=rnorm(n,5,10)
  y=4+x1+x1*x2+.4*x1^2-.1*x2^2+rnorm(n,0,5)
  dataset=data.frame(x1=x1,x2=x2,y=y)

  #' Randomize the data order and split it into 3 sets of 60%,20%,20%.
  #' Return these as train, valid and test
  rnd_indices=sample(nrow(dataset))
  train=dataset[1:(floor(length(rnd_indices)*.6)),]
  valid=dataset[(1+floor(length(rnd_indices)*.6)):(floor(length(rnd_indices)*.8)),]
  test=dataset[(1+floor(length(rnd_indices)*.8)):length(rnd_indices),]

  #' Create a vector lambdas that has the values: 0,.0001,.001,.01,.1,1,2.
  lambdas=c(0,.0001,.001,.01,.1,1,2)

  #' Create a series of neural net models y~. of size 4
  #' with L2 regularization penalties given by the entries
  #' in the lambdas vector. Set maxit to 1000 for all models.
  #' Store these models as an unnamed list called models.
  models=lapply(lambdas,function(lambda){
    nnet(y~.,train,size=4,decay=lambda,maxit=1000,linout=T)
  })

  #' 1. Evaluate each model on the validation data using MSE.
  #'    Store the residuals when you are doing this for use in
  #'    (4).
  #' 2. Choose the best model based on (1)
  #' 3. Evaluate the best model on the test data using MSE
  #' 4. Check if we can be confident that the expected MSE of the
  #'    chosen model is less that a model with no regularization
  #'    penalty. Report the p-value of this test.
  #' 5. Store:
  #'    - The best model as model
  #'    - The validation MSE for the best model as valid_mse
  #'    - The test MSE for the best model as test_mse
  #'    - The lambda value of the best model
  #'    - The p-value from (4) as pVal
  res=lapply(models,function(model){
    p=predict(model,valid)
    p-valid$y
  })
  mses=sapply(res,function(r)mean(r^2))

  best=which.min(mses)
  test_p=predict(models[[best]],test)
  test_mse=mean((test_p-test$y)^2)
  pVal=t.test(res[[best]]^2,res[[1]]^2,"less")$p.value

  cat("\nThe best model was with lambda",lambdas[best],
      "and had",mses[best],"validation MSE and",test_mse,
      "test MSE. The p-value from a t-test comparing its",
      "performance to a model with no L2 regularization was",pVal,"\n")

  model=models[[best]]
  valid_mse=mses[best]
  lambda=lambdas[best]

  #' Plot:
  #'  - The data dataset in 3d. Label your axes "X1", "X2" and "Y".
  #'    Label the plot "3D Neural Net: Weight Decay of L", where
  #'    L is inputs$lambda
  #'  - The regression surface of the best model in blue
  #'    with alpha .7, and its residuals (in red). The surface
  #'    should go from the minimum to the maximum values in the
  #'    x (x1) and y (x2) dimensions. The surface grid should
  #'    be from minimum to maximum data values in 100 even steps.
  #'
  #' Hint 1: The surface3d function requires a vector of x points,
  #' a vector of y points and a matrix of z (height) points. To find
  #' the heights you need to create a data frame from the x and y
  #' vectors that contains *all their combinations*. The rep function
  #' is helpful here. Consider the following:
  #' xSeq=1:3        # [1,2,3]
  #' ySeq=0:4        # [0,1,2,3,4]
  #' l1=length(xSeq) # 3
  #' l2=length(ySeq) # 5
  #' x_=rep(xSeq,each=l2) # [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3]
  #' y_=rep(ySeq,l1)      # [0,1,2,3,4,0,1,2,3,4,0,1,2,3,4]
  #' z=predict(inputs$model,data.frame(x1=x_,x2=y_))
  #' grid=matrix(z,ncol=l2,byrow=T)
  #' surface3d(xSeq,ySeq,grid,alpha=.7,col="blue")
  #'
  #' Hint 2: segments3d requires a matrix where the lines are read
  #' of as being:
  #'     Line    Point From     Point To
  #'      1         row 1         row 2
  #'      2         row 3         row 4
  #'     ...         ...           ...
  #'      n       row 2n-1        row 2n
  #' The interleave function in gdata (already loaded) combines
  #' matrices A and B row-wise, such that the result is:
  #'   Row 1 A
  #'   Row 1 B
  #'   Row 2 A
  #'   Row 2 B
  #'    ...
  #' This can be very helpful in creating the matrix needed for
  #' the segments3d function. Note that interleave requires the
  #' columns of the passed matrices/data frames have the same
  #' names.
  plot3d(dataset,xlab="X1",ylab="X2",zlab="Y",
         main=paste("3D Neural Net: Weight Decay of",lambda))

  xSeq=seq(min(dataset$x1),max(dataset$x1),
           (max(dataset$x1)-min(dataset$x1))/100)
  ySeq=seq(min(dataset$x2),max(dataset$x2),
           (max(dataset$x2)-min(dataset$x2))/100)
  l1=length(xSeq)
  l2=length(ySeq)
  x_=rep(xSeq,each=l2)
  y_=rep(ySeq,l1)
  z=predict(model,data.frame(x1=x_,x2=y_))
  grid=matrix(z,ncol=l2,byrow=T)
  surface3d(xSeq,ySeq,grid,alpha=.7,col="blue")

  data2=dataset
  data2$y=predict(model,dataset)
  pnts_matrix=interleave(dataset,data2)
  segments3d(pnts_matrix,col="red")
}
