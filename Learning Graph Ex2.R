learningGraphExample_2=function(){
  # Prepare data and libraries
  require(stats)
  set.seed(0)
  n <- 500
  x <- runif(n, -2, 12)
  y <- (0.2 * x^4) - (3 * x^3) + (8 * x^2) - (15 * x) +
    10 + rnorm(n, 0, 80)
  train <- data.frame(X=x,Y=y)
  orders=c(1:5,20)

  #' Using the first 10%, 20%, ... 100% of the train data
  #' create in-sample and 10-fold cross-validation MSE
  #' scores as in the last exercise for 6 polynomial
  #' regression models of orders as given by the
  #' order vector in inputs (1,2,3,4,5 and 20).
  #'
  #' Store these as a list of 6 matrices. Each matrix should
  #' be of 2 rows, where the first row gives the in-sample error
  #' and the second row the cross-validation error. Call this list
  #' results.
  #'
  #' Also store the sequence of number of rows in the training set
  #' as dseq.

  get_in_sample=function(train,order) {
    model=lm(Y~poly(X,order),train)
    p=predict(model,train)
    mean((p-train$Y)^2)
  }
  get_out_sample=function(data,order) {
    K=10
    rows=nrow(data)
    step=floor(rows/K)
    tse=sapply(1:K,function(k){
      test_indices=(step*(k-1)+1):(step*k)
      test=data[test_indices,]
      train=data[-test_indices,]
      model=lm(Y~poly(X,order),train)
      p=predict(model,test)
      sum((p-test$Y)^2)
    })
    sum(tse)/rows
  }

  N=nrow(train)
  points=10
  train=train[1:N,]
  step=N/points
  dseq=seq(step,step*points,step)
  # Create models with 50,100,150,...,500
  # You can assume the data is in random order.
  results=lapply(orders,function(order) {
    sapply(dseq,function(n) {
      train=train[1:n,]
      in_sample=get_in_sample(train,order)
      out_sample=get_out_sample(train,order)
      c(in_sample,out_sample)
    })
  })

  #' Plot six plots, one for each result matrix. In each plot
  #' the in-sample (in blue) and cross-validation (in red)
  #' MSEs in the results matrix against the data size in dseq.
  #' Give all plots the same ylim, where this should be .95 of
  #' smallest and 1.05 of the largest value in all the
  #' results matrices.
  #'
  #' In each case, give the X and Y axes the labels "Data" and
  #' "MSE" respectively. Give each graph the title "Order: N", where
  #' N is the order of the polynomial regression model that
  #' generated those results. These are still available as the
  #' vector orders in the inputs.
  #'
  #' To display all six plots, make sure you write all plotting
  #' functions in-between the two par(mfrow=...) calls below.

  all=unlist(results)
  min_y=min(all)*.95
  max_y=min(max(all)*1.05,12000)

  # We allow ourselves to display 6 graphs
  par(mfrow=c(2,3))

  for (i in 1:length(orders)){
    result=results[[i]]
    order=orders[i]
    plot(dseq,result[1,],type="l",col="blue",main=paste("Order: ",order),
         ylim=c(min_y,max_y),xlab="Data",ylab="MSE")
    points(dseq,result[2,],type="l",col="red")
    order=order+1
  }

  # It is good practice to reset the plotting parameters
  par(mfrow=c(1,1))
}
