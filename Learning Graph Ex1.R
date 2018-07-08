learningGraphExample_1=function(){
  # Prepare data and libraries
  require(utils)
  require(mlbench)
  data(BostonHousing2)
  dataset=BostonHousing2[-(1:5)]
  # We pretend the data is a training set
  train=dataset[sample(nrow(dataset)),]

  #' Using the first 10%, 20%, ... 100% of the (training) data:
  #'  1- Create a OLS model from all this n% of the data and
  #'     evaluate its in-sample MSE (the MSE on the same data
  #'     it was trained with).
  #'  2- Get the 10-fold cross-validation MSE for the OLS
  #'     algorithm with this n% of the train data.
  #' Store the results as a 2 row matrix, where the first
  #' row is the in-sample errors, and the second the cross-validation
  #' errors.
  #' Also store the sequence of number of rows in the training set as dseq.
  #'
  #' First helper function gets in-sample error
  get_in_sample=function(train) {
    model=lm(cmedv~.,train)
    p=predict(model,train)
    mean((p-train$cmedv)^2)
  }
  #' First helper function gets cross validation (out-of-sample) error
  get_out_sample=function(data) {
    K=10
    rows=nrow(data)
    step=floor(rows/K)
    tse=sapply(1:K,function(k){
      test_indices=(step*(k-1)+1):(step*k)
      test=data[test_indices,]
      train=data[-test_indices,]
      model=lm(cmedv~.,train)
      p=predict(model,test)
      sum((p-test$cmedv)^2)
    })
    sum(tse)/rows
  }

  N=nrow(train)
  points=10
  step=floor(N/points)
  dseq=seq(step,step*points,step)

  results=sapply(dseq,function(n) {
    train_=train[1:n,]
    in_sample=get_in_sample(train_)
    out_sample=get_out_sample(train_)
    c(in_sample,out_sample)
  })

  #' Plot the in-sample (in blue) and cross-validation (in red)
  #' MSEs in the results matrix against the data size in dseq.
  #' Make the ylim be .95 of smallest and 1.05 of the largest
  #' value in the results matrix.
  #' Give the X and Y axes the labels "Data" and "MSE" respectively.
  #' Give the graph the title "Learning Graph"
  all=results
  min_y=min(all)*.95
  max_y=min(max(all)*1.05,12000)

  plot(dseq,results[1,],type="l",col="blue",main=paste("Learning Graph"),
       ylim=c(min_y,max_y),xlab="Data",ylab="MSE")
  points(dseq,results[2,],type="l",col="red")

  #' When you look at the picture, you may notice that the in-sample error
  #' is not monotonically increasing.
  #' As noted, real problem's learning graphs are noisy and diverge
  #' a long way from the theoretical ideal.
  #' In the next example, we learn how to work with difficult
  #' learning graphs...
}
