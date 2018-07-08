#' We spice up the ANN classification example by doing a realistic
#' hyper-parameter search (admittedly with a small data set - it would
#' take a long time with a large data set.)
neuralNetworksExample_2=function() {
  library(datasets)
  library(utils)
  library(nnet)
  data(iris)
  dataset=iris
  #' This data gives the measurements in centimeters of the variables
  #' sepal length and width and petal length and width, respectively,
  #' for 50 flowers from each of 3 species of iris. The species are Iris
  #' setosa, versicolor, and virginica. The task is to classify the
  #' species given the measurements. It is a commonly used dataset
  #' in machine learning pedogogy and (previously at least) benchmarking.

  #' Begin by splitting the data into training, validation and test
  #' sets in the ration 5:3:2.
  n=nrow(dataset)
  rnd_indices=sample(n)
  train=dataset[rnd_indices[1:(n*.5)],]
  valid=dataset[rnd_indices[(1+(n*.5)):(n*.8)],]
  test=dataset[rnd_indices[(1+(n*.8)):n],]

  #' Now create a set of neural net models. Create 4
  #' models of every combination of size=4,6,8,...,16 and
  #' decay=0,.1,1. We are doing this to try to get at least
  #' one good random initialization of the weights for each
  #' pair of hyper-parameter values.
  #'
  #' Store the models as a flat list (ie not
  #' as a list of lists of lists of models). Use unlist
  #' with recursize=FALSE if required (see help documentation
  #' or play around). The order should be 4 models of size 4,
  #' lambda 0, 4 of size 6 lambda 0, etc.
  lambdas=c(0,.1,1)
  # 7 x 3 x 4 = 7 x 12 = 84 models
  models=lapply(seq(4,16,2),function(s){ # 7 alternatives
    lapply(lambdas,function(lambda){     # 3 alternatives
      lapply(1:4,function(i){            # 4 alternatives
        nnet(Species~.,train,size=s,decay=lambda)
      })
    })
  })
  models=unlist(models,F)
  models=unlist(models,F)

  #' To keep us concious of what we are asking
  #' the computer to do, work out how many models
  #' are being fit in total and store this as num_models
  #' and output this information to the console.
  num_models=length(models)
  cat("\nThere are",num_models,"neural network models.\n")

  #' Now calculate the validation error for each
  #' model. By default, using predict
  #' with a nnet model will return a vector of class
  #' probabilities. But you can change this to have just
  #' the class returned by setting type="class". Return
  #' the validation misclassification error in a vector called
  #' mces.
  #'
  #' Find the model that has the least validation error and
  #' evaluate the test error of that model on the test
  #' data. Then store the best model as best_model, and
  #' its test MCE score as test_mce.
  mces=sapply(models,function(model) {
    p=predict(model,valid,type="class")
    mean(p!=valid$Species)
  })

  best=which.min(mces)
  best_model=models[[best]]
  p=predict(best_model,test,type="class")
  test_mce=mean(p!=test$Species)

  #' Output the number of hidden nodes and l2 regularization
  #' parameter of the best model, as well as its validation and
  #' test error.
  #'
  #' While you might not be able to easily work out the
  #' structure of the best model from its index in models,
  #' you can find the number of hidden units in the best
  #' model by looking at best_model$n[2]. You can also find
  #' the weight decay use in training the best model by
  #' looking at best_model$decay.
  cat("\nThe best model had",best_model$n[2],"hidden nodes and",best_model$decay,"weight decay.\n")
  cat("It had",mces[best],"validation MCE.\n")
  cat("It had",test_mce,"test MCE.\n")

  #' We'll finish up with a little graph that will
  #' reveal a problem with rash conclusions about us
  #' finding the best hyper-parameters for a shallow
  #' neural network for this task.
  #'
  #' Do a barplot of mce. Group the bars in sets
  #' of 4 corresponding to the sets of models with the same
  #' size and decay. These sets are contiguous in the
  #' models list. You group bars by passing a matrix
  #' with the entries in the same columns the bars in the
  #' group (instead of the usual vector), and then specifying
  #' beside=TRUE (otherwise they will be placed on top of
  #' each other).
  #'
  #' We will also give them names corresponding to their
  #' size and decay parameters - just the numbers with a
  #' space between them. The sequences were size: 4,6,8,...,16
  #' and decay: 0,0.1,1. Once you make the names, assign them
  #' as column names to the matrix being passed to barplot.
  #'
  #' Now run the barplot, with the title "Validation MCE", and
  #' x and y axis labels "Hyper-parameters" and "MCE".
  #'
  #' You will probably see that *lots* of models are getting the
  #' same validation mce. That's ok in terms of choosing a good
  #' model. We are still using *one* of the best models (in
  #' fact the first one since which.min will return the
  #' index of the first minimal item). But obviously
  #' it is a mistake to think that we have found the best size
  #' and decay parameters. We would need much more training and
  #' validation data to do that with any confidence!
  heights=matrix(mces,nrow=4)
  sizes=as.character(seq(4,16,2))
  decays=as.character(c(0,.1,1))
  names=sapply(sizes,function(s){
    sapply(decays,function(d){
      paste(s,d)
    })
  })
  names=as.vector(names)
  colnames(heights)=names
  barplot(heights,beside=T)
}
