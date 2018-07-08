#' We will use the e1071 package, which is a wrapper of the popular
#' libSVM implementation of SVMs.
svmExample=function() {
  library(datasets)
  library(utils)
  library(e1071)
  data(iris)
  dataset=iris

  #' This data gives the measurements in centimeters of the variables
  #' sepal length and width and petal length and width, respectively,
  #' for 50 flowers from each of 3 species of iris. The species are Iris
  #' setosa, versicolor, and virginica. The task is to classify the
  #' species given the measurements. It is a commonly used dataset
  #' in machine learning pedogogy and (previously at least) benchmarking.

  #' Begin by splitting the data into training and test
  #' sets in the ration 3:1.
  rnd_indices=sample(nrow(dataset))
  train=dataset[rnd_indices[1:(length(rnd_indices)*.75)],]
  test=dataset[rnd_indices[(1+(length(rnd_indices)*.75)):length(rnd_indices)],]

  #' We will four SVMs, one with each type of kernel the e1071
  #' SVM implementation allows. These are linear, radial, sigmoid
  #' and polynomial.
  #'
  #' Rather than using the svm function, we will use the helper
  #' function tune to perform a little optimization of hyperparameters.
  #' The hyper-parameters you should tune for different kernels are:
  #'
  #' Linear:     cost: exp(-4:1)
  #' Radial:     cost: exp(-1:3) gamma: exp(-3:0)
  #' Sigmoid:    cost: exp(1:3)  gamma: exp(-3:0) coef0: exp(-3:0)
  #' Polynomial: cost: exp(1:3)  gamma: exp(-3:0) coef0: exp(-3:0)
  #'             degree: 2:3
  #'

  #' In each case you will need to set up a ranges list giving
  #' the parameters to tune and the values to test in tuning.
  #' For example:
  #'
  #'    ranges=list(cost=exp(-1:1))
  #'
  #' With these range objects, use tune with the following
  #' arguments:
  #'
  #'  tune(     svm,              # Use svm function to make models
  #'            formula of model, # As in other models
  #'            data=dataset      # As in other models
  #'            range=ranges      # Our range object
  #'            kernel="linear"   # Kernel to use. default is radial
  #'  )
  #'
  #' Use the train data, with Species as your target
  #' variable and everything else as features. Return the output
  #' of the tune function each kernal, in objects called lin,
  #' rad, sig and poly.
  #'
  #' There are plenty of ways to customize the tune function
  #' by passing a customized tune.control object. Read the
  #' help documentation for more information, but just use
  #' the defaults here.
  #'
  #' You can tell that we have already done some tuning to
  #' give reasonablely small search spaces for the different
  #' kernel types. Still this may take several minutes, as we
  #' will be making many models.
  #'
  #' To give you an idea of what we are asking the computer to
  #' do, work out how many combinations are being tried in for
  #' each kernal and return the result as a four element vector
  #' call combos. You can do it programmatically by using length
  #' and unlist (which will turn a list of vectors into a single
  #' vector), or just manually work it out.
  #'
  #' Actually it is even worse than this, since each combination
  #' will be evaluated using 10-fold cross-validation! We could
  #' change that by changing the tune.control object. With bigger
  #' problems it would be essential to do so!

  # 6 models
  ranges_lin=list(cost=exp(-4:1))
  # 5 x 4 = 20 models
  ranges_rad=list(cost=exp(-1:3),gamma=exp(-3:0))
  # 3 x 4 x 4 = 3 x 16 = 48 models
  ranges_sig=list(cost=exp(1:3),gamma=exp(-3:0),coef0=exp(-3:0))
  # 3 x 4 x 4 x 2 = 12 x 8 = 96 models
  ranges_poly=list(cost=exp(1:3),gamma=exp(-3:0),coef0=exp(-3:0),
                   degree=2:3)
  combos=c(length(ranges_lin$cost),
           length(ranges_rad$cost)*length(ranges_rad$gamma),
           length(ranges_sig$cost)*length(ranges_sig$gamma)*length(ranges_sig$coef0),
           length(ranges_poly$cost)*length(ranges_poly$gamma)*length(ranges_poly$coef0)*length(ranges_poly$degree))

  rad=tune(svm,Species~.,data=train,range=ranges_rad)
  lin=tune(svm,Species~.,data=train,range=ranges_lin,kernel="linear")
  sig=tune(svm,Species~.,data=train,range=ranges_sig,kernel="sigmoid")
  poly=tune(svm,Species~.,data=train,range=ranges_poly,kernel="polynomial")

  #' The outputs from the tune function include the best
  #' performance (from cross-validation) in the field
  #' $best.performance. See which is best and save the
  #' $best.model field of this object as best_model. (This is
  #' the svm created using the best parameters.)
  #'
  #' Notice that validation was done for us using cross-validation.
  #' This is why we did not need to create validation data.
  #'
  #' Evaluate the test misclassification for best_model on the test
  #' data. Return best_model and its test MCE as mce.
  models=list(lin,rad,sig,poly)
  best=which.min(sapply(models,function(m)m$best.performance))
  best_model=models[[best]]$best.model

  #' Let's see which model was best
  summary(best_model)
  
  #' Now evaluate the best model on the test data
  p=predict(best_model,test)
  mce=mean(p!=test$Species)

  #' The svm model has a plot function. It will plot the
  #' decision boundary, data points and support vectors of
  #' a 2d slice of the input space.
  #' The first three arguments to the function are:
  #'   1. The svm model to plot
  #'   2. The training data set (it must be the training
  #'      data to match information about the support vectors
  #'      in the model)
  #'   3. A formula specifying the features to plot (if there
  #'      are more than two features)
  #' An additional argument called slice is a named list giving
  #' values to the variables other than those in the formula.
  #'
  #' What you see is a region plot of the different classes for
  #' given values of the plotted features at the values of the
  #' sliced features. The 'x' points are support vectors. The
  #' 'o' are non-support vector data points. Read the help
  #' documentation for plot.svm for more information and
  #' help interpreting the plot.
  #'
  #' In this exercise, plot Petal.Length vs Petal.Width,
  #' on the slice where Sepal.Length=3 and Sepal.Width=2.
  #' The training data is inputs$train, and the best
  #' model is inputs$best_model
  plot(best_model,train,Petal.Length ~ Petal.Width,
       slice=list(Sepal.Length=3,Sepal.Width=2))
}
