# Data split into two subsets
holdoutValidationExample_1 = function () {
  # Load libraries and data
  library(datasets)
  library(utils)
  library(nnet)
  data(airquality)
  dataset=airquality

  # Take the data in inputs and remove rows with
  # NA in the Ozone or Wind columns.
  # Then split the remaining cases into
  # two data sets, the first with 75% and the second with
  # 25% of cases. Cases should be assigned randomly to each
  # data set (use sample).
  missing=which(is.na(dataset$Ozone)|is.na(dataset$Wind))
  dataset=dataset[-missing,]

  tr=sample(nrow(dataset),nrow(dataset)*.75)
  train=dataset[tr,]
  test=dataset[-tr,]

  # Create:
  #   An OLS model
  #   A Poisson regression model
  #   A third order polynomial regression model
  #   A neural network model with 3 hidden nodes
  # Modeling Ozone~Wind
  models=list()
  models[[1]]=lm(Ozone~Wind,train)
  models[[2]]=glm(Ozone~Wind,"poisson",train)
  models[[3]]=lm(Ozone~poly(Wind,degree=3),train)
  models[[4]]=nnet(Ozone~Wind,train,size=3,linout=T)

  # Examine the four models in models by calculating
  # their MSE on the test dataset
  # Return the best model as best
  mses=sapply(models,function(model){
    p=c()
    if (class(model)[1]=="glm")
      p=predict(model,test,type="response")
    else
      p=predict(model,test)
    mean((p-test$Ozone)^2)
  })
  best=which.min(mses)
  cat("The best model was: ")
  if (best==1)
    cat("OLS\n") else if (best==2)
    cat("Poisson regression\n")  else if (best==3)
    cat("polynomial regression\n")  else
    cat("neural network\n")

  # Plot Ozone~Wind from data and then
  # the regression curve of the four models from 0 to 22
  # using color 1,2,3 and 4 respectively.
  plot(Ozone~Wind,dataset)

  mses=sapply(1:4,function(i){
    model=models[[i]]
    f=NULL
    if (class(model)[1]=="glm")
      f=function(x)predict(model,data.frame(Wind=x),type="response")
    else
      f=function(x)predict(model,data.frame(Wind=x))
    plot(f,from=0,to=22,col=i,add=T)
  })

  legend(10,150, # places a legend at the appropriate place
         c("OLS","Poisson","Polynomial","Neural Net"), # puts text in the legend
         lty=rep(1,4), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5),
         col=1:4)
}
# If you run this exercise a few times, you will see that the best model changes. You will also see that sometimes the neural net will be terrible (it can even be constant)!
#   This randomness is the result of the different runs using different random seeds. This causes:
#   i) Different splits on the data when it is divided into training and test (this affects all models)
#   ii) Different random initial weights for the neural net (this affects only the neural net)
#
# In fact, these two components are representative of very different 'randomness'.
#   i) Randomess of the sample data and the training/test split and the possibility that some models or algorithms might 'strike it lucky'.
#   ii) Randomness in a non-deterministic algorithm (hyper-parameter, though not generally tuned, and for some non-deterministic algorithms perhaps untunable)
#
# We ran the algorithm 1000 times and found:
#
# Best performing model:
#
#   OLS:31, Poisson:335, Polynomial:431, Neural Net:203
#
# Summary statistics for MSES:
#
# OLS              Poisson          Polynomial
# Min.   : 321.4   Min.   : 218.5   Min.   : 208.3
# 1st Qu.: 591.5   1st Qu.: 493.1   1st Qu.: 492.1
# Median : 704.7   Median : 587.5   Median : 593.7
# Mean   : 726.8   Mean   : 600.4   Mean   : 607.3
# 3rd Qu.: 839.6   3rd Qu.: 698.2   3rd Qu.: 702.3
# Max.   :1631.0   Max.   :1332.9   Max.   :2086.7
#
# Neural Net
# Min.   : 243.0
# 1st Qu.: 608.5
# Median : 840.2
# Mean   : 906.9
# 3rd Qu.:1119.9
# Max.   :8470.0
#
# This is due to the above randomness, and the small amount of data used to train and test our models. As the amount of data increases, we would get less variance in the results.
# This raises interesting questions.
# - Firstly, are we simply picking a model that happened to be 'lucky' in a particular run?
# - More sophisticatedly, how can we be confident we have enough data to judge the performance of our models?


# We want to concentrate on the first type of randomness here, so let's exclude the neural net model for the moment. Now let's see what happens when we run the example 1000 times with the original data.
#
# Results: OLS:53, Poisson:328, Polynomial:619
# OLS              Poisson         Polynomial
# Min.   : 358.9   Min.   :330.4   Min.   :332.9
# 1st Qu.: 622.9   1st Qu.:523.0   1st Qu.:511.8
# Median : 712.6   Median :600.3   Median :592.5
# Mean   : 719.4   Mean   :600.4   Mean   :607.2
# 3rd Qu.: 798.1   3rd Qu.:660.6   3rd Qu.:719.4
# Max.   :1108.6   Max.   :952.5   Max.   :874.7
#
#
# Let's pretend we had more data, simulating this by replicating the data we have 10 times:
#
# Results: OLS:0, Poisson:149, Polynomial:851
# OLS             Poisson         Polynomial
# Min.   :598.6   Min.   :502.8   Min.   :493.5
# 1st Qu.:662.6   1st Qu.:545.3   1st Qu.:521.7
# Median :687.8   Median :576.0   Median :550.4
# Mean   :693.7   Mean   :575.7   Mean   :552.4
# 3rd Qu.:731.3   3rd Qu.:600.8   3rd Qu.:581.2
# Max.   :816.4   Max.   :664.8   Max.   :656.9
#
# And by replicating the data we have 100 times:
#
# Results: OLS:0, Poisson:0, Polynomial:1000
# OLS             Poisson         Polynomial
# Min.   :632.8   Min.   :529.9   Min.   :506.7
# 1st Qu.:677.5   1st Qu.:560.8   1st Qu.:533.7
# Median :689.4   Median :570.1   Median :541.6
# Mean   :689.4   Mean   :570.4   Mean   :542.7
# 3rd Qu.:701.7   3rd Qu.:580.7   3rd Qu.:552.9
# Max.   :736.3   Max.   :618.2   Max.   :591.9
#
# The moral is that more data helps you to confidently pick the best model. There is less chance of 'lucky' performance by inferior models on the validation data (we talk more about this soon - it is the variance of a model/algorithm).
# Of course it also helps more complex models, as they avoid overfitting.
#
# Note that you cannot simply replicate your data to help select the best model! This would result in overfitting your model to the training data. This was for demonstration purposes only.
#
# Have a play yourself using the following function:
# n - number of runs
# mult - number of times to duplicate the dataset
# useNN - whether you want to also use a neural network model
aggregate_2_3_results=function(n=100,mult=1,useNN=F) {

  library(datasets)
  utils::data(airquality)

  data=NULL
  data_=airquality
  i=1
  while (i<mult) {
    data_=rbind(data_,airquality)
    i=i+1
  }
  missing=which(is.na(data_$Ozone)|is.na(data_$Wind))
  data=data_[-missing,]

  mses_collection=c()
  names=c("OLS","Poisson","Polynomial")
  if (useNN)
    names=c("OLS","Poisson","Polynomial","Neural Net")
  count=rep(0,4-useNN)
  for(i in 1:n) {
    cat(i,"/",n,"\n")

    # Take the data in inputs and remove rows with
    # NA in the Ozone or Wind columns.
    # Then split the remaining cases into
    # two data sets, the first with 75% and the second with
    # 25% of cases. Cases should be assigned randomly to each
    # data set (use sample).
    set.seed(as.numeric(Sys.time()))
    tr=sample(nrow(data),floor(nrow(data)*.75))
    train=data[tr,]
    test=data[-tr,]

    # Output these two dataset, calling the larger
    # train and the smaller test.

    # Create:
    #   An OLS model
    #   A Poisson regression model
    #   A third order polynomial regression model
    #   A neural network model with 3 hidden nodes
    # Modeling Ozone~Wind
    models=list()
    models[[1]]=lm(Ozone~Wind,train)
    models[[2]]=glm(Ozone~Wind,"poisson",train)
    models[[3]]=lm(Ozone~poly(Wind,degree=3),train)
    if (useNN) {
      models[[4]]=nnet(Ozone~Wind,train,size=3,linout=T)
    }

    # Return these models as a list called models.
    # Enter them in the order they are described.

    # Examine the four models in models by calculating
    # their MSE on the test dataset
    # Return the best model as best
    mses=sapply(models,function(model){
      p=c()
      if (class(model)[1]=="glm")
        p=predict(model,test,type="response")
      else
        p=predict(model,test)
      mean((p-test$Ozone)^2)
    })
    mses_collection=rbind(mses_collection,mses)
    count[which.min(mses)]=count[which.min(mses)]+1
  }

  str=paste("\n","Results:",paste(paste(names,count,sep=":"),collapse=", "))
  cat(str)
  boxplot(mses_collection,names=names)
  return(mses_collection)
}

