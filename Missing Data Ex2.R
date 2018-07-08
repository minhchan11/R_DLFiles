#' Here we look at using MCMC sampling (Metropolis in Gibbs)
#' for missing data estimation. Again, a custom implementation of the
#' missing data algorithm is provided for intesting students.
#' This is customized for the example, but how to perform the
#' algorithm in the general case should be clear. Note that MCMC
#' approaches are computationally heavy, and for large data you
#' would normally use compiled code (either manually coded or from
#' a library) for speed.
#'
#' The case here is the same as in the previous missing data example,
#' except now both type (a nominal variable) and income (a real variable)
#' have missing values. See the previous exercise for an explanation of
#' the data use. We will again create additive and multiplicative
#' linear models from the data. But we will use the AIC instead of the
#' BIC in evaluating these models.
missingDataExercise_2=function() {
  # Prepare data and libraries
  library(car)
  data(Duncan)
  dataset=Duncan[c(1:2,4)]
  dataset[sample(nrow(dataset),5),1]=NA
  dataset[sample(nrow(dataset),5),2]=NA
  realTypeValues=Duncan[which(is.na(dataset$type)),1]
  realIncomeValues=Duncan[which(is.na(dataset$type)),2]

  #' Note that as well the dataset, you have the true values
  #' of the missing values in both the type and income columns,
  #' ordered by row, in the realTypeValues and realIncomeValues
  #' vectors if you want to examine them.

  #' Find the row and column indices of the missing values
  missing=which(is.na(dataset),arr.ind=T)

  #' To do the Metropolis in Gibbs algorithm, we need to specify
  #' burn size and sample size. In many normal cases you need a
  #' lot of samples. But we should be ok with quite a small amount.
  burn = 100
  samples = 200
  samples_=MetropolisInGibbs(dataset,burn,samples)

  #' We could look through the probability distributions for the missing
  #' values from the samples, comparing them with the real value
  #' However, we will not do that here. The results are likely to be
  #' poor with this number of samples, and it should be noted that MCMC
  #' sampling algorithms do not actually try to estimate the most likely
  #' value, they rather try to estimate the probability distribution of
  #' possible values.

  #' Create a weighted data set, using original rows without
  #' missing data (weight 1) and the returned samples (weight 1/the
  #' number of samples). Create this data set.
  newData=dataset[-missing[,1],]
  weights=rep(1,nrow(newData))
  newData=rbind(newData,samples_)
  weights=c(weights,rep(1/samples,nrow(samples_)))

  #' Like in the EM missing data example, create an additive and
  #' multiplicative linear model from this weighted data
  model1=lm(prestige~type+income,newData,weights=weights)
  model2=lm(prestige~type*income,newData,weights=weights)

  #' Assuming a single error distribution (rather
  #' than one for each line) it is easy to calculate the log
  #' likelihood for each model:
  logProb1=sum(dnorm(model1$residuals,0,sd(model1$residuals)))
  logProb2=sum(dnorm(model2$residuals,0,sd(model2$residuals)))

  #' Now calculate the AIC score for each model (model1 has 4
  #' parameters, model2 has 6). Do you remember the equation?
  #' If not, re-read the 'Evaluation of Statistical Models'
  #' article from week two. The lower the better,
  aic1=2*4-2*logProb1
  aic2=2*6-2*logProb2
  #' Output these values to the console
  cat("The additive linear model had a AIC score of ",aic1,".\n")
  cat("The multiplicative linear model had a AIC score of ",aic2,".\n")
  if (aic1<aic2)
    cat("We would choose the additive model according the AIC.\n")
  else if (aic1>aic2)
    cat("We would choose the multiplicative model according the AIC.\n")
  else
    cat("The models had equal BIC scores.\n")

  #' Now plot the data and the regression lines from each model (use
  #' two plots). Use different colors for different types (use gray
  #' for missing).
  types=unique(dataset$type)
  types=types[-which(is.na(types))]
  col=sapply(dataset$type,function(t){
    if(is.na(t)) "gray"
    else if (t==types[1]) "blue"
    else if (t==types[2]) "green"
    else "red"
  })

  #' Make two plots
  par(mfrow=c(2,1))
  plot(prestige~income,dataset,col=col,main="Additive Formula")
  f11=function(x){
    predict(model1,data.frame(income=x,type=rep(types[1],length(x))))
  }
  f12=function(x){
    predict(model1,data.frame(income=x,type=rep(types[2],length(x))))
  }
  f13=function(x){
    predict(model1,data.frame(income=x,type=rep(types[3],length(x))))
  }
  xseq=0:100
  points(xseq,f11(xseq),type="l",col="blue")
  points(xseq,f12(xseq),type="l",col="green")
  points(xseq,f13(xseq),type="l",col="red")

  plot(prestige~income,dataset,col=col,main="Multiplicative Formula")
  f21=function(x){
    predict(model2,data.frame(income=x,type=rep(types[1],length(x))))
  }
  f22=function(x){
    predict(model2,data.frame(income=x,type=rep(types[2],length(x))))
  }
  f23=function(x){
    predict(model2,data.frame(income=x,type=rep(types[3],length(x))))
  }
  points(xseq,f21(xseq),type="l",col="blue")
  points(xseq,f22(xseq),type="l",col="green")
  points(xseq,f23(xseq),type="l",col="red")

  #' Reset mfrow
  par(mfrow=c(1,1))

  #' return (samples)

}

MetropolisInGibbs=function(dataset,burn,samples) {
  #' Find the row and column indices of the missing values
  missing=which(is.na(dataset),arr.ind=T)

  #' Unique types, excluding NA
  types=unique(dataset$type)
  types=types[-which(is.na(types))]

  #' Assign initial random values
  for (i in 1:nrow(missing)) {
    if (missing[i,2]==1) {
      # Assign random type
      dataset[missing[i,1],missing[i,2]]=sample(types,1)
    }
    else {
      # assign random integer numeric value between 0 and 100
      dataset[missing[i,1],missing[i,2]]=sample(0:100,1)
    }
  }
  # Create an update function that will perform one
  # iteration of the algorithm
  update=function(dataset,missing,types) {
    calcParams=function(type) {
      typedIncome=dataset$income[which(dataset$type==type)]
      out=list(
        mu=mean(typedIncome),
        sigma=var(typedIncome),
        priorNum=length(typedIncome)+1
      )
      if (out$mu<0)
        stop("Damn")
      return (out)
    }
    priorDenom=nrow(dataset)+3
    params=lapply(1:length(types),function(i) calcParams(types[i]))
    for (i in 1:nrow(missing)) {
      if (missing[i,2]==1) {
        # Missing value is in the type column
        # Find current value
        cur=dataset[missing[i,1],1]
        # Find candidate value
        cand=sample(types[-which(types==cur)],1)
        # Calculate pNew and pOld
        pNew=dnorm(dataset[missing[i,1],2],
                   params[[which(types==cand)]]$mu,
                   params[[which(types==cand)]]$sigma) *
             params[[which(types==cand)]]$priorNum/priorDenom
        pOld=dnorm(dataset[missing[i,1],2],
                   params[[which(types==cur)]]$mu,
                   params[[which(types==cur)]]$sigma) *
             params[[which(types==cur)]]$priorNum/priorDenom
        # Check if update to be made.
        # Note condition always true if pNew>pOld
        if (runif(1) < pNew/pOld) {
          # Update dataset and parameters
          dataset[missing[i,1],1]=cand
          params[[which(types==cur)]]=calcParams(cur)
          params[[which(types==cand)]]=calcParams(cand)
        }
      }
      else {
        # Missing value is in the income column
        # Find current value
        cur=dataset[missing[i,1],2]
        # Find candidate value
        # We allow non-integer numbers
        cand=rnorm(1,dataset[missing[i,1],2],2)
        # Only proceed if cand is in valid range.
        # Otherwise pNew is 0 so no update happens (we assume
        # conditional Gaussian are cut off at 0 and 100)
        if (cand>=0 && cand<=100) {
          # Calculate pNew and pOld
          # Note that we do not need to worry about P(type) since this
          # will be the same for pNew and pOld
          pNew=dnorm(cand,
                     params[[dataset[missing[i,1],1]]]$mu,
                     params[[dataset[missing[i,1],1]]]$sigma)
          pOld=dnorm(cur,
                     params[[dataset[missing[i,1],1]]]$mu,
                     params[[dataset[missing[i,1],1]]]$sigma)
          # Check if update to be made.
          # Note condition always true if pNew>pOld
          if (runif(1) < pNew/pOld) {
            # Update dataset and parameters
            dataset[missing[i,1],2]=cand
            params[[dataset[missing[i,1],1]]]=calcParams(dataset[missing[i,1],1])
          }
        }
      }
    }
    # Return the updated dataset
    return (dataset)
  }

  # Perform burn
  for (i in 1:burn) {
    if (i%%10==0)
      cat("Burn ",i,"/",burn,"\n")
    dataset=update(dataset,missing,types)
  }
  samples_=c()
  # Perform sampling
  for (i in 1:samples) {
    if (i%%10==0)
      cat("Sample ",i,"/",samples,"\n")
    dataset=update(dataset,missing,types)
    # We only store as samples the rows that had missing values
    samples_=rbind(samples_,dataset[missing[,1],])
  }

  # Return samples
  return (samples_)
}
