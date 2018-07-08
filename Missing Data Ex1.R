#' Here we look at using the EM algorithm to
#' fill in missing data. We discuss the similarities between
#' transductive learning and missing data estimation.
#'
#' Again, we have produced a custom EM algorithm to allow
#' interested students to look at the implementation.
#' Others can just use it as any third-party library. It is
#' in this file below the exercise function so remember to
#' source the file.
#'
#' We will look specifically at a case where the missing values
#' are nominal, since that is usually what we use the EM algorithm
#' for. We prefer the MCMC approach when real valued features
#' are missing values.
#'
#' Since the EM part is done quickly, we also look at
#' multiplicative vs additive linear regression models and use
#' the BIC for model evaluation.
missingDataExercise_1=function() {
  library(car)
  data(Duncan)
  dataset=Duncan[c(1:2,4)]
  dataset[sample(nrow(dataset),5),1]=NA
  realValues=Duncan[which(is.na(dataset$type)),1]

  #' The data contains two columns:
  #'   type - a nominal variable with values {prof,wc,bc}
  #'          representing class of employment (professional, white
  #'          collar, blue collar)
  #'   income - a number representing average income for this
  #'          profession in 1950 USA, between 0 and 100
  #'   prestige - a number representing the prestige this
  #'          profession was held in in 1950 USA, between 0 and 100
  #' Each row has a name representing the profession. There are 45 rows.
  #' We have caused some of the type values to be missing. The actual
  #' value of the missing items is given in the realValues vector.
  #'
  #' At this point, assume that our aim is to create a model that
  #' can estimate the prestige of a career based on its type and
  #' income. In this case, type is a feature and finding the missing
  #' values of that feature is a missing data problem.

  #' Before we do anything else, work out the number of the rows that
  #' contain missing type values.
  missing=which(is.na(dataset$type))

  #' Also work out the unique values of the type variable (excluding NA
  #' which represents a missing value.)
  types=unique(dataset[,1])
  types=types[!is.na(types)]

  #' We seek to estimate the missing values of type given the
  #' known data. The only available other feature
  #' is income. We will assume a conditional Gaussian relationship
  #' between type and income, P(income|type), and that we have a
  #' Dirichlet prior of all ones on the parameters of P(type).
  #'
  #' Given these assumptions, we can use our expectation
  #' maximization algorithm to find probability distributions over
  #' the missing values.
  #'
  #' Run our custom EM algorithm, and store the results.
  probs=EM(dataset)
  #' The results give a probability distribution for each of
  #' the missing values.

  #' Output the results to console
  print(probs)
  #' Also output the true type values for each row number to console.
  for (i in 1:length(missing))
    cat("Row: ",missing[i]," , True value: ",as.character(realValues[i]),"\n")

  #' The approach works quite well, but has trouble with wc values.
  #' This is due to the fact that it has a very low prior - if you
  #' combine the known values and the true values for the missing items
  #' you will see that there are 21 bc, 18 prof and only 6 wc values.
  #' On the plus side, as they were chosen randomly, this means it is
  #' unlikely the missing values are wc values.

  #' Note that it would be possible that estimating the missing type
  #' values (or estimating a distribution over their possible values)
  #' given the income and known type data was *all* we wanted to
  #' accomplish. In this case, we would be performing so called
  #' 'transductive' learning. You can use supervised learning techniques
  #' for transductive learning, in which case you train on the labelled
  #' cases and use the resulting model to estimate the unknown cases.
  #' But if you want to make use of the fact that you know before hand
  #' the feature values of the specific cases you want to use the model
  #' on, it begins to look very much like a missing data situation, and
  #' missing data algorithms can indeed be applied.
  #'
  #' Taking this further, any supervised learning task can be treated as
  #' a transductive learning task once data the model is to be applied
  #' on is available. At that point, it too can be solved using missing
  #' data approaches.

  #' But in our case, we want to performing missing data estimation
  #' of the missing feature values and then use this in the
  #' supervised learning task of using the type and income
  #' features to estimate the prestige values. We now need to use the
  #' probability distributions obtained from the EM algorithm for the
  #' missing type values as part of this task.
  #'
  #' To do this, first create a new data set containing only
  #' the rows without missing values in the original data.
  fullData=dataset[-missing,]

  #' Then add to this rows giving every
  #' possible type value for each missing row, paired with the
  #' actual income value for that row.
  for (i in 1:length(missing)) {
    for (j in 1:length(types)) {
      fullData=rbind(
        fullData,
        data.frame(
          type=types[j],
          income=dataset$income[missing[i]],
          prestige=dataset$prestige[missing[i]]
        ))
    }
  }

  #' Now create a weight vector, with one element for each row in
  #' the new data set.
  #' Rows from the original dataset without any missing values will
  #' get a weight of 1. Other rows gets a weight given by the
  #' probability of the that combination from the EM algorithm.
  weights=rep(1,nrow(dataset)-length(missing))
  for (i in 1:length(missing)) {
    for (j in 1:length(types)) {
      weights=c(weights,probs[i,j])
    }
  }

  #' We are now able to use this weighted data with supervised
  #' learning algorithms. Almost all supervised learning algorithms work
  #' fine with weighted training data, however such an option is
  #' not always implemented in particular libraries/tool suites.
  #'
  #' One simple algorithm implementation in R that does allow
  #' is the stats library implementation of linear regression.
  #' Make two models using the lm function, with the formulas:
  #'  i.   prestige~type+income
  #'  ii.  prestige~type*income
  #' The first will give different intercepts for each type.
  #' The second will give different income parameters as well.
  model1=lm(prestige~type+income,fullData,weights=weights)
  model2=lm(prestige~type*income,fullData,weights=weights)

  #' Since we used all the data, we will use BIC to evaluate the
  #' models. The first model has
  #' Do you remember the equation?
  #' Now plot the data and the regression lines from each model (use
  #' two plots). Use different colors for different types (use gray
  #' for missing).
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

  #' Since we used all the data as training data, we cannot use
  #' validation techniques for selection/evaluation.
  #' Finally, we use this as a chance to practice statistical
  #' model evaluation. Assuming a single error distribution (rather
  #' than one for each line) it is easy to calculate the log
  #' likelihood for each model:
  logProb1=sum(dnorm(model1$residuals,0,sd(model1$residuals)))
  logProb2=sum(dnorm(model2$residuals,0,sd(model2$residuals)))
  #' Now calculate the BIC score for each model (model1 has 4
  #' parameters, model2 has 6). Do you remember the equation?
  #' If not, re-read the 'Evaluation of Statistical Models'
  #' article from week two. The lower the better,
  bic1=log(nrow(dataset))*4-2*logProb1
  bic2=log(nrow(dataset))*6-2*logProb2
  #' Output these values to the console
  cat("The additive linear model had a BIC score of ",bic1,".\n")
  cat("The multiplicative linear model had a BIC score of ",bic2,".\n")
  if (bic1<bic2)
    cat("We would choose the additive model according the BIC.\n") else if (bic1>bic2)
    cat("We would choose the multiplicative model according the BIC.\n") else
    cat("The models had equal BIC scores.\n")
}


#' Here we create the EM algorithm steps
#' The E step returns the parameters for the distributions
E_Step=function(probs,types,missing,dataset,rows){
  #' We want to calculate the parameters for the distributions
  #' P(income|type) and P(type) given the data and
  #' current probabilities for the missing type values.
  lapply(1:length(types),function(i){
    #' We want to get the mean for each distribution
    #' We include the missing rows probabilistically
    num1=sum(dataset[rows[[i]],2])
    num2=sum(sapply(1:length(missing),function(j){
      dataset[missing[j],2]*probs[j,i]
    }))
    denom1=length(rows[[i]])
    denom2=sum(probs[,i])
    mu=(num1+num2)/(denom1+denom2)
    #' We do the same thing for the variance
    num3=sum((dataset[rows[[i]],2]-mu)^2)
    num4=sum(sapply(1:length(missing),function(j){
      (dataset[missing[j],2]-mu)*probs[j,i]
    }))
    sigma=(num3+num4)/(denom1+denom2-1)
    #' We record the parameters for each distribution:
    #' The mu and sigma of the normal distribution, and
    #' the count of the type for the prior, plus one (since
    #' we assume a Dirichlet prior).
    return (list(mu=mu,sigma=sigma,prior=denom1+denom2+1))
  })
}

#' The M_Step returns the probabilities for the missing
#' values
M_Step=function(params,types,missing,dataset){
  probs=matrix(rep(1/length(types),length(missing)*length(types)),
               nrow=length(missing))
  for (i in 1:length(missing)) {
    #' We find the density values for the income value
    #' for each type density
    evidence=sapply(1:length(types),function(type){
      dnorm(dataset[missing[i],2],params[[type]]$mu,sqrt(params[[type]]$sigma))
    })
    #' We normalize to get the probability
    evidence=evidence/sum(evidence)
    #' And the probability of the type given P(type)
    prior=sapply(1:length(types),function(type){
      params[[type]]$prior
    })
    prior=prior/sum(prior)
    #' Now we multiply them together and normalize and store
    #' the result in the appropriate row of the probability matrix
    combined=evidence*prior
    combined=combined/sum(combined)
    probs[i,]=combined
  }
  return (probs)
}


#' Here is the EM main function. We have not tried to
#' provide a fully general EM algorithm. This assumes that
#' (i)   There are two features in dataset and they are columns
#'       1 and 2.
#' (ii)  The first column is nominal, and contains missing values
#' (iii) The second column is continuous and does not contain
#'       missing values
#' (iv)  That the two features should be modelled by a
#'       conditional Gaussian in the EM algorithm.
#'
#' Given these restrictions, we will try to do things as
#' generically as possibile, avoiding hardcoding values.
#'
#' It is, of course, possible to use the EM algorithm in much
#' more general cases.
EM=function (dataset,tol=.00001,initial=NULL) {
  #' Not that the user can specify a tolerance for convergence.
  #' If the probabilities matrix changes less than this amount
  #' in a single iteration we will declare the algorithm converged.

  #' First, record which rows contain missing values
  missing=which(is.na(dataset[,1]))
  #' Secondly record the unique values of type (ignoring NA which
  #' represents missing values)
  types=unique(dataset[,1])
  types=types[!is.na(types)]

  #' Record which rows have which type value.
  rows=list(
    which(dataset[,1]==types[1]),
    which(dataset[,1]==types[2]),
    which(dataset[,1]==types[3])
  )

  #' Generate a matrix, with one row for each missing value
  #' and one column for each value of type. These rows will
  #' record the probability that the particular row takes the
  #' particular value. Start off setting all these distributions
  #' to uniform.
  #'
  #' These probabilities will be our initial values in the EM
  #' algorithm. The EM algorithm can converge to a local optima, and
  #' you may want to try multiple runs from random initial values.
  #' We have included an argument in this function that allows
  #' the user to specify alternative initial values: initial
  probs=matrix(rep(1/length(types),length(missing)*length(types)),
               nrow=length(missing))
  #' Here we overwrite with the specified initial values if present
  if (!is.null(initial))
    probs=initial

  cont=TRUE
  while (cont) {
    params=E_Step(probs,types,missing,dataset,rows)
    oldProbs=probs
    probs=M_Step(params,types,missing,dataset)
    cont=sum(oldProbs-probs)>tol
  }
  colnames(probs)=types
  rownames(probs)=missing
  return (probs)
}
