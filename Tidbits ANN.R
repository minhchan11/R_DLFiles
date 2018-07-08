library(nnet)
data(airquality)

#' At this point, we will be looking at a very simple neural net
#' implementation called nnet. This implementation only permits
#' 'shallow' networks can be built, meaning that there is only
#' one hidden layer.
#'
#' For the exercises in this module, we will use the nnet constructor
#' with the following arguments:
set.seed(1)
model=nnet(
  Ozone~Wind,   # The formula giving the target variable and features
  airquality,   # The dataset to find these variables in
  size=4,       # The number of nodes in the single hidden layer
  linout=TRUE   # Specifies that we are doing regression, if classification linout = FALSE
  )
#' Later we will learn to use more arguments in nnet.
#'
#' The predict function works as expected:
predict(model,data.frame(Wind=c(1,10,7,16)))

#' Non-Determinism
#'
#' The parameters (weights) in a nnet model are fitted by
#' gradient descent, such that weights are iteratively updated
#' in the direction required to minimize a cost function (in
#' the regression case, one like MSE).
#'
#' The initial weights used cannot be zero (otherwise all
#' gradients are 0 and gradient descent cannot be used).
#' They can be specified by the Wts argument. If this is not
#' specified, they are random. Since there are many local optima,
#' this means that the algorithm is non-deterministic: Running
#' it multiple times will lead to different results. It is usual
#' to try multiple runs to find the best result.
#'
#' You can see that we set the random seed in the above example.
#' This was to make sure we got a reasonable model.
#' In the exercises, the random seed is set before any code in
#' which you are asked to use non-deterministic functions, so that
#' your answer will match the demonstration answer.

#' For a bit of fun...
#' We can explicitly reproduce the workings of our model:
#' nnet uses the logistic activation function
sigmoid=function(sum) 1 / (1 + exp(-sum))

summary(model) # We see the weights
model$wts      # They are stored in this vector

# This function calculates values of hidden nodes
h=function(i,wind) {
  if (i==0)
    1       # bias node
  else
    sigmoid(model$wts[(i*2-1)]+wind*model$wts[i*2])
}
#' This function replicates the whole network
y=function(wind) {
  # We calculate the feature transformation corresponding
  # to our hidden layer.
  # Remember this transformation (or at least its weights)
  # was learnt from the data.
  h0=h(0,wind)
  h1=h(1,wind)
  h2=h(2,wind)
  h3=h(3,wind)
  h4=h(4,wind)
  hs=c(h0,h1,h2,h3,h4) # This is the feature transformation
  # We use the last weights to perform linear regression
  # on the transformed features.
  sum(hs*model$wts[9:13])
}
y(17)
predict(model,data.frame(Wind=17))

# Nice :)

#' Of course, the hard part is learning the weights
#' from the training data. We *could* manually do that, but
#' we won't!
