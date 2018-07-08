#' Learning graphs are useful for working out if you have enough
#' training data for a particular modelling algorithm.
#'
#' To make them we generate a sequence of models using
#' increasing proportions of our training data, for example
#' 10%, 20%, ... , 100%.
#'
#' For each of these models we calculate the in-sample (training)
#' error, and cross-validated (or other out-of-sample) error.
#'
#' The idea is that as data increases, in-sample and out-of-sample
#' error are optimistic and pessemistic predictors of expected error
#' with infinite data. Ie as the training data approaches infinity,
#' in-sample error approaches the expected error of a model trained
#' on infinite data *from below*, and out-of-sample error approaches
#' it *from above*.
#'
#' By examining this sequence we can see:
#'  - approximately how much room there appears to be for
#'    improvement from more data
#'  - approximately what the expected error of an optimal model
#'    of this type (trained on infinite data) would be
#'  - approximately how much improvement we can expect from
#'    increasing our training data by a certain amount
#'
#' We can see these things by, respectively:
#'  - The difference between the two curves at the 100% of training
#'    data model
#'  - The middle of the difference between the two curves at the
#'    100% of training data model
#'  - The slope of the two curves at the 100% of training data model

#' Let's have an example.
#' First we create some pretend data
dseq=seq(100,1000,100)
out1=c(165,114,107,105,94,92,88,86,85,83)
in1=c(14,54,55,57,58,60,61,62,62,63)
# Now we can plot the graph
plot(dseq,in1,type="l",col="blue",main=paste("Learning Graph"),
     ylim=c(0,170),xlab="Data",ylab="MSE")
points(dseq,out1,type="l",col="red")
# Let's annotate that plot
segments(1000,83,1000,63,col="orange",lty=2)
abline(h=73,lty=2)
legend(100,160,
       c("Room for improvement","Approx. expected performance with infinite data"), # puts text in the legend
       lty=c(2,2), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),
       cex=.6,
       col=c("orange","black"))
title(sub="Slope of blue line at full amount of training data is .01\nSlope of red line at full amount of training data is -.02",cex.sub=.6)
#' So if we add another 100 rows to our training data, we can
#' expect our cross-validated error to fall by about 2.

#' Learning graphs can often be much more difficult to interpret
#' than those that we have seen in these exercises. The reason is that
#' they are sometimes a *lot* noisier than this example. They might
#' jump around a lot more, with the blue and red lines criss-crossing
#' each other.
#'
#' In such cases, it is normally sufficient to smooth the learning
#' graph to make it useable. Smoothing means instead of plotting the
#' MSEs for each data size, average over 3 or 5 (for example) points.
#'

# Ignore the equations- this just gives us something reasonable.
set.seed(10)
x=seq(1000,20000,1000)
out2=6500-x/log(x)+rnorm(length(x),0,800)
in2=3500+x/log(x)+rnorm(length(x),0,800)
plot(x,out2,type="l",col="red",ylim=c(min(in2),max(out2)),
     ylab="Error",xlab="Data",main="Unsmoothed Learning Graph")
points(x,in2,type="l",col="blue")
# Difficult to understand, so we smooth it. Here 6 works pretty well.
n=6
smooth=function(y,s) {
  sapply(1:(length(y)-(s-1)),function(i) {
    sum(y[i:(i+s-1)])/s
  })
}
x_s=smooth(x,n)
in2_s=smooth(in2,n)
out2_s=smooth(out2,n)
plot(x_s,in2_s,type="l",col="blue",ylim=c(min(in2_s),max(out2_s)),
     ylab="Error",xlab="Data",main="Smoothed Learning Graph")
points(x_s,out2_s,type="l",col="red")

#' Exercise One:
#' When you look at the picture, it may be that the in-sample error
#' is not monotonically increasing but rather decreasing!
#' As noted, real problem's learning graphs are noisy and diverge
#' a long way from the theoretical ideal.
#'
#' Exercise Two:
#' Which models look to have significant room to improve from more
#'   data?
#' Which models look to have better optimal performance?
#' Which models would you keep working with?
