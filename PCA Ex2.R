principleComponentExample_2=function() {
  # Prepare libraries and data
  library(mlbench)
  library(utils)
  data(Sonar)
  dataset=Sonar

  #' dataset contains the Sonar dataset, with 60 numeric
  #' features giving the energy with a sonar frequency band.
  #' The target is a class variable specifying whether the
  #' target is a metal cylinder or a roughly cylindrical rock.
  #' It is a classification problem, not a regression problem.
  #' But we will be looking only at the PCA of the features.
  #' The features are columns 1 through 60.
  #'
  #' Perform a (scaled) PCA analysis of the features and return it
  #' as pca.
  pca=prcomp(dataset[1:60],scale=TRUE)

  #' How should we choose the number of PCAs to use?
  #' PCs are directions of maximal variance, and fortunately
  #' we can see how much each PC accounts for the
  #' variance in the data. It is in the pca$sdev field.
  #'
  #' Plot this with a bar graph. Give the x axis the label "PCs"
  #' and the plot the title "Std Dev of data by PC", and give the
  #' index of the PC as the name associated with each bar.
  barplot(pca$sdev,
          names.arg=1:length(pca$sdev),
          xlab="PCs",
          main="Std Dev of data by PC")

  #' Typically you would us PCs that are before the 'elbow'
  #' of the graph (where the slope passes through 45%), or you
  #' would look for an obvious break. It is possible to automate
  #' such a procedure.
}
