gmmExample=function() {
  # Prepare the data and packages
  library(mclust)
  dataset=iris

  #' Split data into features (all except species) and speciesClass
  features=iris[,1:4]
  speciesClass=iris[,5]

  #' Create our GMM model.
  #' We will look for three distinct groups in this data
  model <- Mclust(features, 3)

  #' The Mclust class has a plot function. Use it
  #' to plot the results.
  plot(model, what = "classification", main = "GMM Classification")

  #' Create a matrix showing how the discovered clusters match
  #' the species class. You can find the cluster number in
  #' model$classification
  species=unique(speciesClass)
  m=sapply(1:3,function(i){
    sapply(1:3,function(j) {
      length(intersect(
        which(speciesClass==species[i]),which(model$classification==j)))
    })
  })

  #' Give the columns and rows appropriate names and display the matrix
  colnames(m)=c("Cluster 1","Cluster 2","Cluster 3")
  rownames(m)=species
  print(m)
}
