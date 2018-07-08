principleComponentExample_1=function(){
  # Prepare data and libraries
  library(car)
  library(utils)
  data(Duncan)
  dataset=Duncan[2:4]

  #' dataset contains three numeric variables: education,
  #' income and prestige. Calculate the all-but-one
  #' cross-validation MSEs for 5 algorithms:
  #'  1- An OLS of prestige~income
  #'  2- An OLS of prestige~education
  #'  3- An OLS of prestige~income+education
  #'  4- An OLS of prestige~PC1
  #'  5- An OLS of prestige~PC1+PC2
  #'
  #' Where PC1 and PC2 are the first and second principle
  #' components of income and education.
  #'
  #' Note that each iteration of the all-but-one cross-validation
  #' is a validation with separate training and validation data.
  #' You are not allowed to 'peek' at the validation data when
  #' building the model. This means that the principle components
  #' cannot be calculated once using all rows and used for all
  #' iterations of the cross-validation. Rather, they must
  #' be calculated each cross-validation iteration, using only the
  #' training data of that iteration. Then the hold out data
  #' must be projected onto the principle components
  #' discovered from the training data.
  #'
  #' Return the cross-validation MSEs as a list called mses.
  #' Entries should be in the order the models are listed in above.
  #'
  #' PCA Hint 1: You should always perform PCA on scaled data.
  #' prcomp will be default center but not scale the data! The
  #' reason poor default setting is backwards compatability
  #' with an old S function. (S was the programming language R
  #' was based on)
  #'
  #' PCA Hint 2: when you call prcomp, the data is projected onto
  #' the calculated principle components in the field $x. In this
  #' matrix the columns are called PC1, PC2, etc.
  #'
  #' PCA Hint 3: You can call predict on the object returned by
  #' prcomp to project new data onto the principle components.
  #' You will need to match column names when you do this.
  f1_res=sapply(1:nrow(dataset),function(i){
    model=lm(prestige~income,dataset[-i,])
    predict(model,dataset[i,])-dataset$prestige[i]
  })
  f2_res=sapply(1:nrow(dataset),function(i){
    model=lm(prestige~education,dataset[-i,])
    predict(model,dataset[i,])-dataset$prestige[i]
  })
  f12_res=sapply(1:nrow(dataset),function(i){
    model=lm(prestige~.,dataset[-i,])
    predict(model,dataset[i,])-dataset$prestige[i]
  })
  pc1_res=sapply(1:nrow(dataset),function(i){
    pca=prcomp(dataset[-i,1:2],scale=TRUE)
    i_=predict(pca,dataset[i,1:2])
    df=data.frame(pca$x,prestige=dataset[-i,3])
    model=lm(prestige~PC1,df)
    df2=data.frame(i_,prestige=dataset[i,3])
    predict(model,df2)-dataset$prestige[i]
  })
  pc12_res=sapply(1:nrow(dataset),function(i){
    pca=prcomp(dataset[-i,1:2],scale=TRUE)
    i_=predict(pca,dataset[i,1:2])
    df=data.frame(pca$x,prestige=dataset[-i,3])
    model=lm(prestige~.,df)
    df2=data.frame(i_,prestige=dataset[i,3])
    predict(model,df2)-dataset$prestige[i]
  })

  res=list(f1_res,f2_res,f12_res,pc1_res,pc12_res)
  mses=sapply(res,function(r)mean(r^2))

  #' Create a barplot of the cross-validation mses of the
  #' algorithms. Give the plot the title "CV-MSES" and
  #' give the bars the names "inc.","edu.","inc.+edu.","PC1",
  #' and "PC1+PC2"
  barplot(mses,
          names.arg = c("inc.","edu.","inc.+edu.","PC1","PC1+PC2"),
          main="CV-MSE")
}
