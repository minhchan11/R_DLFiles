# Data split into three subsets
holdoutValidationExample_2 = function () {
  library(utils)
  dataset=getBrainWeight()

  # Split data into three:
  #  - A training set called train with 60% of the data
  #  - A validation set called valid with 20% of the data
  #  - A test set called test with 20% of the data
  # Use a single sample statement to get a vector of random row indices
  lngth=nrow(dataset)
  rnd_indices=sample(lngth)
  pnt1=floor(lngth*.6)
  pnt2=floor(lngth*.8)
  train=dataset[rnd_indices[1:pnt1],]
  valid=dataset[rnd_indices[(pnt1+1):pnt2],]
  test=dataset[rnd_indices[(pnt2+1)]:lngth,]

  #' Create regression models of Brain_Weight based on all
  #' other variables, using the train data
  #' Note that Gender and Age_Range are factors.
  #' - An OLS model, with additive factors
  #' - A Poisson regression model
  #' - A Polynomial regression model, with a polynomial expansion of 2
  #' only on the Head_Size feature.
  #' - A neural network model with 4 hidden nodes
  models=list()
  models[[1]]=lm(Brain_Weight~.,train)
  models[[2]]=glm(Brain_Weight~.,"poisson",train)
  models[[3]]=lm(Brain_Weight~Gender+Age_Range+poly(Head_Size,degree=2),train)
  models[[4]]=nnet(Brain_Weight~.,train,size=4,linout=T)
  # Return these models in an unnamed list called models

  #' Evaluate the MSE for each of the models on the validation
  #' data and return the best model as best
  mses=sapply(models,function(m){
    p=NULL
    if (class(m)[1]=="glm")
      p=predict(m,valid,type="response")
    else
      p=predict(m,valid)
    mean((p-valid$Brain_Weight)^2)
  })
  b=which.min(mses)
  best=models[[b]]

  cat("The best model was: ")
  if (b==1)
    cat("OLS\n") else if (b==2)
    cat("Poisson regression\n") else if (b==3)
    cat("polynomial regression\n") else
    cat("neural network\n")

  #' Evaluate the MSE for the selected model (best)
  #' on the test data
  #' Return this as test_mse
  p=NULL
  if (class(best)[1]=="glm")
    p=predict(best,test,type="response") else
    p=predict(best,test)
  test_mse=mean((p-test$Brain_Weight)^2)

  #' Plot the full data set (data) Brain_Weight~Head_Size with
  #' colors for the different data points depending on the values
  #' of Gender and Age_Range as below for the regression curves.
  #' Then plot the regression curves the chosen model for different
  #' factor values. These curves should be:
  #'   Gender   Age_Range
  #'     1          1           red
  #'     1          2           blue
  #'     2          1           green
  #'     2          2           black
  #' Plot the curves from min(Head_Size)-1 to max(Head_Size)+1
  gender=which(colnames(dataset)=="Gender")
  age_range=which(colnames(dataset)=="Age_Range")
  cols=apply(dataset,1,function(datum){
    if (datum[gender]==1 && datum[age_range]==1)
      "red"
    else if (datum[gender]==1 && datum[age_range]==2)
      "blue"
    else if (datum[gender]==2 && datum[age_range]==1)
      "green"
    else
      "black"
  })
  plot(Brain_Weight~Head_Size,dataset,col=cols)
  predict_=function(dataset_) {
    dataset_$Gender=as.factor(dataset_$Gender)
    dataset_$Age_Range=as.factor(dataset_$Age_Range)
    if (class(best)[1]=="glm")
      predict(best,dataset_,type="response")
    else
      predict(best,dataset_)
  }
  f=function(x,G,A) {
    n=length(x)
    df=data.frame(Head_Size=x,Gender=rep(G,n),Age_Range=rep(A,n))
    predict_(df)
  }
  x_min=min(dataset$Head_Size)-1
  x_max=max(dataset$Head_Size)+1
  x_seq=seq(x_min,x_max,(x_max-x_min)/100)
  points(x_seq,f(x_seq,1,1),type="l",col="red")
  points(x_seq,f(x_seq,1,2),type="l",col="blue")
  points(x_seq,f(x_seq,2,1),type="l",col="green")
  points(x_seq,f(x_seq,2,2),type="l",col="black")

  legend(3800,1300, # places a legend at the appropriate place
         c("Male 20-46","Male 46+","Female 20-46","Female 46+"), # puts text in the legend
         lty=rep(1,4), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5),
         col=c("red","blue","green","black"))

}

getBrainWeight=function() {
  #  Data reproduced here for local use from  http://www.stat.ufl.edu/~winner/data/brainhead.dat
  #
  #' Variables / Values
  #' Gender /  1=Male, 2=Female
  #' Age Range  / 1=20-46, 2=46+
  #' Head size (cm^3)
  #' Brain weight (grams)
  v=c(1,1,4512,1530,
      1,1,3738,1297,
      1,1,4261,1335,
      1,1,3777,1282,
      1,1,4177,1590,
      1,1,3585,1300,
      1,1,3785,1400,
      1,1,3559,1255,
      1,1,3613,1355,
      1,1,3982,1375,
      1,1,3443,1340,
      1,1,3993,1380,
      1,1,3640,1355,
      1,1,4208,1522,
      1,1,3832,1208,
      1,1,3876,1405,
      1,1,3497,1358,
      1,1,3466,1292,
      1,1,3095,1340,
      1,1,4424,1400,
      1,1,3878,1357,
      1,1,4046,1287,
      1,1,3804,1275,
      1,1,3710,1270,
      1,1,4747,1635,
      1,1,4423,1505,
      1,1,4036,1490,
      1,1,4022,1485,
      1,1,3454,1310,
      1,1,4175,1420,
      1,1,3787,1318,
      1,1,3796,1432,
      1,1,4103,1364,
      1,1,4161,1405,
      1,1,4158,1432,
      1,1,3814,1207,
      1,1,3527,1375,
      1,1,3748,1350,
      1,1,3334,1236,
      1,1,3492,1250,
      1,1,3962,1350,
      1,1,3505,1320,
      1,1,4315,1525,
      1,1,3804,1570,
      1,1,3863,1340,
      1,1,4034,1422,
      1,1,4308,1506,
      1,1,3165,1215,
      1,1,3641,1311,
      1,1,3644,1300,
      1,1,3891,1224,
      1,1,3793,1350,
      1,1,4270,1335,
      1,1,4063,1390,
      1,1,4012,1400,
      1,1,3458,1225,
      1,1,3890,1310,
      1,2,4166,1560,
      1,2,3935,1330,
      1,2,3669,1222,
      1,2,3866,1415,
      1,2,3393,1175,
      1,2,4442,1330,
      1,2,4253,1485,
      1,2,3727,1470,
      1,2,3329,1135,
      1,2,3415,1310,
      1,2,3372,1154,
      1,2,4430,1510,
      1,2,4381,1415,
      1,2,4008,1468,
      1,2,3858,1390,
      1,2,4121,1380,
      1,2,4057,1432,
      1,2,3824,1240,
      1,2,3394,1195,
      1,2,3558,1225,
      1,2,3362,1188,
      1,2,3930,1252,
      1,2,3835,1315,
      1,2,3830,1245,
      1,2,3856,1430,
      1,2,3249,1279,
      1,2,3577,1245,
      1,2,3933,1309,
      1,2,3850,1412,
      1,2,3309,1120,
      1,2,3406,1220,
      1,2,3506,1280,
      1,2,3907,1440,
      1,2,4160,1370,
      1,2,3318,1192,
      1,2,3662,1230,
      1,2,3899,1346,
      1,2,3700,1290,
      1,2,3779,1165,
      1,2,3473,1240,
      1,2,3490,1132,
      1,2,3654,1242,
      1,2,3478,1270,
      1,2,3495,1218,
      1,2,3834,1430,
      1,2,3876,1588,
      1,2,3661,1320,
      1,2,3618,1290,
      1,2,3648,1260,
      1,2,4032,1425,
      1,2,3399,1226,
      1,2,3916,1360,
      1,2,4430,1620,
      1,2,3695,1310,
      1,2,3524,1250,
      1,2,3571,1295,
      1,2,3594,1290,
      1,2,3383,1290,
      1,2,3499,1275,
      1,2,3589,1250,
      1,2,3900,1270,
      1,2,4114,1362,
      1,2,3937,1300,
      1,2,3399,1173,
      1,2,4200,1256,
      1,2,4488,1440,
      1,2,3614,1180,
      1,2,4051,1306,
      1,2,3782,1350,
      1,2,3391,1125,
      1,2,3124,1165,
      1,2,4053,1312,
      1,2,3582,1300,
      1,2,3666,1270,
      1,2,3532,1335,
      1,2,4046,1450,
      1,2,3667,1310,
      2,1,2857,1027,
      2,1,3436,1235,
      2,1,3791,1260,
      2,1,3302,1165,
      2,1,3104,1080,
      2,1,3171,1127,
      2,1,3572,1270,
      2,1,3530,1252,
      2,1,3175,1200,
      2,1,3438,1290,
      2,1,3903,1334,
      2,1,3899,1380,
      2,1,3401,1140,
      2,1,3267,1243,
      2,1,3451,1340,
      2,1,3090,1168,
      2,1,3413,1322,
      2,1,3323,1249,
      2,1,3680,1321,
      2,1,3439,1192,
      2,1,3853,1373,
      2,1,3156,1170,
      2,1,3279,1265,
      2,1,3707,1235,
      2,1,4006,1302,
      2,1,3269,1241,
      2,1,3071,1078,
      2,1,3779,1520,
      2,1,3548,1460,
      2,1,3292,1075,
      2,1,3497,1280,
      2,1,3082,1180,
      2,1,3248,1250,
      2,1,3358,1190,
      2,1,3803,1374,
      2,1,3566,1306,
      2,1,3145,1202,
      2,1,3503,1240,
      2,1,3571,1316,
      2,1,3724,1280,
      2,1,3615,1350,
      2,1,3203,1180,
      2,1,3609,1210,
      2,1,3561,1127,
      2,1,3979,1324,
      2,1,3533,1210,
      2,1,3689,1290,
      2,1,3158,1100,
      2,1,4005,1280,
      2,1,3181,1175,
      2,1,3479,1160,
      2,1,3642,1205,
      2,1,3632,1163,
      2,2,3069,1022,
      2,2,3394,1243,
      2,2,3703,1350,
      2,2,3165,1237,
      2,2,3354,1204,
      2,2,3000,1090,
      2,2,3687,1355,
      2,2,3556,1250,
      2,2,2773,1076,
      2,2,3058,1120,
      2,2,3344,1220,
      2,2,3493,1240,
      2,2,3297,1220,
      2,2,3360,1095,
      2,2,3228,1235,
      2,2,3277,1105,
      2,2,3851,1405,
      2,2,3067,1150,
      2,2,3692,1305,
      2,2,3402,1220,
      2,2,3995,1296,
      2,2,3318,1175,
      2,2,2720, 955,
      2,2,2937,1070,
      2,2,3580,1320,
      2,2,2939,1060,
      2,2,2989,1130,
      2,2,3586,1250,
      2,2,3156,1225,
      2,2,3246,1180,
      2,2,3170,1178,
      2,2,3268,1142,
      2,2,3389,1130,
      2,2,3381,1185,
      2,2,2864,1012,
      2,2,3740,1280,
      2,2,3479,1103,
      2,2,3647,1408,
      2,2,3716,1300,
      2,2,3284,1246,
      2,2,4204,1380,
      2,2,3735,1350,
      2,2,3218,1060,
      2,2,3685,1350,
      2,2,3704,1220,
      2,2,3214,1110,
      2,2,3394,1215,
      2,2,3233,1104,
      2,2,3352,1170,
      2,2,3391,1120)
  m=matrix(v,ncol=4,byrow=T)
  df=as.data.frame(m)
  colnames(df)=c("Gender","Age_Range","Head_Size","Brain_Weight")
  df[,1]=as.factor(df[,1])
  df[,2]=as.factor(df[,2])
  return(df)
}
