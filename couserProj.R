
  library(caret)

  train01<-read.csv("pml-training0.csv")
  test0<-read.csv("pml-testing0.csv")
  
  train3000<-sample(nrow(train01),3000)######从train中随机抽取3000行。
  train301<-train01[train3000,]  
  ####将train0的这3000行提取出来赋值给train3
  #dim(train301)    3000  56
  
  set.seed(1231)
  trControl = trainControl(method = "cv", number = 4)
  modFit1<-train(train301$classe~.,method="rf",data=train301)
  print(modFit1$finalModel)
  predict(modFit1,newdata=test0)
  ### B A B A A E D B A A B C B A E E A B B B
  
  
  set.seed(1232)
  trControl = trainControl(method = "cv", number = 4)
  modFit2<-train(train301$classe~.,method="rpart",data=train301)
  print(modFit2$finalModel)
  predict(modFit2,newdata=test0)
  ### C A C A A C C A A A C C C A C A A A A C
  
  
  train5000<-sample(nrow(train01),5000)######从train中随机抽取5000行。
  train302<-train01[train5000,]
  
  set.seed(1233)
  #trControl = trainControl(method = "cv", number = 4)
  modFit3<-train(train302$classe~.,method="rf",data=train302)
  print(modFit3$finalModel)
  predict(modFit3,newdata=test0)
  ####  B A B A A E D B A A B C B A E E A B B B
  
#   set.seed(1234)
#   modFit4<-train(train302$classe~.,method="gbm",data=train302)
#   #print(modFit4$finalModel)
#   predict(modFit4,newdata=test0)
#   
  
  set.seed(1235)
  modFit5<-train(train302$classe~.,method="rpart",data=train302)
 # print(modFit5$finalModel)
  predict(modFit5,newdata=test0)
  ### C A C A A C C A A A C C C A C A A A A C
  
  
  #### use PCA
    
  train02<-read.csv("pml-training.csv")
  test02<-read.csv("pml-testing.csv")
  for(i in 1:160)
    train02[,i]<-as.numeric(train02[,i])
  for(i in 1:160)
    test02[,i]<-as.numeric(test02[,i])
  
  
  
  preproc <- preProcess(train02, method='pca', thresh=0.99)
  train02.pca <- predict(preproc, train02[,-1]) 
  # make a reduced training set (exclude column 1)
  modelFit <- train(train02[[1]] ~ ., data=train02.pca, method='rf')
  
  # predicting
  test02.pca <- predict(preproc, test02)          # make a reduced testing set
  predictions <- predict(modelFit, test02.pca)
  
  
  
  
  
  
