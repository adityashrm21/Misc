# trying to implement the SWITCH ensemble technique in R and comparing the accuracy changes
# http://www.public.asu.edu/~huanliu/papers/ecml04.pdf

setwd("~/Desktop/F/data science/Switch_try")

library('ElemStatLearn')
library("klaR")
library("caret")

sub = sample(nrow(spam), floor(nrow(spam) * 0.9))
train = spam[sub,]
test = spam[-sub,]

xTrain = train[,-58]
yTrain = train$spam

xTest = test[,-58]
yTest = test$spam

model = NaiveBayes(xTrain,yTrain,type="raw")

t1=prop.table(table(predict(model,xTest)$class==yTest))

library(mclust)
m=Mclust(xTrain, G=6)

train$cat=m[[14]]
xTrain$cat=m[[14]]
xtrn=c()
ytrn=c()
k=1
for(i in 1:5)
{
  for(j in i+1:6)
  {
    xtrn[[k]]=xTrain[xTrain$cat==i | xTrain$cat==j,]
    ytrn[[k]]=yTrain[xTrain$cat==i | xTrain$cat==j]
    k=k+1
  }
}
mod=c()
for(i in 1:15)
{
  xtrn[[i]]$cat=NULL
  mod[[i]] = train(xtrn[[i]],ytrn[[i]],'nb',trControl=trainControl(method='cv',number=10))
  
}
xTrain$cat=NULL
pred=c()
for( i in 1:15)
{
  pred[[i]]=predict(mod[[i]], xTrain)
}

  train$pseudo1=0
  train$pseudo2=0
  train$pseudo3=0
  train$pseudo4=0
  train$pseudo5=0
  train$pseudo6=0
  train$pseudo7=0
  train$pseudo8=0
  train$pseudo9=0
  train$pseudo10=0
  train$pseudo11=0
  train$pseudo12=0
  train$pseudo13=0
  train$pseudo14=0
  train$pseudo15=0
  
  

  train$pseudo1[pred[[1]]==yTrain]=1
  train$pseudo2[pred[[2]]==yTrain]=1
  train$pseudo3[pred[[3]]==yTrain]=1
  train$pseudo4[pred[[4]]==yTrain]=1
  train$pseudo5[pred[[5]]==yTrain]=1
  train$pseudo6[pred[[6]]==yTrain]=1
  train$pseudo7[pred[[7]]==yTrain]=1
  train$pseudo8[pred[[8]]==yTrain]=1
  train$pseudo9[pred[[9]]==yTrain]=1
  train$pseudo10[pred[[10]]==yTrain]=1
  train$pseudo11[pred[[11]]==yTrain]=1
  train$pseudo12[pred[[12]]==yTrain]=1
  train$pseudo13[pred[[13]]==yTrain]=1
  train$pseudo14[pred[[14]]==yTrain]=1
  train$pseudo15[pred[[15]]==yTrain]=1
  
  k=1
  for(i in 1:5)
  {
    for(j in i+1:6)
    {
      xtrn[[k]]=train[train$cat==i | train$cat==j,]
      ytrn[[k]]=yTrain[train$cat==i | train$cat==j]
      k=k+1
    }
  }
  for(i in 1:15)
    xtrn[[i]]$cat=NULL
  
  
    mod1 = NaiveBayes(train[,1:57],as.factor(train$pseudo1),type="raw")
    mod2 = NaiveBayes(train[,1:57],as.factor(train$pseudo2),type="raw")
    mod3 = NaiveBayes(train[,1:57],as.factor(train$pseudo3),type="raw")
    mod4 = NaiveBayes(train[,1:57],as.factor(train$pseudo4),type="raw")
    mod5 = NaiveBayes(train[,1:57],as.factor(train$pseudo5),type="raw")
    mod6 = NaiveBayes(train[,1:57],as.factor(train$pseudo6),type="raw")
    mod7 = NaiveBayes(train[,1:57],as.factor(train$pseudo7),type="raw")
    mod8 = NaiveBayes(train[,1:57],as.factor(train$pseudo8),type="raw")
    mod9 = NaiveBayes(train[,1:57],as.factor(train$pseudo9),type="raw")
    mod10 = NaiveBayes(train[,1:57],as.factor(train$pseudo10),type="raw")
    mod11 = NaiveBayes(train[,1:57],as.factor(train$pseudo11),type="raw")
    mod12 = NaiveBayes(train[,1:57],as.factor(train$pseudo12),type="raw")
    mod13 = NaiveBayes(train[,1:57],as.factor(train$pseudo13),type="raw")
    mod14 = NaiveBayes(train[,1:57],as.factor(train$pseudo14),type="raw")
    mod15 = NaiveBayes(train[,1:57],as.factor(train$pseudo15),type="raw")
    
    prd1=predict(mod1, xTest)
    prd2=predict(mod2, xTest)
    prd3=predict(mod3, xTest)
    prd4=predict(mod4, xTest)
    prd5=predict(mod5, xTest)
    prd6=predict(mod6, xTest)
    prd7=predict(mod7, xTest)
    prd8=predict(mod8, xTest)
    prd9=predict(mod9, xTest)
    prd10=predict(mod10, xTest)
    prd11=predict(mod11, xTest)
    prd12=predict(mod12, xTest)
    prd13=predict(mod13, xTest)
    prd14=predict(mod14, xTest)
    prd15=predict(mod15, xTest)
    
    ph1=prd1$posterior[,2]
    ph1=cbind(ph1,prd2$posterior[,2])
    
    ph1=cbind(ph1,prd3$posterior[,2])
    ph1=cbind(ph1,prd4$posterior[,2])
    ph1=cbind(ph1,prd5$posterior[,2])
    ph1=cbind(ph1,prd6$posterior[,2])
    ph1=cbind(ph1,prd7$posterior[,2])
    ph1=cbind(ph1,prd8$posterior[,2])
    ph1=cbind(ph1,prd9$posterior[,2])
    ph1=cbind(ph1,prd10$posterior[,2])
    ph1=cbind(ph1,prd11$posterior[,2])
    ph1=cbind(ph1,prd12$posterior[,2])
    ph1=cbind(ph1,prd13$posterior[,2])
    ph1=cbind(ph1,prd14$posterior[,2])
    ph1=cbind(ph1,prd15$posterior[,2])
    
    py1=predict(mod[[1]],xTest,type="prob")
    py2=predict(mod[[2]],xTest,type="prob")
    py3=predict(mod[[3]],xTest,type="prob")
    py4=predict(mod[[4]],xTest, type="prob")
    py5=predict(mod[[5]],xTest,type="prob")
    py6=predict(mod[[6]],xTest, type="prob")
    py7=predict(mod[[7]],xTest,type="prob")
    py8=predict(mod[[8]],xTest, type="prob")
    py9=predict(mod[[9]],xTest,type="prob")
    py10=predict(mod[[10]],xTest, type="prob")
    py11=predict(mod[[11]],xTest,type="prob")
    py12=predict(mod[[12]],xTest, type="prob")
    py13=predict(mod[[13]],xTest,type="prob")
    py14=predict(mod[[14]],xTest, type="prob")
    py15=predict(mod[[15]],xTest,type="prob")

    ans=py1*ph1[,1]+py2*ph1[,2]+py3*ph1[,3]+py4*ph1[,4]+py5*ph1[,5]+py6*ph1[,6]+py7*ph1[,7]+py8*ph1[,8]+py9*ph1[,9]+py10*ph1[,10]+py11*ph1[,11]+py12*ph1[,12]+py13*ph1[,13]+py14*ph1[,14]+py15*ph1[,15]
    
    ans$q="email"
    
    for( i in 1:length(ans$email))
    {
      if(ans$email[i]<ans$spam[i])
        ans$q[i]="spam"
    }
  
    t2=prop.table(table(ans$q==yTest))
    
#comparing predictions we see that accuracy improves from 68.98% to 85.68%
    t1 
    t2
    
