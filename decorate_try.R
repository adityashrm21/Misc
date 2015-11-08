#trying to implement the DECORATE algorithm in R
#reference - http://www.cs.utexas.edu/~ml/papers/decorate-proposal-03.pdf

setwd("~/Desktop/F/data science/decorate_try")

plot(0,0, xlim=c(1,150), ylim=c(1,100), xlab="Number of training examples", ylab="Accuracy" ,type="b")
#
z=5
while(z<=140)
{
library(rpart)
h=sample(nrow(iris),z)
train=iris[h,]
test=iris[-h,]
actual=test$Species
test$Species=NULL
cases=z
Csize=25
I=50
R=0.8
name=c("setosa","versicolor", "virginica")
normalize<-function(df)
{
  for( i in 1:nrow(df))
  {
    for(j in 1:ncol(df))
    {
      df[i,j]=df[i,j]/sum(df[i,])    
    }
  }
  df
}

inverse_prob<-function(df)
{
  dat=data.frame()
  for( i in 1:nrow(df))
  {
    for(j in 1:ncol(df))
    {
      dat[i,j]=(1/df[i,j])/sum(1/df[i,])    
    }
  }
  dat
}

error<-function(pred)
{
  e=0
  p=apply(pred,1,which.max)
  ans=name[p]
  for (i in 1:cases)
  {
     if(ans[i]==train$Species[i])
       e=e+1
  }
  e
}

generate<-function(mean, std)
{
  size=R*cases
  rand=rnorm(1000,mean,std)
  k=sample(rand,size)
  k
}

new_addition<-function(train)
{
  dat=data.frame()
  dat=generate(mean(train[,1]),sd(train[,1]))
  for(i in 2:(ncol(train)-1))
  {
    dat=cbind(dat,generate(mean(train[,i]),sd(train[,i])))
    dat[,i]
  }
  dat
}

i=1
trials=1

Cstar=rpart(Species~., data=train)
pred=predict(Cstar,train[,1:4], type="prob")
pred.cstar=predict(Cstar,test)
epsilon=error(pred)
#accuracy1=prop.table(table(actual==name[apply(pred,c,1,which.max)]))[2]



while(i<Csize & trials<I)
{
  add=new_addition(train)
  colnames(add)=c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")
  add=as.data.frame(add)
  pred.test=predict(Cstar,add)
  pred.test[pred.test==0]=0.0000001
  
  pred.test=normalize(pred.test)
  pred.test.new=inverse_prob(pred.test)
  p=apply(pred.test.new,1,which.max)
  ans=name[p] 
  add$Species=ans
  train=rbind(train,add)
  
  C=rpart(Species~.,data=train)
  pred.c=predict(C,test)
  p1=apply(pred.c,1,which.max)
  
  #accuracy2=prop.table(table(actual==name[p1]))[2]
  
  #if(accuracy2>accuracy1)
  #{
  #  accuracy1=accuracy2
  #  pred.cstar=(pred.c+pred.cstar)/2
  #}
  
  pred.cstar=(pred.c+pred.cstar)/2
  train=train[1:z,]
  pred1=predict(C,train[,1:4], type="prob")
  #pred2=predict(Cstar,train, type="prob")
  epsilon.star=error(pred1)
  if(epsilon.star<epsilon)
  {
    i=i+1
    epsilon=epsilon.star
  } else
  {
    pred.cstar=2*pred.cstar-pred.c
  }
  trials=trials+1
}

points(z,prop.table(table(actual==name[apply(pred.cstar,1,which.max)]))[2]*100)
z=z+10
}
