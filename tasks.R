# Exploring Supervised Machine Learning

# Importing the dataset
library(readxl)
mydata=read_xlsx(file.choose())
View(mydata)
str(mydata)
mydata$Scores=as.integer(mydata$Scores)
summary(mydata)

library(ggplot2)
ggplot(mydata,aes(Hours,Scores))+geom_point()+ggtitle("Scores vs Hours")
barplot(Hours,Scores,data=mydata)
plot(mydata,col=" blue",main="Scores vs Hours")
abline(model,col="red")
plot(model)

library(DataExplorer)
plot_missing(mydata)
# Splitting the dataset into training data and test data
#install.packages('caTools')
library(caTools)
split=sample(2,nrow(mydata),replace=T,prob=c(0.7,0.3))
train=mydata[split==1,]
test=mydata[split==2,]
dim(train)
dim(test)
# Fitting Simple Linear Regression to the Training set
model=lm(Scores~Hours,data=train)
summary(model)
pred=predict(model,train)
pred
dataF=data.frame(train,pred)
dataF
comparison=data.frame(actual_scores=train$Scores,predcited_scores=pred)
comparison
library(ModelMetrics)
rmse(train$Scores,pred)
pred_test=predict(model,test)
pred_test
df=data.frame(test,pred_test)
df
df1=data.frame(actual_scores=test$Scores,predicted_scores=pred_test)
df1
rmse(test$Scores,pred_test)
# based on the coefficient of the model
# predict the score of the student when he studies 9.25 hrs/day
coef(model)[1] + 9.25*coef(model)[2]

