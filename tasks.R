# Exploring Supervised Machine Learning

# Importing the dataset
library(readxl)
mydata=read_xlsx(file.choose())
View(mydata)
str(mydata)

#converting scores variable into interger type
mydata$Scores=as.integer(mydata$Scores)
summary(mydata)

#data visualization
library(ggplot2)
ggplot(mydata,aes(Hours,Scores))+geom_point()+ggtitle("Scores vs Hours")
barplot(Hours,Scores,data=mydata)
plot(mydata,col=" blue",main="Scores vs Hours")
abline(model,col="red")
plot(model)


# plot for identifying missing values in the dataset
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

#predicting the test set output
pred=predict(model,train)
pred
dataF=data.frame(train,pred)
dataF


#comparing actual scores with predicted scores
comparison=data.frame(actual_scores=train$Scores,predcited_scores=pred)
comparison

#calculating root mean square error onn train and test data
library(ModelMetrics)
rmse(train$Scores,pred)
pred_test=predict(model,test)
pred_test
df=data.frame(test,pred_test)
df
df1=data.frame(actual_scores=test$Scores,predicted_scores=pred_test)
df1
# actual vs predicted graph
x=c(21,47,75,30,85,41,95)
y=c(28.60815,53.42983,85.88896,38.15495,78.25152,47.70175, 89.70768)
plot(x, type = "o", col = "red",  
     xlab = "Hours", ylab = "Scores ",  
     main = "Actual vs predicted") 

lines(y, type = "o", col = "blue")
legend(1,95, legend=c("actual", "predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
rmse(test$Scores,pred_test)

# based on the coefficient of the model
# predict the score of the student when he studies 9.25 hrs/day
coef(model)[1] + 9.25*coef(model)[2]

#student who studied for 9.25 hrs will score 93.04906 

