iris=read.csv(file.choose())

library(tidyr)
#converting Species variable to factor
iris$Species=as.factor(iris$Species)
iris=iris[,-1]
# Load the Caret package which allows us to partition the data
library(caret)
# We use the dataset to create a partition (80% training 20% testing)
index <- createDataPartition(iris$Species, p=0.7, list=FALSE)
# select 30% of the data for testing
testset <- iris[-index,]
# select 70% of data to train the models
trainset <- iris[index,]
str(trainset)
prop.table(table(iris$Species))


library(ggplot2)
library(plotly)
#histogram
library(ggthemes)
histogram <- ggplot(data=iris, aes(x=SepalWidthCm)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  
  ylab("Frequency") + 
  ggtitle("Histogram of Sepal Width")+
  theme_economist()
print(histogram)

g=ggplot(data=trainset, aes(x =PetalLengthCm, y = PetalWidthCm))
g <-g + 
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Petal Length") +
  ylab("Petal Width") +
  ggtitle("Petal Length-Width")+
  geom_smooth(method="lm")
print(g)
 

#boxplot
box <- ggplot(data=trainset, aes(x=Species, y=SepalLengthCm)) +
  geom_boxplot(aes(fill=Species)) + 
  ylab("Sepal Length") +
  ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
print(box)

library(rpart.plot)
library(rpart)

# build the model
tree=rpart(Species~.,trainset,method="class",cp=0)
rpart.plot(tree)

#predict the model on unseen data(testdata)
predict_unseen <-predict(tree, testset, type = 'class')

table_mat <- table(testset$Species, predict_unseen)
table_mat

#lets check the accuracy
#It is the proportion of true positive and true negative over the sum of the matrix. 
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
#You can print the accuracy of the test set:
print(paste('Accuracy for test', accuracy_Test))
