library(caret)
library(kernlab)
library(reshape2)
library(ggplot2)
data(spam)

# Data Splitting
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
names(training)

# Scatterplot matrix pairs
featurePlot(x = training[,c("charExclamation","charDollar","capitalTotal","money")],
            y = training$type,
            plot="pairs",
            main="Scatterplot Matrix Pairs of Selected Email Attributes")

## Reshape data wide -> long
long <- melt(training[,c("capitalTotal","type")])

remove_outlier <- subset(long, value<=5000)
ggplot(remove_outlier, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=type)) + 
  ggtitle("Total Number of Capital Letters by Email Type")

boxplot.stats(remove_outlier$value[remove_outlier$type=="spam"])
# lower whisker, 1st quartile, median, 3rd quartile, upper whisker 

boxplot.stats(remove_outlier$value[remove_outlier$type=="nonspam"])
# lower whisker, 1st quartile, median, 3rd quartile, upper whisker 

# Fit a model
set.seed(32343)
modelFit <- train(type~., data=training, method="rf")

# Final model
modelFit$finalModel

# Prediction
predictions <- predict(modelFit, newdata=testing)
predictions

# Confusion Matrix
confusionMatrix(predictions, testing$type) #94.09% accuracy
