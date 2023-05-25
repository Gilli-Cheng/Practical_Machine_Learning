# Practical_Machine_Learning

This is a assignment from Practical Machine Learning on Coursera, thank you for review my work!!

web version : https://rpubs.com/GillianCheng/1045656



## Overview

This is an assignment from Practical Machine Learning from JHU on coursera, this report will use data from Weight Lifting Exercise Dataset, which includes data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. 

Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

Since there are a lot of predictors and categorical outcome, I choose _decision tree_, _boosting_, and _random forest_ to build up the model.

Training dataset will be further separate into actual training data and validation data, final model will be apply on the training data in the end for prediction.

reference : http:/groupware.les.inf.puc-rio.br/har#weight_lifting_exercises#ixzz4Tjq1wXIK


## Data
The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.


### Getting and Cleaning Data
Let's start with grabbing the data online and cleaning it!
```{r,message=FALSE}
url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# i have a quick view of the original dataset
training <- read.csv(url1, na.strings = c("", NA, "#DIV/0!"))
testing <- read.csv(url2, na.strings = c("", NA, "#DIV/0!"))
training$classe <- as.factor(training$classe)

library(caret)
# omit all the NA columns
# you can also use -nearZeroVar(training)
training <- training[,colSums(is.na(training)) == 0]
training <- subset(training, select = -c(1:7))

testing <- testing[,colSums(is.na(testing)) == 0]
testing <- testing[,-c(1:7)]
```

Separate the training data into actual training data and validation data.
```{r,message=FALSE}
set.seed(2023)
inTrain <- createDataPartition(training$classe, p = 0.75, list = F)
trainsub <- training[inTrain,]
testsub <- training[-inTrain,]
```

we can see correalation between the variable though `corrplot()`
```{r, fig.align='center', fig.dim=c(8,6)}
library(ggplot2);library(corrplot)
corrplot(cor(trainsub[,-c(53)]),method = "circle",
         type = "lower",tl.cex = 0.6, tl.col = "black")
```



### Tree
Let's start with decision tree, set the method to "rpart".
We can see _roll_belt<131_, _pitch_forearm<0.34_, _magnet_dumbbell_y<440_, and _roll_forearm<124_ variables are used to build up the classification.

WE GOT **0.50** ACCURACY FROM DECISION TREE.

```{r, message=FALSE, fig.align='center', fig.dim=c(8,6)}
set.seed(2023)
tree.model <- train(classe ~ ., data = trainsub, method = "rpart")

# plot the decision tree
library(rattle)
fancyRpartPlot(tree.model$finalModel)

# prediction and accuracy
pred <- predict(tree.model, testsub)
confusionMatrix(pred, as.factor(testsub$classe))$overall["Accuracy"]
```

### Boosting
Further building the second model by `gbm()`, the prediction of the gbm are probability, so some formatting are needed.

WE GOT **0.82** ACCURACY FROM BOOSTING, HIGHER THAN THE DECISION TREE!!

ps: it doesn't work with caret method = "gbm", does anyone know why?
```{r, message=FALSE, warning=FALSE}
set.seed(2023)
library(gbm)
gbm.model <- gbm(formula = classe ~ . , distribution = "multinomial",
                 data = trainsub, verbose = F,n.trees = 100)

# set type = "response" for probs!!
gbm.pred <- predict(gbm.model, testsub, type = "response", n.trees = 100)

# note that gbm predict returns the probability,
# by using which.max() choosing the highest prob of classe
pred_class <- apply(gbm.pred, 1, which.max)
pred_class <- as.factor(pred_class)
levels(pred_class) <- c("A","B","C","D","E")

confusionMatrix(pred_class, testsub$classe)$overall["Accuracy"]
```

### Random Forest
Finally the last model, random forest, which mostly perform the highest accuracy but easily to get overfitted in the short side.

WE GOT **0.99** ACCURACY FROM RANDOM FOREST!!THE HIGHEST!!

```{r, message=FALSE}
set.seed(2023)
library(randomForest)
rf.model <- randomForest(classe ~ ., data = trainsub, method = "class", 
                         ntree = 50, mtry = 17,do.trace = F, proximity = T, importance = T)
# do.trace = T to see how it iterate

pred <- predict(rf.model, testsub, type = "class")

confusionMatrix(pred, as.factor(testsub$classe))$overall[1]
```
Let's have a look on error rate of each classe and OOB(Out-of-bag) error.

OOB is the mean prediction error on each training sample xᵢ, using only the trees that did not have xᵢ in their bootstrap sample. ---from wiki

error rate lower as the number of trees grown more, it seems ntree = 40 will reach the lowest error rate.
```{r, fig.align='center', fig.dim=c(8,6)}
plot(rf.model)
legend("topright", colnames(rf.model$err.rate),col=c("black","#DF536B","#61D04F","#2297E6","#28E2E5","#CD0BBC"),cex=0.8,fill=c("black","#DF536B","#61D04F","#2297E6","#28E2E5","#CD0BBC"))
```


### The final prediction

```{r}
final.predict <- predict(rf.model, testing, type = "class")

final.predict
```

