# MCfinal

# Predict activity quality from activity monitors

## Synopsis

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The goal of this project is to predict the manner in which they did the exercise. This is the `classe` variable in the training set.

## Data description
The outcome variable is `classe`, a factor variable with 5 levels. For this data set, participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in 5 different fashions:

1. Exactly according to the specification (Class A)
2. Throwing the elbows to the front (Class B)
3. Lifting the dumbbell only halfway (Class C)
4. Lowering the dumbbell only halfway (Class D)
5. Throwing the hips to the front (Class E)

## Prepare R envrioment

```{r message=FALSE}
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
```

## Setting the seed for repreducability

```{r}
set.seed(929)
```

## Reading the Data 

```{r cache=TRUE}
training.file   <- './data/pml-training.csv'
test.cases.file <- './data/pml-testing.csv'
training.url    <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
test.cases.url  <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
```

- Creating Directories

```{r}
if (!file.exists("data")){
  dir.create("data")
}
if (!file.exists("data/submission")){
  dir.create("data/submission")
}
```

- Download data

```{r message=FALSE, warning=FALSE, cache=TRUE}
download.file(training.url, training.file)
download.file(test.cases.url,test.cases.file )
```

## Data prossecing 

In this section some basic transformations and cleanup will be performed, so that:
1. NA values are omitted. 
2. Irrelevant columns such as `user_name`, `raw_timestamp_part_1`, `raw_timestamp_part_2`, `cvtd_timestamp`, `new_window`, and `num_window` (columns 1 to 7) will be removed in the subset.

### Here we determine how the NA strings in the data exist.
```{r}
training <-read.csv(training.file, na.strings=c("NA","#DIV/0!", ""))
testing <-read.csv(test.cases.file , na.strings=c("NA", "#DIV/0!", ""))
```

### Cleaning the data

```{r}
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]
```

### Subset the data

```{r}
training <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]
```

- Now if we check for NA we find that there is no any.
```{r}
sum(is.na(training))
sum(is.na(testing))
```

- Check if there are any Variables having nearly zero variance.

```{r}
non_zero_var <- nearZeroVar(training)
print(non_zero_var)
```

## Cross-validation

- Here we are splitting the data into a training set and a testing set
```{r}
training.index = createDataPartition(y = training$classe,
                                     p = 3/4, list = F)
sub.training = training[training.index,]
sub.testing = training[- training.index,]
```

## Exploratory analysis

### Dimensions of Dataset

```{r}
dim(sub.training); dim(sub.testing)
```

-  Here we have 53 variable, but we need to investigate the variable `classe` that we will predict later.

```{r}
summary(sub.training$classe)
```

We see that `classe` is a character variable we want to predict, to do that we need to convert `classe` into a factor variable.

### Levels of the `Classe`

```{r}
sub.training$classe = as.factor(sub.training$classe)
sub.testing$classe = as.factor(sub.testing$classe)
str(sub.training$classe)
```

Now the `classe` is a factor with 5 levels, to see how frequently every level occur we apply the `summary` function as we did above.

```{r}
summary(sub.training$classe)
```

### `Classe` Distribution

Lets now take a look at the number of instances (rows) that belong to each class. We can view this as an absolute count and as a percentage.

```{r}
#summarize the class distribution
percentage <- prop.table(table(sub.training$classe)) * 100
cbind(Freq=table(sub.training$classe), Percentage=percentage)
```

- We can see that class A has the highest number of instances (28.4% of the dataset). And the other classes are close to each other in their percentage.

- The variable `classe` contains 5 levels. The plot of the outcome variable shows the frequency of each levels in the `sub.training` data.

```{r}
plot(sub.training$classe, col="blue4",
     main="Levels of Classe",
     xlab="Classe levels",
     ylab="Frequency")
```

- The plot above shows that Level A is the most frequent `classe`. And D is the least frequent one.

## Expected out-of-sample error

- The **expected out-of-sample error** will correspond to: (1 - accuracy) in the cross-validation data.
- Accuracy is the proportion of correct classified observation divided by the total sample in the `sub.training` data set. Expected accuracy is the expected accuracy in the out-of-sample data set (i.e. original testing data set).
Thus, the expected value of the out-of-sample error will correspond to the expected number of misclassified observations/total observations in the Test data set, which is the quantity: (1 - accuracy) found from the cross-validation data set.

## Prediction Models

- In this section 2 types of models will be applied:
1. Decision Tree.
2. Random forest


### Decision tree

- First we will fit the model

```{r}
fit.dt <- rpart(classe ~ .,
                data= sub.training,
                method="class")
```

- Second we will preform a predictions in `sub.testing` data.

```{r}
predict.dt <- predict(fit.dt, sub.testing,
                      type = "class")
```

- Third plotting the predictions

```{r}
rpart.plot(fit.dt, main="Classification Tree",
           extra=102, under=T, faclen=0)
```

- Finally the confusion matrix shows the errors of the prediction algorithm.

```{r}
confusionMatrix(predict.dt, sub.testing$classe)
```

### Random forest

Fitting the model

```{r}
fit.rf <- randomForest(classe ~ ., data=sub.training,
                       method="class")
```

- Performing a prediction

```{r}
predict.rf <- predict(fit.rf, sub.testing, type = "class")
```

- Following confusion matrix shows the errors of the prediction algorithm.

```{r}
confusionMatrix(predict.rf, sub.testing$classe)
```

## Conclusion

- From the confusion matrices we can conclude that the Random Forest algorithm performs better than decision trees algorithm. And the accuracy of the Random Forest model is 0.9947 (95% CI: (0.9922, 0.9965)) compared to 0.7237 (95% CI: (0.7109, 0.7362)) for Decision Tree model. This huge difference in accuracy force us to go with the `rf` algorithm.

- The Expected out-of-sample error is defined as 1 - accuracy, so it's equal to:

```{r}
1- 0.9947
```
So with a so small out-of-sample error we expect that a very few observations will be misclassified.

## Prediction

- Here we will use the model we chose to predict the data in the `testing` data set.

```{r}
rf.predictions = predict(fit.rf, testing, type="class")
print(rf.predictions)
```



