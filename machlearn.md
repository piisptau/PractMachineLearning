Predicting exercise activity
========================================================

The data come from http://groupware.les.inf.puc-rio.br/har.

Read training and test data from csv source files


```r
ptrain <- read.csv("C:/Users/piisptau/Downloads/pml-training.csv", header=TRUE, stringsAsFactors=F)

ptest <- read.csv("C:/Users/piisptau/Downloads/pml-testing.csv", header=TRUE, stringsAsFactors=F)
```


Reduce the training and test sets by selecting only the following 54 columns


```r
trainbaseset = ptrain[,c("num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm", "gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z",
"roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y",   "gyros_dumbbell_z",  "accel_dumbbell_x",
"accel_dumbbell_y",  "accel_dumbbell_z",  "magnet_dumbbell_x","magnet_dumbbell_y", "magnet_dumbbell_z",  "roll_forearm","pitch_forearm","yaw_forearm",
"total_accel_forearm","gyros_forearm_x", "gyros_forearm_y",     "gyros_forearm_z",    "accel_forearm_x","accel_forearm_y",  "accel_forearm_z", "magnet_forearm_x", 
"magnet_forearm_y","magnet_forearm_z","classe")]

testing = ptest[,c("num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm", "gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z",
"roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y",   "gyros_dumbbell_z",  "accel_dumbbell_x",
"accel_dumbbell_y",  "accel_dumbbell_z",  "magnet_dumbbell_x","magnet_dumbbell_y", "magnet_dumbbell_z",  "roll_forearm","pitch_forearm","yaw_forearm",
"total_accel_forearm","gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x","accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", 
"magnet_forearm_y","magnet_forearm_z")]
```

Divide training set into two parts, 50& of the set model training and rest for validation.



```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
trainbaseset$classe = factor(trainbaseset$classe)

trainIndex = createDataPartition(y = trainbaseset$classe, p=0.5, list=FALSE) 

trainingSet = trainbaseset[trainIndex,]

validationSet = trainbaseset[-trainIndex,]
```

Add missing classe column to the test set


```r
classe = rep("x",20)

testSet = cbind(testing, classe)
```

Make a random forest model with 4-fold cross validation


```r
modFitRandFor <- train(trainingSet$classe ~.,data = trainingSet,method="rf", trControl=trainControl(method="cv", number=4))
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modFitRandFor
```

```
## Random Forest 
## 
## 9812 samples
##   53 predictors
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (4 fold) 
## 
## Summary of sample sizes: 7358, 7358, 7360, 7360 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.002        0.003   
##   30    1         1      0.003        0.004   
##   50    1         1      0.003        0.004   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

Test the model on the validation set and get model accuracy from confusionMatrix output as 0.9809. 


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
predValValid = predict(modFitRandFor, validationSet)
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
confusionMatrix(validationSet$classe, predValValid)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2789    1    0    0    0
##          B    8 1888    2    0    0
##          C    0    7 1704    0    0
##          D    0    0    7 1600    1
##          E    0    0    0    9 1794
## 
## Overall Statistics
##                                         
##                Accuracy : 0.996         
##                  95% CI : (0.995, 0.998)
##     No Information Rate : 0.285         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.995         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.997    0.996    0.995    0.994    0.999
## Specificity             1.000    0.999    0.999    0.999    0.999
## Pos Pred Value          1.000    0.995    0.996    0.995    0.995
## Neg Pred Value          0.999    0.999    0.999    0.999    1.000
## Prevalence              0.285    0.193    0.175    0.164    0.183
## Detection Rate          0.284    0.192    0.174    0.163    0.183
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       0.998    0.997    0.997    0.997    0.999
```

Make prediction from the test set


```r
predictRF = predict(modFitRandFor, testSet)

predictRF
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

These test set predictions were 100% accurate.


