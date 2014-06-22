Predicting exercise activity
========================================================

Source data
-----------

The data come from http://groupware.les.inf.puc-rio.br/har.

Read training and test data from csv source files


```r
ptrain <- read.csv("C:/Users/piisptau/Downloads/pml-training.csv", header=TRUE, stringsAsFactors=F)

ptest <- read.csv("C:/Users/piisptau/Downloads/pml-testing.csv", header=TRUE, stringsAsFactors=F)
```

Data preparation
----------------

Reduce the training and test sets by selecting only the following 54 columns. Columns with NA values are omitted and only numeric columns are included in the reduced set.



```r
trainbaseset = ptrain[,c("num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm", "gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y",   "gyros_dumbbell_z", "accel_dumbbell_x","accel_dumbbell_y",  "accel_dumbbell_z",  "magnet_dumbbell_x","magnet_dumbbell_y", "magnet_dumbbell_z",  "roll_forearm","pitch_forearm","yaw_forearm",
"total_accel_forearm","gyros_forearm_x", "gyros_forearm_y","gyros_forearm_z","accel_forearm_x","accel_forearm_y",  "accel_forearm_z", "magnet_forearm_x", 
"magnet_forearm_y","magnet_forearm_z","classe")]

testing = ptest[,c("num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm", "gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y",   "gyros_dumbbell_z",  "accel_dumbbell_x","accel_dumbbell_y",  "accel_dumbbell_z",  "magnet_dumbbell_x","magnet_dumbbell_y", "magnet_dumbbell_z",  "roll_forearm","pitch_forearm","yaw_forearm",
"total_accel_forearm","gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x","accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", 
"magnet_forearm_y","magnet_forearm_z")]
```

Divide training set into two parts, 50% of the set model training and rest for validation.



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

Making the model
----------------

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
## Summary of sample sizes: 7358, 7359, 7360, 7359 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.002        0.003   
##   30    1         1      0.002        0.002   
##   50    1         1      0.002        0.003   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

Model results
-------------

Test the model on the validation set and get model accuracy from confusionMatrix output as 0.997. 


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
##          A 2788    1    0    0    1
##          B    8 1887    2    1    0
##          C    0    2 1708    1    0
##          D    0    0   10 1598    0
##          E    0    1    0    2 1800
## 
## Overall Statistics
##                                         
##                Accuracy : 0.997         
##                  95% CI : (0.996, 0.998)
##     No Information Rate : 0.285         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.996         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.997    0.998    0.993    0.998    0.999
## Specificity             1.000    0.999    1.000    0.999    1.000
## Pos Pred Value          0.999    0.994    0.998    0.994    0.998
## Neg Pred Value          0.999    0.999    0.999    1.000    1.000
## Prevalence              0.285    0.193    0.175    0.163    0.184
## Detection Rate          0.284    0.192    0.174    0.163    0.183
## Detection Prevalence    0.284    0.193    0.174    0.164    0.184
## Balanced Accuracy       0.998    0.998    0.996    0.998    1.000
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


