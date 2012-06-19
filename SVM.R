#
# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for the webinar
# June 5, 2012
#
#### SVM MODEL ####
# This script begins by developing a Support Vector Machine model of the
# weather data set using the presentation in Chapter 14 of Data Mining with
# Rattle and R (Williams; SV 2011) as a guide
# After building a model on a training data set and evaluating its performance
# on a test data set, the script goes on to explor the automatic tuning
# capabilities for SVM models provided in the R package caret
#----------------------------------------------------------------
# libraries used: kernlab, caret, e1071
#-----------------------------------------------------------------
# This script assumes that the weather data has been read into a 
# data frame called weather
# -----------------------------------------------------------------
#data <- subset(weather,select=c(MinTemp:RainToday,RainTomorrow))
#set.seed(42)  					# Set seed
#### Determined the observations for the training,validate and test datasets.
#N <- nrow(weather)										# 366 observations
#train <- sample(N, 0.7*N)								# 256 observations
#test <-  setdiff(setdiff(seq_len(N),train), train)    # 110 observation
#--------------------------------------------------------------
# Build the svm
svm.model <- ksvm(RainTomorrow ~ .,
                  data=data[train,],
                  kernel="rbfdot",
                  prob.model=TRUE)

svm.model
#----------------------------------------------------------------------

testDF <- na.omit(data[test,])   # remove NAs from validattion data set
featuresTest <- testDF[,-21]			 # remove target variable
# Run the svm model on the validate set
pr <- predict(svm.model,featuresTest , "response") 
# Generate the confusion matrix
AP <- c("Actual", "Predicted")				 # row names for CM
CM <- table(testDF$RainTomorrow,pr,dnn=AP)   # CM counts
CMpct <- round(100*CM/length(pr))            # CM %									
oe <- overall(CM)							 # overall error
CM;CMpct;oe									 # print results
#
#       Predicted
#Actual No Yes
#No  82   2
#Yes 12   4

# overall error 0.14
#-------------------------------------------------------------------------
#  Try automatic model selection using the caret package
#  Note, currently automatic selection is limited to using numeric variables
#  Journal of Statistical Software November 2008, Vol 28, Issue 5
#
trainDF <- na.omit(data[train,])  # get rid of NAs to satisfy svm.predict
# Get indices of features that are not mumeric
dc <- lapply(data,class)
FacInd <- which(dc=="factor")
Xvars <- trainDF[,-FacInd]
Yvar  <- trainDF$RainTomorrow
bootcontrol <- trainControl(number = 200)
registerDoParallel(4)			 # Register a parallel backend
# for 4 cores
getDoParWorkers()
system.time(svmFit <- train(Xvars,Yvar,method="svmRadial",
                            tuneLength=5,
                            trControl=bootcontrol,
                            scaled=FALSE)
)
#
# Time for single core
#user  system elapsed 
#3.25    0.11  168.42 						
#
# Time for 4 cores
#user  system elapsed 
#3.57    0.08   70.41 				

svmFit
#> svmFit
#228 samples
#16 predictors
#2 classes: 'No', 'Yes' 
#
#No pre-processing
#Resampling: Bootstrap (200 reps) 
#
#Summary of sample sizes: 228, 228, 228, 228, 228, 228, ... 
#
#Resampling results across tuning parameters:
#
#C     Accuracy  Kappa  Accuracy SD  Kappa SD
#0.25  0.835     0      0.0318       0       
#0.5   0.835     0      0.0318       0       
#1     0.835     0      0.0318       0       
#2     0.835     0      0.0318       0       
#4     0.835     0      0.0318       0       
#
#Tuning parameter 'sigma' was held constant at a value of 0.0786
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were C = 0.25 and sigma = 0.0786. 
#-----------------------------------------------------------------------
#testDF <- na.omit(data[test,])  # get rid of NAs to satisfy svm.predict
## Get indices of features that are not mumeric
#dc <- lapply(data,class)
#FacInd <- which(dc=="factor")
#Xvars <- testDF[,-FacInd]
#Yvar  <- testDF$RainTomorrow

svmPred <- extractPrediction(list(svmFit),testX=Xvars,testY=Yvar)
svmPred2 <- svmPred[svmPred$dataType=="Test",]
confusionMatrix(svmPred2$pred,svmPred2$obs)
#------------------------------------------------------------------
#
# Build a confusion matrix by hand
pr2 <- predict(svmFit$finalModel, Xvars, "response") 
# Generate the confusion matrix
AP <- c("Actual", "Predicted")				# row names for CM
CM2 <- table(pr2,Yvar,dnn=AP)				# CM counts
CMpct2 <- round(CM2/length(pr2))            # CM %									
CM2;CM2pct2							        # print results
