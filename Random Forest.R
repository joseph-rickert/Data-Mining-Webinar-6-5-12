#
##
# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for Webinar 
# June 5, 2012
#
# Random Forests is an "ensemble technique"
# Hundreds of decision trees are built and combined to produce a result
# RF (and other ensemble techniques) tend to be robust with respect to
# small changes in the data and with respect to noise. That is, small
# changes in the data set have little effect on the final result
# 
# Overview of RF algorithm
#    1) Deside on some large number of trees to build (e.g. N=500)
#    2) For each tree:
#         randomly select the observations to use ("bagging" - sampling with replacement)
#         for each split of the tree, randomly select a small set of predictor variables to use      
#         build each tree out to its maximum depth
#    3) Produce the final result by having each tree vote on the classification
# 
# Since each tree is fully built out individual trees tend to overfit
# But, by combining multiple trees results in less bias 
##------------------------------------------------------------------------------------
# libraries used in this script: rattle, colorspace, randomForest, and ROCR
##------------------------------------------------------------------------------------
# The script assumes that the weather data has been read into a data frame called weather
#---------------------------------------------------------------------------------------
#data <- subset(weather,select=c(MinTemp:RainToday,RainTomorrow))
##
#set.seed(42)  										# Set seed
#### Determined the observations for the training,validate and test datasets.
#N <- nrow(weather)										# 366 observations
#train <- sample(N, 0.7*N)								# 256 observations
#test <-    setdiff(setdiff(seq_len(N),train), train)    # 110 observation
#----------------------------------------------------------------------------
# Build the Random Forest Model
model.rf <- randomForest(RainTomorrow ~ .,
                         data=data[train,],			# use the training data
                         ntree=500,					# build 500 trees
                         mtry=4,						# use 4 variables at each split
                         importance=TRUE,			# keep track of variable importance
                         na.action=na.roughfix,		# impute missing values (median for numeric vars, most frequent for categorical vars)
                         replace=FALSE)				# sample with replacement
#
# Examine the model
model.rf								# print summary of model

# Confusion matrix is Actual (rows) vs Predicted (cols)
#OOB estimate of  error rate: 13.67% (Model is 86% accurate with observations not used to build the trees"
#Confusion matrix: (rows are actual numbers, columns are predicted values)
#No Yes class.error
#No  206   9  0.04186047  = 9/(206+9)    Model predicts rain when there is none 4%
#Yes  26  15  0.63414634  = 26/(26+15)   You get wet 63% of the time you follow the model's no rain prediction

#On 26 days model predicts no rain but it does rain
#Model predicts rain on 9 days but it does not rain
#
# Lool at 
head(model.rf$predicted)				# Value predicted for each observation in the training set
round(head(model.rf$err.rate,15),4)		# look at the error rate
min.err <- min(data.frame(model.rf$err.rate)["OOB"])	# find tree with minnimum error rate
min.err								# Minimum error rate
min.err.index <- which(data.frame(model.rf$err.rate)["OOB"]==min.err)
min.err.index							# Tree with minimum error

#-------------------------------------------------------------------------
# List the importance of the variables.
rn <- round(importance(model.rf), 2)
rn[order(rn[,3], decreasing=TRUE),]
#
# Plot variable importance
varImpPlot(model.rf, main="",col="dark blue")
title(main="Variable Importance Random Forest weather.csv",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))	
#--------------------------------------------------------------------------
# Plot the error rate against the number of trees.
plot(model.rf, main="")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest weather.csv",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# ------------------------------------------------------------------------
# Evaluate model performance on the Test set 
# -------------------------------------------------------------------------
# Run the tree model on the test set
pr <- predict(model.rf, data[test, ], type="class")
# -------------------------------------------------------------------------
# Generate the confusion matrix showing counts.
CM <- table(data[test, ]$RainTomorrow,pr,dnn=c("Actual", "Predicted"))
CM
# Generate the confusion matrix showing percentages.
CMpct <- round(CM/length(pr),2)
CMpct
# Calucate the overall error percentage.
OA <- overall(table(pr, weather[test, ]$RainTomorrow, dnn=c("Predicted", "Actual")))
OA
x <- CM[1,2]/(CM[1,1]+CM[1,2])
y <- CM[2,1]/(CM[2,1]+CM[2,2])
P1 <- paste("Model predicts rain when there is none",100*round(x,3),"% of time")
P2 <- paste("You get wet",100*round(y,3),"% of the time you follow the model's no rain prediction")
P1
P2
#
#----------------------------------------------------------------------------------------------
# Build a model from a balanced training set
model2.rf <- randomForest(RainTomorrow ~ .,
                          data=data[train,],			# use the training data
                          ntree=500,					# build 500 trees
                          mtry=4,						# use 4 variables at each split
                          importance=TRUE,			# keep track of variable importance
                          na.action=na.roughfix,		# impute missing values
                          sampsize=c(35,35),          # balance sample size to draw sanples with equal numbers of yes and no
                          replace=FALSE)				# sample with replacement
#
model2.rf

#OOB estimate of  error rate: 31.25%
#Confusion matrix:
#No Yes class.error
#No  146  69   0.3209302
#Yes  11  30   0.2682927
