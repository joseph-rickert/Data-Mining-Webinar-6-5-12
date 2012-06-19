# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for Webinar
# June 5, 2012
#
# TREE MODEL WITH RPART
#
# Algorithm based on recursive partitioning
# See section 11.2 of Data Mining with Rattle and R by Williams
# Partition the data set according to some criterion of "best" partition
# Do the same for each of the two new subsets
# Once a partition is made, stick with it (greedy approach)
# Measures of "best" partition:
#      (1) information gain (the default)
#      (2) Gini
# Information Gain Algorithm:
# For all possible splits (partitions)
#  Split data, D, into to subsets S1 and S2 where D = S1 U S2
#	Calculate information I1 and I2 associated with S1 and S2
#	Compute total information of split: Info(D,S1,S2) = (|D1|/D)*I1 + (|D2|/|D|)*I2
#	Compute the information gain of the split: info(D) - info(D,S1,S2)
#   Select split with greatest information gain
#----------------------------------------------------------------------------------
# Following code assumes that the weather data has been read in
#-----------------------------------------------------------------------------------
# Select variables for the model
data <- subset(weather,select=c(MinTemp:RainToday,RainTomorrow))
#-----------------------------------------------------------------------------------
# Select a subset for training
set.seed(42)									  # Set seed
N <- nrow(data)
train <- sample(N,0.7*N)						   # Pick out observations for training
test <-  setdiff(setdiff(seq_len(N),train), train) #  observations
#------------------------------------------------------------------------------------
# Build a classification tree model
form <- formula(RainTomorrow ~ .)				# Describe the model to R
model <- rpart(formula=form,data=data[train,])	# Build the model
#--------------------------------------------------------------------------------------
# Plot the Tree
# Rattle style plot
drawTreeNodes(model)							# From Rattle
title(main="Decision Tree weather.csv $ RainTomorrow",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
#------------------------------------------------------------------------------------
# 3 ways to examine the results
# str(model)
#model
#printcp(model)									# from rpart
summary(model)
# ------------------------------------------------------------------------------------
# find out in which leaf each observation ended up
leaf <-model$where								
leaf
#------------------------------------------------------------------------
# Evaluate performance
# Run the tree model on the validate set
pr <- predict(model, weather[test,], type="class")
# Generate the confusion matrix
AP <- c("Actual", "Predicted")				# row names for CM
CM <- table(weather[test,]$RainTomorrow,pr,dnn=AP) # CM counts
CMpct <- round(CM/length(pr),2)           # CM %
## Helper function to calculate overall error 
overall <- function(x)
{
  if (nrow(x) == 2) 
    oe <- (x[1,2] + x[2,1]) / sum(x) 
  else
    oe <- 1 - (x[1,rownames(x)]) / sum(x)
  return(oe)
} 

oe <- overall(CM)                         # overall error
CM;CMpct;oe

# -----------------------------------------------------------------------------------
# ROC Curve: requires the ROCR package.
# Generate an ROC Curve for the rpart model on weather.csv [validate].
# 
# Get vector RainTommorrow in validate data set
RT <- weather[test,]$RainTomorrow
prRT <- as.vector(as.integer(pr))
pred <- prediction(predictions=prRT, labels=RT) # prediction is a function from the ROCR package
#------------------------------------------------------------------------------------
# Plot the ROC curve
plot(performance(pred, "tpr", "fpr"), col="#CC0000FF", lty=1, lwd=2,add=FALSE)
#fpr: False positive rate. P(Yhat = + | Y = -). Estimated as: FP/N.
#tpr: True positive rate. P(Yhat = + | Y = +). Estimated as: TP/P.
segments(0,0,1,1,col="blue",lwd=2)	
# Add a legend to the plot.
legend("bottomright", c("tree.m"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))
# Add decorations to the plot.
title(main="ROC Curve forweather test set]",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
#











# Explore an unpruned tree
# The complexity parameter set the minimum benefit that must be
# gained at each split of the decision tree. (default = .01)
# Typical behavior with cp=0 is to see the error reate decrease
# at first and then begin to increase.
set.seed(41)
control <- rpart.control(minsplit=10,
                         minbucket=5,
                         maxdepth=20,
                         usesurrogate=0,
                         maxsurrogate=0,
                         cp=0,
                         minibucket=0)
model2 <- rpart(formula=form,control=control,data=data[train,])
print(model2$cptable)
plotcp(model2)
grid()