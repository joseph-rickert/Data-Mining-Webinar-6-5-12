#
# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for Webinar
# June 5, 2012
#
#library(rattle)
#library(ada)
#library(colorspace)

# Boost model with ada boost and rpart
# Boosting is an ensememble method meta-algorithm similar to random forests
# It works by iterating through models, each time increasing the weights
# of observations that were misclassified
# Boosting apparently works best with "weak learners", algorithms that are
# not much better than random guessing.
#-------------------------------------------------------------------------
# This script assuumes that the weather data has been read in to a 
# data frame called weather, and that this data with the first two
# columns removed were put into the data frame called "data"
#-----------------------------------------------------------------------------
# Select variables for the model
# It is assumed that the vectors train and test that contain the indices to be used
# to build the training and test data sets have already been built.
#
#
#------------------------------------------------------------
# Build the Ada Boost model.
ada.model <- ada(RainTomorrow ~ .,
                 data=data[train,],
                 control=rpart.control(maxdepth=30,
                                       cp=0.010000,
                                       minsplit=20,
                                       xval=10),
                 iter=50)     # iter specifies number of trees to build

# Print the results of the modelling.
print(ada.model)

#Call:
#ada(RainTomorrow ~ ., data = data[train, ], control = rpart.control(maxdepth = 30, 
#cp = 0.01, minsplit = 20, xval = 10), iter = 50)
#
#Loss: exponential Method: discrete   Iteration: 50 
# (The algorithm is minimizing an exponential loss function)
#Final Confusion Matrix for Data:
#Final Prediction
#True value  No Yes
#No  215   0
#Yes  17  24
#
#Train Error: 0.066 
#Out-Of-Bag Error:  0.102  iteration= 31 (suggested number of iterations based on
#                                         training error and error adjusted by
#                                         the kappa statistic)
#Additional Estimates of number of iterations:
#train.err1 train.kap1 
#37         37 (kappa statistic adjusts for unbalanced target variable)
#--------------------------------------------------------------
# Look at variables used in the construction of the model
print(sort(names(listAdaVarsUsed(ada.model))))
#
# Plot the error rate as we increase the number of trees.
plot(ada.model)
# Plot variable importance
# Note the x access is relative improvement in accuracy
# The x axis is the improvement in accuracy  gained from including
# the variable named. Read the chart from lower left to upper right.
# Williams notes (p275) that there may be a bias in favor of 
# categorical variables. So he discounts their importance. 
varplot(ada.model)
