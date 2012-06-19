####### SETUP FOR SCRIPTS TO BE USED IN 
####### INTRODUCTIN TO R FOR DATA MINING WEBINAR
####### June 5, 2012 ############
#
####### LIBRARES USSED #######################################
library(rattle)  		# support functions and data mining GUI
library(Hmisc)
library(fBasics)		# for describe()
library(gplots)         # for barplot
library(vcd)			# for rainwow_hcl colors
library(ellipse)		# for correlation plot
library(rpart)			# CART, decision trees
library(ROCR)			# Visualize the performance of classifiers
library(party)			# ctree function
library(ada)			# Boosted models
library(colorspace)     # plot colors
library(randomForest)   # random forest algorithm
library(caret)			# functions for automating data mining process
library(kernlab)		# Support vector machines
library(e1071)			# support vector machines
library(doParallel)		# Parallel "backends"

######### SOME HELPER FUNCTIONS #################################################

## Helper function to calculate overall error 
overall <- function(x)
{
  if (nrow(x) == 2) 
    oe <- (x[1,2] + x[2,1]) / sum(x) 
  else
    oe <- 1 - (x[1,rownames(x)]) / sum(x)
  return(oe)
} 

# Little function to draw multiple histograms
# Examples of how you can string functions together in R
jplot <- function(var){
  hs <- hist(eval(parse(text=var)), main="", xlab=var, ylab="Frequency", 
             col="grey90", ylim=c(0, 90), breaks="fd", border=TRUE)
  dens <- density(eval(parse(text=var)), na.rm=TRUE)
  rs <- max(hs$counts)/max(dens$y)
  lines(dens$x, dens$y*rs, type="l", col=rainbow_hcl(1)[1])
  rug(eval(parse(text=var)))		          # Add a rug to the plot
  title(main=paste("Distribution of",var))  # Add a title to the plot.
}