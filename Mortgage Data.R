# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for Webinar: "Introduction to R for Data Mining"
# May, 2012
#
###### SCRIPT TO BUILD LOGISTIC REGRESSION MODEL TO PREDICT MORTGAGE DEFAULTS #####
#---------------------------------------------------------------------------
# Some subsidary functions
#---------------------------------------------------------------------------
# Function to compute a "long form" of the confusion matrix
Cmatrix <- function(df){
  df <- as.data.frame(df)
  df$Result <- c("True Negative","False Negative","False Positive","True Positive")
  df$PCT <- round(df$Counts/sum(df$Counts),2)*100
  df$Rates <- round(c(df$Counts[1]/(df$Counts[1]+df$Counts[3]),
                      df$Counts[2]/(df$Counts[2]+df$Counts[4]),
                      df$Counts[3]/(df$Counts[1]+df$Counts[3]),
                      df$Counts[4]/(df$Counts[2]+df$Counts[4])),2)
  names(df) <- c("Actual","Predicted","Counts","Results","Pct","Rates")
  return(df)
}
#------------------------------------------------------------------------------
# LOOK AT THE MORTGATE DEFAULT DATA
#----------------------------------
dataDir <- "C:/Users/Joseph/Documents/DATA/Mortgage Data/mortDefault"
mdata <- file.path(dataDir,"mortDefault.xdf")
rxGetInfo(mdata,getVarInfo=TRUE)

rxGetInfo(mdata, numRows=5)
#---------------------------------------------------------------------------
##### CREATE TRAINING AND TEST FILES
#-----------------------------------
#info <- rxGetInfo(mdata)
#N <- info$numRows
#
## Create a new data file having a variable with uniform random numbers
# going from 1 to 10. This variable will be used to create the training and test
# data sets.
# A little note on how the random numbers are created:
#		A transform should work on an arbitrary chunk of data.  Typically
#		RevoScaleR functions will test transforms on a small chunk before
#		fully processing. The internal variable (.rxNumRows) gives the size 
#		of the chunk.  

#rxDataStep(inData = mdata, outFile = "mortDefault2",
#transforms=list(urns = as.integer(runif(.rxNumRows,1,11))),
#overwrite=TRUE)
rxGetInfo("mortDefault2",getVarInfo=TRUE)
rxHistogram(~urns,data="mortDefault2")		#check to see if things look random
#-------------------------------------------------------------------------------
# BUILD THE TRAINING FILE
#------------------------
#rxDataStepXdf(inFile = "mortDefault2", outFile = "mdTrain",
#rowSelection = urns < 9, blocksPerRead=20,overwrite=TRUE )

rxGetInfo("mdTrain",getVarInfo=TRUE,numRows=5)
rxHistogram(~default,data="mdTrain")
#-------------------------
# BUILD THE TEST FILE
#-------------------------
#rxDataStepXdf(inFile = "mortDefault2", outFile = "mdTest",
#rowSelection = urns > 8, blocksPerRead=20,overwrite=TRUE )
#
rxGetInfo("mdTest",getVarInfo=TRUE,numRows=5)
rxHistogram(~default,data="mdTest")
#---------------------------------------------------------------------------
# BUILD A CLASSIFICATION MODEL USING LOGISTIC REGRESSION
#---------------------------------------------------------------------------
system.time(
  model <- rxLogit(default ~ F(houseAge) + F(year)+ creditScore + yearsEmploy + ccDebt, 
                   data="mdTrain", 
                   reportProgress=rxGetOption("reportProgress"), )
)
#	
#Elapsed computation time: 23.149 secs.
#user  system elapsed 
#56.81   10.58   23.17 
#Elapsed computation time: 24.384 secs.
#user  system elapsed 
#59.29   10.31   24.48 

summary(model)
#----------------------------------------------------------------------
# MAKE PREDICTIONS ON THE TEST DATA USING THE MODEL CREATED ABOVE
#----------------------------------------------------------------------
rxPredict(modelObject=model,data="mdTest",outData="mdTest",overwrite=TRUE)
rxGetInfo("mdTest",getVarInfo=TRUE,numRows=5)
#rxSummary(~default_Pred,data="mdTest")
# Add a new prediction variable
rxDataStep(inData="mdTest",outFile="mdTest",
           transforms=list(Pdefault = as.logical(round(default_Pred))),
           overwrite=TRUE)
#
rxGetInfo("mdTest",getVarInfo=TRUE,numRows=5)

#-------------------------------------------------------------------------------
# GENERATE THE CONFUSION MATRIX
#-------------------------------
conMc <- rxCube(~ F(default) + F(Pdefault),data="mdTest")
Cmatrix(conMc)
#-------------------------------------------------------------------------------

########### KMEANS ANALYSIS ###############################
#
form <- formula(~ creditScore + houseAge + yearsEmploy + ccDebt + year)
md.km <- rxKmeans(formula=form, data = mdata, 
                  numClusters = 2,
                  outFile = "mortDefault3",
                  algorithm = "lloyd",
                  overwrite=TRUE)
md.km
rxGetInfo("mortDefault3",getVarInfo=TRUE,numRows=5)	
# Put some data into a data frame to plot		
mdDf <- rxXdfToDataFrame(file="mortDefault3", 
                         varsToDrop = c("year"),
                         rowSelection=urns == 5,
                         maxRowsByCols = 1000)
vars <- 1:4				
plot(mdDf[vars],col=mdDf$.rxCluster)
title(main="Clusters in Mortgage Default Data",line=3)