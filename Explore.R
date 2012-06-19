# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for Introduction to R for Data Mining
# June 5, 2012
#
# READ IN THE DATA WE WILL BE WORKING WITH
# Read in the weather data set from the from disk
name <- "weather.csv"
dataDir <- "C:/Users/Joseph/Documents/DATA/Rattle Data"
file <- file.path(dataDir,name)
weather <- read.csv(file)

#-----------------------------------------------------------------
#
# Look at the file in the RPE editor  
#
#-----------------------------------------------------------------	
# Simplify the weather data set
weather <- weather[,-c(1,2)]
#-----------------------------------------------------------------
# Get a numerical summary
summary(weather)
#
describe(weather) #describe is in the Hmisc package
#
#-------------------------------------------------------------------------------------
# Look at the target variable
# Bar Plot 
# The 'gplots' package provides the 'barplot2' function.
# Generate the summary data for plotting.
ds <- summary(weather$RainTomorrow)
# Plot the data.
bp <-  barplot2(ds, beside=TRUE, 
                ylab="Frequency", 
                xlab="RainTomorrow", 
                ylim=c(0, max(ds)+15), 
                col="blue")
# Add the actual frequencies.
text(bp, ds+9, ds)
#---------------------------------------------------------------------
#
# Plot the distributions of some variables
# Look at all of the pieces that make a plot
hs <- hist(weather$Sunshine, main="", 
           xlab="Sunshine", ylab="Frequency", 
           col="grey90", ylim=c(0, 90), 
           breaks="fd", border=TRUE)
dens <- density(weather$Sunshine, na.rm=TRUE)
rs <- max(hs$counts)/max(dens$y)
lines(dens$x, dens$y*rs, type="l", col=rainbow_hcl(1)[1])
rug(weather$Sunshine)		          # Add a rug to the plot
title(main=paste("Distribution of","Sunshine"))  # Add a title to the plot.
#--------------------------------------------------------------------------------
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

call a custom function that does 4 plots
attach(weather)			# This will make the variables in weather available
# Plot some distributions using a custom function jplot
par(mfrow=c(2,2))
v <- c("Sunshine","WindGustSpeed","WindSpeed9am","WindSpeed3pm")
lapply(v,jplot)
detach(weather)
#-------------------------------------------------------------------------------------
# Look at correlations
# The 'ellipse' package provides the 'plotcorr' function
# Correlations work for numeric variables only.
numeric <- c("MinTemp", "MaxTemp", "Rainfall", "Evaporation",
             "Sunshine", "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
             "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
             "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")
#
cor <- cor(weather[,numeric], use="pairwise", method="pearson")
# Graphically display the correlations.
par(mfrow=c(1,1))
plotcorr(cor, col=colorRampPalette(c("red", "white", "blue"))(11)[5*cor + 6])
title(main="Correlation weather.csv using Pearson",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))