#
# Prepared by Joseph B. Rickert
# Technical Marketing Manager Revolution Analytics
# for Webinar
# June 5, 2012
#
# This script assumes that the data have been read in during the
# Explore script

############## KMEANS ##############################################
# Look at kmeans cluster
# kmeans works on numeric variables
# First we build a data frame with only the numeric variables
numvars <- lapply(weather,is.numeric)  		
numdata <- na.omit(weather[,numvars==TRUE])
head(numdata)
#-----------------------------------------------------------------
# Run the kmeans algorithm
km <- kmeans(x=numdata, centers=10) 
# Plot the first 5 variables colored by cluster
vars <- 1:5
plot(numdata[vars], col=km$cluster)
title(main="weather",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Note in the plot: "left" variable is on the y axis
#                   "under" variable is on the x axis


############# HIERARCHICAL CLUSTERING ################################
# Function to produce a hierarchical correlation plot
# Follows code on page 135 of Data Mining with Rattle and R
# Note that int the plot shorter lengths correspond to higher correlations

cc <- cor(numdata,use="pairwise",method="pearson")	# compute distance matrix
hc <- hclust(dist(cc),method="average")				# Run the hclust

# Produce a basic plot
dn <- as.dendrogram(hc)
#plot(dn,horiz=TRUE)
# produce a fancier plot
op <- par(mar = c(3, 4, 3, 4.29))
plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), 
                                      pch = 21:22, bg=  c("light blue", "pink"), lab.cex = 0.75, lab.col = "tomato"), 
     edgePar = list(col = "gray", lwd = 2), xlab="Height")
title(main="Correlation Clusters using Pearson method")
sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"))
par(op)
