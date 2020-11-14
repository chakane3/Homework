# Files required: FBS.csv
# Make sure you put this file in the same directory as this file

# In this file we are going to cluster the data in file FBS.csv, based on the
# features stadium capacity, latitude, longitude, endowment, and enrollment



#-- Read the data
# Edit: Used absolute path
dat <- read.csv("/Users/chakaneshegog/Desktop/HW4:445001/Part2/FBS.csv", header=TRUE)
cat("Preview the data:\n")
print(head(dat))


#-- Standardize the data
stddat <- dat
for (i in 2:(length(names(dat))-1)){
  stddat[[i]] <- ( dat[[i]] - mean(dat[[i]]) ) / sd(dat[[i]])
}

#--Plot original data
plot(dat$Longitude, dat$Latitude, col="blue", xlab="Longitude", ylab="Latitude", pch=18, 
     main = "scatter plot") 

# I. K-MEANS
set.seed(4)
k = 8
clustk <- kmeans(x = stddat[ , c(2:6)], k)

clustk_output <- data.frame(Cluster = seq(1, k), Size=clustk[["size"]], clustk[["centers"]], Whithin_cluster_SS = clustk$withinss)
cat("\nClustering output - standardized:\n")
print(clustk_output)

#plot clusters
clust1 <- clustk$cluster #cluster assignments
dat1 <- cbind(dat, cluster=clust1) #append the clusters info to dat

colors = c("blue", "red", "green", "grey", "olivedrab", "violet", "pink", "orange")
plot(dat1$Longitude, dat1$Latitude, col="white", xlab="Longitude", ylab="Latitude", 
       main=paste("clustered data using k-means:", k, "clusters"))



for (i in 1:k){
  #different clusters -> different colors
  points(dat[dat1$cluster==i,"Longitude"], dat[dat1$cluster==i,"Latitude"], col=colors[i], pch=18)
    
  #plot the centroids
  points(clustk_output[i,"Center_Longitude"], clustk_output[i,"Center_Latitude"], col=colors[i], pch=4) #different clusters -> different colors

}




#-- De-Standardize the data
for (i in 3:(length(names(clustk_output))-1)){
  clustk_output[[i]] <- clustk_output[[i]]*sd(dat[[i-1]]) + mean(dat[[i-1]])
}

cat("\nClustering output - non-standardized:\n")
print(clustk_output)
