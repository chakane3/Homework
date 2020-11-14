# Libraries needed: none
# Files required: TraderJoes.csv
# Make sure you put this file in the same directory as this file

# In this file we are going to cluster the data in the TraderJoes file, based on the
# features Longitude and Latitude


#--Read the data
dat <- read.csv("/Users/chakaneshegog/Desktop/HW4:445001/Part1/TraderJoes.csv", header=TRUE)
cat("Preview the data:\n")
print(head(dat))


#--Plot original data
plot(dat$Longitude, dat$Latitude, col="blue", xlab="Longitude", ylab="Latitude", pch=18, 
     main = "scatter plot") 


#I. K-MEANS
set.seed(-50)
k = 8;
readline(paste("\nPress enter for k-means with", k, "clusters..."))
clustk <- kmeans(dat[ , c("Longitude", "Latitude")], k)
    

#print clusters centers and sizes
clustk_output <-data.frame(Cluster = seq(1, k), Size=clustk[["size"]], clustk[["centers"]], Whithin_cluster_SS = clustk$withinss)
names(clustk_output)[3] <- "Center_Longitude"
names(clustk_output)[4] <- "Center_Latitude" 
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
  legend(clustk_output[i,"Center_Longitude"], clustk_output[i,"Center_Latitude"], 
         legend = as.character(i), bty="n", text.col = colors[i], xjust = 0.5, yjust = 0.5)
}


#-- The elbow method
readline(paste("\nPress enter for the elbow method..."))
set.seed(60)
k_values = seq(2, 20);
wss_values <- c();

for (k in k_values){
  clustk <- kmeans(dat[ , c("Longitude", "Latitude")], k)
  wss_values <- c(wss_values, clustk$tot.withinss)  #whithin cluster sum of squares
}
plot(k_values, wss_values,type="b", pch = 19,
     xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

