# Understand the Data
Insurance_Policy <- read.csv("C:\\Users\\Jonathan\\Desktop\\Edureka\\Insurance_Policy.csv")
View(Insurance_Policy)
str(Insurance_Policy)
summary(Insurance_Policy)

# Install k-means packages
install.packages("animation")
library(animation)

# Select simularity criteria
Policy_Data <- Insurance_Policy[ , -3]
View(Policy_Data)

# scale the data
Policy_Data_Scale <- scale(Policy_Data)
View(Policy_Data_Scale)
Policy_Data_Scale <- as.data.frame(Policy_Data_Scale)

# Add weight
Policy_Data_Scale$Premiums.Paid <- Policy_Data_Scale$Premiums.Paid * 3
Policy_Data_Scale$Claims.made <- Policy_Data_Scale$Claims.made * 5
View(Policy_Data_Scale)

Policy_Data_Scale <- Policy_Data_Scale[ , -4]

# Optimal Number of clusters 
clust_data <- Policy_Data_Scale
wss <- (nrow(clust_data)-1)*sum(apply(clust_data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clust_data,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# 4

# Kmeans
set.seed(16)
clust_data_Km <- kmeans(clust_data, 4)

names(clust_data_Km)
clust_data_Km$betweenss
clust_data_Km$withinss
clust_data_Km$cluster

clust_data_Km

clust_data_Final <- cbind(Policy_Data, cluster_number = clust_data_Km$cluster)
View(clust_data_Final)

#Profiling


mean(clust_data_Final$Premiums.Paid)
median(clust_data_Final$Premiums.Paid)
summarize(clust_data_Final$Premiums.Paid, clust_data_Final$cluster_number, median)

median(clust_data_Final$age)
summarize(clust_data_Final$age, clust_data_Final$cluster_number, median)

median(clust_data_Final$Claims.made)
summarize(clust_data_Final$Claims.made, clust_data_Final$cluster_number, median)


