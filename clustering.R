######################
# K-Means Clustering #
######################

# Dataset

# The dataset is based on some measurements of various species of flowers.

# 1. Title: Iris Plants Database

# 2. Number of Instances: 150

# 3. Number of Attributes: 4 numeric, predictive attributes

# 4. Attribute Information:
#    - sepal length in cm
#    - sepal width in cm
#    - petal length in cm
#    - petal width in cm

# 5. Missing Attribute Values: None

# Loading libraries
library(cluster)
library(ggplot2)

# Importing the Iris dataset:
df <- read.csv("iris.csv")
head(df)

names(df)[1] <- "sepal_length"
head(df)

# Creating a Scatter Matrix (Pairplot) of the Dataset
# Where each of the 4 variables are plotted against one another
scatter.matrix<- pairs(df)

#scaling data set with built in function
df <- scale(df)
head(df)
pairs(df)

# Using the elbow method to find the optimal number of clusters
# The elbow method compare the Within Cluster Sum of Squares to the value of K
#setting seed for random generator and initialization of empty vector to store Within clusters sum of squares from loop 
set.seed(5)
wcss <- vector()

#populating wcss vector
for(i in 1:10){
  wcss[i] <- sum(kmeans(df, i)$withinss)
}

# Plotting the results:
plot(1:10, wcss, type = 'b', main = paste('Elbow method for clusters of Iris'), xlab = 'K - number of clusters', ylab = 'WCSS')

#from above plot I would consider 2, 3 or 4 number of clusters at first
#drop in WCSS from 2 to 3 clusters is still significant and from 3 clusters onward s we could actually fit line not indicative of 
#a big change in WCSS - curve is flattening
#my choice for K is 3!




# Fit K-Means to the Dataset with the best value for K and predict the clusters of the dataset

set.seed(23)
kmeans <- kmeans(df, 3, iter.max = 300, nstart = 10)

#adding prediction to dataframe
df <- cbind(df, kmeans$cluster)

#renaming new column
colnames(df)[5] <- "kmeans_predicted_cluster"

#back as dataframe 
df <- data.frame(df)

View(df)

# Visualization of  Cluster Membership

clusplot(df[,1:4],
         kmeans$cluster,
         lines = 0,       
         shade = TRUE,         
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Iris flower clusters:\n K-means clustering K=3.'),
         xlab = 'First component',
         ylab = 'Second component')



#scatter plots
#factorizing last column from numeric(better handled in plots)
df$kmeans_predicted_cluster <- as.factor(df$kmeans_predicted_cluster)


# sepal_length vs sepal_width 
ggplot(df)+
  geom_point(aes(x = sepal_length, y = sepal_width, color = kmeans_predicted_cluster) , size = 5)+ 
  labs(
    x = 'Sepal Length', 
    y = 'Sepal Width',
    title = "Cluster membership based on sepal length and width (standarized)",
    subtitle = "K-means clustering for K=3")

# petal_length vs petal_width  
ggplot(df)+
  geom_point(aes(x = petal_length, y = petal_width, color = kmeans_predicted_cluster) , size = 5)+ 
  labs(
    x = 'Petal Length',
    y = 'Petal Width',
    title = "Cluster membership based on sepal length and width (standarized)",
    subtitle = "K-means clustering for K=3")







###########################
# Hierarchical Clustering #
###########################


#dendrogram for the Scaled Data
dendrogram <- hclust(dist(df[,-5], method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'x',
     ylab = 'Euclidean Distances')

#K = 2
abline(h=100)

#K = 3
abline(h=30)



# According to theory:
# To find the optimal number of clusters we find the largest vertical(Euclidean) distance we can go without crossing any horizontal lines
# In this case we should cut at K = 2, sub-setting dataset into 2 clusters
K2_hierarchical_predicted_cluster <- as.factor(cutree(dendrogram, 2))
K2_hierarchical_predicted_cluster

# we can also check creating 3 clusters and compare it with previous analysis
K3_hierarchical_predicted_cluster <- as.factor(cutree(dendrogram, 3))
K3_hierarchical_predicted_cluster

df <- data.frame(cbind(df, K2_hierarchical_predicted_cluster, K3_hierarchical_predicted_cluster))
View(df)



# Visualization of Cluster Membership
# Create at least one scatter plot with two different variables with the cluster membership set as the color
clusplot(df[,1:4],
         K2_hierarchical_predicted_cluster,
         lines = 0,       
         shade = TRUE,         
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Iris flower clusters:\n Hierarchical clustering with cutoff at K = 2'),
         xlab = 'First component',
         ylab = 'Second component')

clusplot(df[,1:4],
         K3_hierarchical_predicted_cluster,
         lines = 0,       
         shade = TRUE,         
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Iris flower clusters:\n Hierarchical clustering with cutoff at K = 3'),
         xlab = 'First component',
         ylab = 'Second component')

##########################################plots cutoff 2

# sepal_length vs sepal_width 
ggplot(df)+
  geom_point(aes(x = sepal_length, y = sepal_width, color = K2_hierarchical_predicted_cluster) , size = 5)+ 
  labs(
    x = 'Sepal Length',
    y = 'Sepal Width',
    title = "Cluster membership based on sepal length and width (standarized)",
    subtitle = "Hierarchical clustering method, cutoff = 2")

# petal_length vs petal_width  
ggplot(df)+
  geom_point(aes(x = petal_length, y = petal_width, color = K2_hierarchical_predicted_cluster) , size = 5)+ 
  labs(
    x = 'Petal Length',
    y = 'Petal Width',
    title = "Cluster membership based on petal length and width (standarized)",
    subtitle = "Hierarchical clustering method, cutoff = 2")

##########################################plots cutoff 3

# sepal_length vs sepal_width 
ggplot(df)+
  geom_point(aes(x = sepal_length, y = sepal_width, color = K3_hierarchical_predicted_cluster) , size = 5)+ 
  labs(
    x = 'Sepal Length',
    y = 'Sepal Width',
    title = "Cluster membership based on sepal length and width (standarized)",
    subtitle = "Hierarchical clustering method, cutoff = 3")

# petal_length vs petal_width  
ggplot(df)+
  geom_point(aes(x = petal_length, y = petal_width, color = K3_hierarchical_predicted_cluster) , size = 5)+ 
  labs(
    x = 'Petal Length',
    y = 'Petal Width',
    title = "Cluster membership based on petal length and width (standarized)",
    subtitle = "Hierarchical clustering method, cutoff = 3")


##################################################################################################################
########################################################################################################


# Conclusions about agreement between the two solutions?

par(mfrow=c(1,3))

#clustering 1
clusplot(df[,1:4],
               kmeans$cluster,
               lines = 0,       
               shade = TRUE,         
               color = TRUE,
               labels = 4,
               plotchar = FALSE,
               span = TRUE,
               main = paste('Iris flower clusters:\n K-means clustering K=3.'),
               xlab = 'First component',
               ylab = 'Second component')

#clustering 3
clusplot(df[,1:4],
               K3_hierarchical_predicted_cluster,
               lines = 0,       
               shade = TRUE,         
               color = TRUE,
               labels = 4,
               plotchar = FALSE,
               span = TRUE,
               main = paste('Iris flower clusters:\n Hierarchical clustering with cutoff at K = 3'),
               xlab = 'First component',
               ylab = 'Second component')

#clustering 2
clusplot(df[,1:4],
               K2_hierarchical_predicted_cluster,
               lines = 0,       
               shade = TRUE,         
               color = TRUE,
               labels = 4,
               plotchar = FALSE,
               span = TRUE,
               main = paste('Iris flower clusters:\n Hierarchical clustering with cutoff at K = 2'),
               xlab = 'First component',
               ylab = 'Second component')

#I performed hierarchical clustering for K = 2 only because theoretically from Euclidean distance point
#of view that should be the optimal approach.
#Knowing the K number of clusters from Elbow method and also having some knowledge about most popular 
#dataset in biology, bioinformatics, and data science :) I would opt for 3 clusters to be modeled above
#In this case both strategies(K-means and hierarchical, K=2) seem to overlap at least visually
#Especially cluster no1 is well defined differing only by one data point from method to method
#If I compare cluster placement between 2 classification methods predicting the same number of clusters
#It does not look so good as only 69 out of 150 points has been assign the same cluster which is like 46%
#and most of the same prediction is within the first most obvious cluster I assume

table(as.numeric(df$kmeans_predicted_cluster) == as.numeric(df$K3_hierarchical_predicted_cluster))


#I think K-means approach is more precise as we do some pre calculation defining number of clusters for this method
#I forced subjectively K = 3 for hclustering but dendrogram suggested rather 2 clusters
# I believe there are 3 clusters observing behaviour of WCSS in elbow method and knowing history of dataset
# Although visually at first time you can clearly distinguish 2 and the difference between second and third
# cluster is not that obvious

