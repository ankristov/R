# https://coreysparks.github.io/blog/demographic-modeling-cluster-analysis/
# We will discuss Unsupervised Learning or  the situation where you are looking for groups in your data when your data don’t come with a group variable. I.e. sometimes you want to find groups of similar observations, and you need a statistical tool for doing this.
# In statistics, this is called Cluster analysis
# What is Cluster Analysis:
# - Attempts to find sub-groups within a data set
# - Observations within a particular sub-gruop are statistically more similar to other members of their sub-group than to members of another sub-group
# - Many ways in which to do this:
#   * K-means/K-medioids
#   * Hierarchical clustering
#   * Model based clustering
#   * Latent class analysis
# - All of these methods use observed data to measure the dissimilarity between observations, then create groups, or clusters (buckets) from these observations.
# Metric of similarity - distance based, most often - Euclidian distance: dist(xi, xj) = sqrt((xi-xj)'(xi-xj))

x1 <- c(1,5,1)
x2 <- c(5,1,2)
dist(x = rbind(x1,x2), method = 'euclidian')
sqrt( (x1[1] - x2[1])^2 +  (x1[2] - x2[2])^2 + (x1[3] - x2[3])^2)

x3 <- c(1,7,2)
dist(x = rbind(x1,x2,x3), method = 'euclidian')

x4 <- c(11,7,6)
dist(x = rbind(x1,x2,x3, x4), method = 'euclidian')


#######################################
# Load & Preprocessing Data
#######################################


# load data
library(readr)
#columns types are stored here: read_rds(url("https://raw.githubusercontent.com/coreysparks/r_courses/master/prbspec.rds"))
prb <- read_csv(file = 'data/PRB2008_All.csv',
                col_types = cols(
                  Y = col_integer(),
                  X = col_integer(),
                  ID = col_integer(),
                  Country = col_character(),
                  Continent = col_character(),
                  Region = col_character(),
                  Year = col_integer(),
                  Population. = col_double(),
                  CBR = col_integer(),
                  CDR = col_integer(),
                  Rate.of.natural.increase = col_double(),
                  Net.Migration.Rate = col_integer(),
                  ProjectedPopMid2025 = col_double(),
                  ProjectedPopMid2050 = col_double(),
                  ProjectedPopChange_08_50Perc = col_integer(),
                  IMR = col_double(),
                  WomandLifeTimeRiskMaternalDeath = col_integer(),
                  TFR = col_double(),
                  PercPopLT15 = col_integer(),
                  PercPopGT65 = col_integer(),
                  e0Total = col_integer(),
                  e0Male = col_integer(),
                  e0Female = col_integer(),
                  PercUrban = col_integer(),
                  PercPopinUrbanGT750k = col_integer(),
                  PercPop1549HIVAIDS2001 = col_double(),
                  PercPop1549HIVAIDS2007 = col_double(),
                  PercMarWomContraALL = col_integer(),
                  PercMarWomContraModern = col_integer(),
                  PercPpUnderNourished0204 = col_double(),
                  MotorVehper1000Pop0005 = col_integer(),
                  PercPopwAccessImprovedWaterSource = col_integer(),
                  GNIPPPperCapitaUSDollars = col_integer(),
                  PopDensPerSqKM = col_integer(),
                  PopDensPerSqMile = col_double()))
#prb <- read_csv(file = 'https://raw.githubusercontent.com/coreysparks/data/master/PRB2008_All.csv',
#                col_types = read_rds(url("https://raw.githubusercontent.com/coreysparks/r_courses/master/prbspec.rds")))
head(prb)

library(dplyr)
glimpse(prb)

# choose complete cases of variables of interest
dim(prb)
prb <- prb %>% select(IMR, TFR, PercPopLT15, e0Total, PercUrban, PercMarWomContraModern) %>%
  filter(complete.cases(IMR, TFR, PercPopLT15, e0Total, PercUrban, PercMarWomContraModern))
dim(prb)

# our data
head(prb)
knitr::kable(head(prb))
View(prb)

#######################################
# Data Partition 
#######################################
library(caret)
library(lattice)
library(ggplot2)

set.seed(1115)
in_train <- createDataPartition(y = prb$IMR, p = .80, list = F)
train <- prb[in_train,]; dim(train)
test <- prb[-in_train,]; dim(test)

#######################################
# Hierarchial clustering
#######################################
library(stats)
# First we form our matrix of distances between all the countries on our observed variables
dist_mat <- dist(train, method = 'euclidian')
dist_mat
# Then we run a hierarhical clustering algorithm on the matrix. There are lots of different ways to do this, we will just use the simplest method, the single-linkage, or nearest neighbor approach. This works by first sorting the distances from smallest to largest, then making clusters from the smallest distance pair.
# Once this is done, this pair is merged into a cluster, their distance is then compared to the remaining observations, so on and so on, until you have a set of clusters for every observation.
# The original way to plot these analyses is by a dendrogram, or tree plot.
fit_hc1 <- hclust(d = dist_mat, method = 'single')
plot(fit_hc1, hang = -1, main = 'Single linkage cluster analysis of PRB data')

install.packages('scorecard')
install.packages('factoextra')
library(scorecard)
library(factoextra)
library(class)
library(RColorBrewer)

fviz_dend(fit_hc1, k = 5, k_colors = brewer.pal(n=5, name='Accent'),
          color_labels_by_k = T, ggtheme = theme_minimal())

groups <- cutree(fit_hc1, k = 5)
table(groups)

# So this is silly because the method round 3 cluster that only had one observation. This is a weakness of the single linkage method, instead we use another method. Ward’s method is typically seen as a better alternative because it tends to find clusters of similar size.
fit_hc2 <- hclust(d = dist_mat, method = 'ward.D')
plot(fit_hc2, hang=-1, main = "Ward's cluster analysis of PRB data")
fviz_dend(fit_hc2, k = 5, k_colors = brewer.pal(n = 5, name = 'Accent'),
          color_labels_by_k = T, ggtheme = theme_minimal())
groups <- cutree(fit_hc2, k = 5)
table(groups)

# groups info -> to data
train$groups1 <- factor(groups)
ggplot(data = train) +
  geom_point(aes(x = IMR, y = TFR,  pch = groups1, color = groups1, cex = 3))

#######################################
# K-means
#######################################
# Another type of cluster finder. Will always find a given number of k clusters. Ideally we can minimize a within cluster variance measure to find the optimal number
train <- train %>% select(-groups1)
fit_km <- kmeans(x = train, centers = 3, nstart = 10)
names(fit_km)
fit_km

install.packages("ClusterR")
library(ClusterR)

fit_km2 <- KMeans_rcpp(data = train, cluster = 3, num_init = 10)

train$clusters <- as.factor(fit_km$cluster)

ggplot(data = train) +
  geom_point(aes(x = IMR, y = TFR, color = clusters, group = clusters, cex = 3)) 

# Finding optimal number of clusters
# Loop over 1 to 10 clusters and store the between group variances, then plot the relative differences. You are looking for the number of clusters where you see a shoulder in the plot.
ss <- NULL
for (i in 1:10){
  fit_km <- kmeans(x = train, nstart = 10, centers = i)
  ss[i] <- fit_km$betweenss / fit_km$totss
  #ss[i] <- fit_km$totss
  #ss[i] <- fit_km$tot.withinss / fit_km$totss
}
plot(x = 1 : length(ss), y = ss)
plot(x = 2 : length(ss), y = diff(ss))

# Looks like the difference in variances stops declining dramatically at k=3 clusters.
# Here are the test cases plotted as X’s

test$clusters <- as.factor(predict_KMeans(data = test, CENTROIDS = fit_km2$centroids))
#pred <- predict(fit_km, data = test)

ggplot(data = test) +
  geom_point(aes(x = IMR, y = TFR, group = clusters, color = clusters, cex = 2.5), pch = 'x') + 
  geom_point(data = train, aes(x = IMR, y = TFR, group = clusters, color = clusters))

