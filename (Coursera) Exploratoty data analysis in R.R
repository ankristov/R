# Edward Tufte (2006). Beatiful Evidence. Graphics Press LLC. www.edwardtufte.com

# The fine particle pollution (PM2.5), the annual mean averaged over 3 years cannot excees 12 mcg/m3
# Question: Are there any counties ini the US that exceed that national standard for fine particle pollution?

library(readr)
library(ggplot2)
pollution1999 <- read_csv("daily_88101_1999.csv")
head(pollution)
str()
View(pollution)
pollution1999 <- select(pollution, "County Code", "Site Num", "Latitude", "Longitude", "Date Local", "Arithmetic Mean", "State Name", "County Name")
pollution1999 <- rename(pollution1999, "pm25" = "Arithmetic Mean")
names(pollution1999) <- sub(" ", "_", names(pollution1999))
names(pollution1999) <- tolower(names(pollution1999))
summary(pollution1999$pm25)
boxplot(pollution1999$pm25, col = "blue")
abline(h = 12)
ggplot(pollution1999) + geom_boxplot(aes("1999", pm25)) + 
  geom_abline(aes(intercept = 12, slope = 0))
ggplot(pollution1999) + geom_boxplot(aes("1999", pm25)) +
  coord_cartesian(ylim = c(0,40))
hist(pollution1999$pm25, col = "green")
rug(pollution1999$pm25)
hist(pollution1999$pm25, col = "green", breaks = 100)
rug(pollution1999$pm25)
abline(v = 12, lwd = 2)
abline(v = median(pollution1999$pm25), col = "magenta", lwd = 4)
ggplot(pollution1999) + geom_histogram(aes(pm25), binwidth = 5) +
  geom_vline(aes(xintercept = median(pollution1999$pm25)), color = "magenta")
ggplot(pollution1999) + geom_histogram(aes(pm25), binwidth = 5) +
  coord_cartesian(xlim = c(0,50))
ggplot(pollution1999) + geom_bar(aes(state_name))

ggplot(pollution1999) +
  geom_boxplot(aes(x = reorder(state_name, pm25, median), pm25, color = StateName), show.legend = F) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# histogram for two extrem states with highest and lowest pollution level
ggplot(filter(pollution1999, state_name %in% c("Hawaii","Georgia"))) +
  geom_histogram(aes(pm25), binwidth = 1) + 
  geom_vline(aes(xintercept = 12), color = "magenta") +
  facet_wrap(~ state_name, nrow = 2)
# air pollution vs. latitude and longitude 
pollution1999 %>% 
  mutate(east_west = cut(longitude, breaks = c(min(longitude) - 1, median(longitude), max(longitude) + 1), labels = c("East", "West"))) %>%
  ggplot() +
  geom_point(aes(latitude, pm25, color = east_west), alpha = 0.01) +
  geom_hline(aes(yintercept = 12), color = "grey0", linetype = "dotted") +
  scale_color_discrete(name = "", labels = c("West", "East")) 
# line types: "blank", "solid", "dashed", "dotted", "dotdash", "longdash", and "twodash"
pollution1999 %>% 
  filter(!is.na(pm25)) %>%
  mutate(east_west = cut(longitude, breaks = c(min(longitude) - 1, median(longitude), max(longitude) + 1), labels = c("East", "West"))) %>%
  ggplot() + geom_point(aes(latitude, pm25),alpha = 0.01) +
  geom_hline(aes(yintercept = 12), color = "magenta", linetype = "dotted") +
  facet_wrap(~ east_west, nrow = 2)
# why there is NA level in east_west?
p <- pollution1999 %>% filter(is.na(east_west)) %>% select(longitude, east_west)
# it seems that cut does not include min value. Fix: extract -1 from min in cut (line 45,53)

x <- rnorm(100)
y <- x + rnorm(100)
df <- tibble(x,y)
g <- gl(2,50, labels = c("Male", "Female"))
ggplot(data = df) +
  geom_point(data = df[g=="Male",], aes(x, y), color = "blue") +
  geom_point(data = df[g=="Female",], aes(x, y), color = "red") 

# pdf
?Devices
pdf(file = "plot.pdf")
ggplot(data = df) +
  geom_point(data = df[g=="Male",], aes(x, y), color = "blue") +
  geom_point(data = df[g=="Female",], aes(x, y), color = "red")
dev.off()
dev.cur()

ggplot(data = df) +
  geom_point(data = df[g=="Male",], aes(x, y), color = "blue") +
  geom_point(data = df[g=="Female",], aes(x, y), color = "red")
dev.copy(png, file = "example.png")
dev.off()

# Colors
pal <- colorRamp(c("red","blue"))
pal(0.3)
pal(seq(0,1,len = 10))
cols <- brewer.pal(3,"BuGn")
pal <- colorRampPalette(cols)
image(volcano,pal(20))
# RColorBrewer package
install.packages("RColorBrewer")
library(RColorBrewer)
cols <- brewer.pal(3, name = "BuGn")
pal <- colorRampPalette(cols)
par(mfrow = c(1,1))
image(volcano)
image(volcano, col = pal(20))
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)
# now in ggplot
# 1
df <- tibble(x = x, y = y,
             d = densCols(x,y,colramp = colorRampPalette(cols)))
df <- tibble(x = x, y = y,
             d = densCols(x,y,colramp = colorRampPalette(rev(rainbow(10,end = 4/6)))))
ggplot(df) + geom_point(aes(x,y, col = d), size = 1, show.legend = F) +
  scale_color_identity() +
  theme_bw()
# 2
ggplot(df) + geom_hex(aes(x,y), bins = 100) +
  scale_fill_gradientn("", colors = rev(rainbow(10, end = 4/6)))

# Hierarchial Clustering
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning#ggdendro-package-ggplot2-and-dendrogram
# http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=79
set.seed(1234)
par(mar = c(4,4,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)
plot.new()
plot(x,y, col = "blue", pch = 19, cex = 1)
text(x+0.05, y+0.05, labels(as.character(1:12)))
ggplot(data = data_frame(x,y)) +
  geom_point(aes(x,y), color = "blue") +
  geom_text(aes(x,y, label = 1:12),nudge_x = 0.05, nudge_y = 0.05)
# calculate distance
df <- data_frame(x = x, y = y)
distXY <- dist(df)
hClustering <- hclust(distXY)
par(mfrow = c(1,1))
plot(hClustering)
plot(hClustering, hang = -1, cex = .8)
plot(as.dendrogram(hClustering), type = "rectangle", ylab = "Height", cex = .6)
plot(as.dendrogram(hClustering), type = "triangle", ylab = "Height", cex = .6)
install.packages("ape")
library("ape")
plot(as.phylo(hClustering), type = "unrooted", cex = 0.6, no.margin = T)
plot(as.phylo(hClustering), type = "fan", cex = 0.8, no.margin = T)
plot(as.phylo(hClustering), type = "radial", cex = 0.8, no.margin = T)
colors = c("red", "blue", "green")
clus3 <- cutree(hClustering,3)
plot(as.phylo(hClustering), type = "fan", tip.color = colors[clus3], label.offset = 0.05, cex = 1, no.margin = T)
plot(as.phylo(hClustering), type = "cladogram", tip.color = "red", edge.color = "steelblue", edge.width = 2, label.offset = 0.05, cex = 1, no.margin = T, edge.lty = 1)
install.packages("ggdendro")
library("ggdendro")
ggdendrogram(hClustering)
ggdendrogram(hClustering, rotate = T, theme_dendro = F)
dend <- as.dendrogram(hClustering)
dend_data <- dendro_data(dend, type = "rectangle")
dend_data
names(dend_data)
head(dend_data)
head(dend_data$labels)
install.packages("dendextend")
library(dendextend)
dend <- iris[1:30,-5] %>% scale %>% dist %>%
  hclust %>% as.dendrogram %>%
  set("branches_k_color", k=3) %>%
  set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(0.9,1.2)) %>%
  set("leaves_pch", 19) %>%
  set("leaves_col", c("blue", "red"))
ggplot(as.ggdend(dend))
ggplot(as.ggdend(dend), theme = theme_minimal())
ggplot(as.ggdend(dend)) + 
  scale_y_reverse(expand = c(0.2,0)) + 
  coord_polar(theta = "x")

# K-means Clustering
df <- tibble(x,y)
kmeansObj <- kmeans(df, centers = 3)
cbind(df, kmeansObj$cluster)
#names(df)[3] <- "cluster"
ggplot() +
  geom_point(data = df, aes(x = x, y = y, color = kmeansObj$cluster)) +
  geom_point(data = as_tibble(kmeansObj$centers), aes(x = x, y = y, color = 1:length(x)), shape = 3)
# heat map k-mean clustering
set.seed(1234)
dataMatrix <- as.matrix(df)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(nfrow = c(1,2), mar = c(2,4,0.1,0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix))
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")
# how kmeans works
# first create fun to estimate distances from every points (x,y) to every centroid (cx,cy)
initCentroids <- function(x,y){ # x,y - two vectors of variables 
  cx <- sample(c(min(x), mean(x), max(x)), replace = F)
  cy <- sample(c(min(y), mean(y), max(y)), replace = F)
  data.frame(cx = cx, cy = cy)
}
mdist <- function(x,y,cx,cy) {
  distCentroidMatrix <- matrix(rep(NA, length(x) * length(cx)), nrow = length(x), ncol = length(cx))
  for(i in 1:length(cx)){
    distCentroidMatrix[,i] <- sqrt( (x-cx[i])^2 + (y-cy[i])^2 ) # distance from every point to every centroid
  }
  distCentroidMatrix
}
centroids <- initCentroids(x,y)
cx <- centroids$cx
cy <- centroids$cy
mdist(x,y,cx,cy)
distTmp <- mdist(x,y,cx,cy)
newClust <- apply(distTmp, 1, which.min) # find minimum for distances for each point
cols1 <- c("red", "orange", "purple")
plot(x,y, pch = 19, cex = 2, col = cols1[newClust], xlim = c(0,4), ylim = c(0,4))
points(cx,cy, pch = 3, cex = 2, col = 'green', lwd = 4)
newCx <- tapply(x,newClust,mean) # recalculate centroids center mass
newCy <- tapply(y,newClust,mean)
distTmp2 <- mdist(x,y,newCx, newCy) # new distances
newClust2 <- apply(distTmp2,1,which.min) # new clusters assignment
plot(x,y,pch = 19,cex = 2, col = cols1[newClust2])
points(newCx,newCy, pch = 3, cex = 2, col = 'green', lwd = 4)
finalCx <- tapply(x,newClust2,mean)
finalCy <- tapply(y,newClust2,mean)
kmObj <- kmeans(df,centers = 3)
plot(x,y,col = kmObj$cluster, pch = 19, cex = 2) 
points(kmObj$centers, col = c("black", "red", "green"), pch=3,cex=3,lwd=3)

# Heat map
# https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
# Good tutorial on heatmaps http://sebastianraschka.com/Articles/heatmaps_in_r.html#clustering
library(readr)
nba <- read_csv("http://datasets.flowingdata.com/ppg2008.csv")
head(nba)
nba$Name <- with(nba, reorder(Name, PTS)) # names converted to factors with levels sorted according to PTS
head(nba)
reorder(nba$Name, nba$PTS)
# Whilst FlowingData uses heatmap function in the stats-package that requires the 
# plotted values to be in matrix format, ggplot2 operates with dataframes. For 
# ease of processing, the dataframe is converted from wide format to a long format.
library(ggplot2)
install.packages("scales")
library(scales) # for rescale()
library(plyr) # for ddply()
install.packages("reshape2")
library(reshape2) # for melt()
nba_m <- melt(nba)
#nba_m <- ddply(nba_m,.(variable),transform, rescale = rescale(nba_m$value)) #convert matrix to data frame
#nba_m <- ddply(nba_m,.(variable),transform) #convert matrix to data frame
nba_m <- mutate(nba_m, rescale = rescale(nba_m$value))
nba_m <- mutate(nba_m, rescale1 = scale(nba_m$value))

ggplot(nba_m, aes(variable, Name)) +
  geom_tile(aes(fill = rescale),  color = "white")+
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_grey(base_size = 9) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 9 * 0.8, angle = 330, hjust = 0, color = "grey50"))


# System colors tile representation
# https://stackoverflow.com/questions/19289358/how-can-i-plot-a-image-with-x-y-r-g-b-coordinates-using-ggplot2
colorMatrix <- matrix(data = colors(), nrow = 30, ncol = 22)
colorMatrix_df <- tibble(x = 1:dim(colorMatrix)[2], y = 1:dim(colorMatrix)[2], color = colorMatrix[x,y])
ggplot(data=image.rgb, aes(x=x, y=y, col=rgb(r,g,b))) + 
  geom_point() + 
  scale_color_identity()
ggplot(data=image.rgb, aes(x=x, y=y, fill=rgb(r,g,b))) +
  geom_tile() +
  scale_fill_identity()

# Dimension Reduction
set.seed(1234)
par(mar = rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)
# let's add pattern to our data
set.seed(678918)
for (i in 1:nrow(dataMatrix)) {
  coinFlip <- rbinom(1,size = 1, prob = 0.5)
  if (coinFlip) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each = 5)
  }
}
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)
# add more pattern
hh <- hclust(dist(dataMatrix))
plot(hh)
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3), mar = c(4,4,0,0))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1, xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column mean", pch = 19)
# we see clear patterns in rows and columns there
heatmap(dataMatrixOrdered)

# SVD - singular value decomposition
# how SVD works
mat1 <- matrix(c(1,2,2,5,3,7), nrow = 2)
s <- svd(mat1)
matD <- s$d
matU <- s$u
matV <- s$v
matU %*% matD %*% t(matV) # is equal to original mat1


svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)
par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)
# Relationship to principal component analisys
pca1 <- prcomp(dataMatrixOrdered, scale = T)
plot(pca1$rotation[,1], svd1$v[,1], pch = 19, xlab = "Principal component 1", ylab = "Right Singulat Vector 1")
abline(c(0,1))
# SVD explained
# 1
simpleMatrix <- dataMatrixOrdered * 0
for (i in 1:dim(simpleMatrix)[1]) {simpleMatrix[i,] <- rep(c(0,1), each = 5)}
for (i in 1:dim(simpleMatrix)[1]) {simpleMatrix[i,] <- rep(c(1,0), each = 5)}
svd2 <- svd(simpleMatrix)
par(mfrow = c(1,3))
image(t(simpleMatrix)[,nrow(simpleMatrix):1])
plot(svd2$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd2$d^2/sum(svd2$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)
# add more pattern
set.seed(678910)
dataMatrix <- matrix(rnorm(400), nrow = 40)
for (i in 1:dim(dataMatrix)[1]){
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  if (coinFlip1) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), each = 5)
  }
  if (coinFlip2) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), 5)
  }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0,1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0,1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")
# and SVD can help us to discover these two patterns
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[,1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[,2], pch = 19, xlab = "Column", ylab = "Second right singular vector")
# we hardly see that both first and second right singular vectors have two patterns 
par(mfrow = c(1,2))
plot(svd2$d, pch = 19, xlab = "Column", ylab = "Singular value")
plot(svd2$d^2/sum(svd2$d^2), pch = 19, xlab = "Column", ylab = "Singular value")
# we see that first singular vector explains about 40% of variation
# let's reconstruct
approx1 <- svd2$u[,1] %*% t(svd2$v[,1]) * svd2$d[1] 
approx5 <- svd2$u[,1:5] %*% diag(svd2$d[1:5]) %*% t(svd2$v[,1:5])
approx10 <- svd2$u[,1:10] %*% diag(svd2$d[1:10]) %*% t(svd2$v[,1:10])
par(mfrow = c(1,4), mar = c(2,2,2,0))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image(t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1], main = "(d)")
# SVD doesn't wotk with matrixes with missing values
# one way is imputing: replace missed values with nearest mean
# https://www.bioconductor.org/packages/release/bioc/html/impute.html
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("impute")
library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = F)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)) # for comparison
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1,2))
plot(svd1$v[,1], pch = 19)
plot(svd2$v[,1], pch = 19) # we see that result is pretty similar

# PCA - principal components analysis
# A Tutorial on Principal Component Analysis http://arxiv.org/pdf/1404.1100.pdf
# PCA is very similar to SVD. Let's demostrate
# We'll demonstrate this now. First we have to scale mat, our simple
# example data matrix.  This means that we subtract the column mean from
# every element and divide the result by the column standard deviation.
# Of course R has a command, scale, that does this for you. Run svd on
# scale of mat.
svd1 <- svd(scale(mat))
prcomp(scale(mat)) # results looks pretty similar
svd1$v[,1]

# Example of Exploratary Data Analysis
#loading data and orgnizing them in data frame
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/accData.zip")
unzip("./data/accData.zip", exdir = "./data/")
#You should create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set.
trainSubjects_df <- as_tibble(read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = c("subjectID")))
trainActivity_df <- as_tibble(read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = c("activityN")))
features_df <- as_tibble(read.table("./data/UCI HAR Dataset/features.txt", colClasses = c("integer", "character")))
names <- sub("\\(\\)","",features_df$V2)
names <- gsub("\\(|\\)","",features_df$V2)
names <- gsub(",","-",features_df$V2)
#names <- sub("([0-9]*),([0-9]*)","\1-\2",names)
names <- sub("\\(","",names)
names <- sub("\\)","",names)
trainData_df <- as_tibble(read.table("./data/UCI HAR Dataset/train/x_train.txt", col.names = names))
activityLabels_df <- as_tibble(read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("activityN","activity")))
trainActivity_df <- inner_join(trainActivity_df,activityLabels_df, by = "activityN")
train_df <- as_tibble(cbind(trainSubjects_df, trainActivity_df, trainData_df))
train_df$activity <- tolower(train_df$activity)
samsungData <- train_df 
samsungData1 <- samsungData %>% select(1:15) %>% filter(subjectID == 1) 
samsungData1 <- samsungData1 %>% mutate(n = 1:nrow(samsungData1))
# Analysis
table(samsungData$activity)
numericActivity <- as.numeric(as.factor(samsungData$activity))[samsungData$subjectID == 1]
par(mfrow = c(1,2))
plot(samsungData1$n, as.matrix(samsungData1[,4]), pch=19,col=numericActivity, ylab = names(samsungData)[4])
plot(samsungData1$n, as.matrix(samsungData1[,5]), pch=19,col=numericActivity, ylab = names(samsungData)[5])
legend(150,-0.2,legend = unique(samsungData1$activity), col=unique(numericActivity), pch = 19)
# the same in ggplot
ggplot(data = samsungData1) +
  geom_point(aes(samsungData1$n, as.matrix(samsungData1[,4]), color = numericActivity)) +
  scale_color_identity()
# to show multiple graphs see http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
distanceMatrix <- dist(samsungData1[,4:6])
hclustering <- hclust(distanceMatrix)
par(mfrow = c(1,1), mar = c(4,4,2,2))
plot(as.dendrogram(hclustering))
source("myplclust.R")
par(mfrow = c(1,1))
myplclust(hclustering, lab.col = numericActivity)
##############################################################################
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )
}
######################################################################
install.packages("dendextend")
library(dendextend)
dend <- hclustering %>% as.dendrogram %>%
  set("branches_lwd", 0.3) %>%
  set("labels_colors", numericActivity) %>% 
  set("labels_cex", c(0.9,1.2)) %>%
  set("leaves_pch", 19) %>%
  set("leaves_col", numericActivity) 
ggplot(as.ggdend(dend))
# nothing interesting in clustering; lets plot max acceleration
par(mfrow = c(1,2))
plot(samsungData1$n, as.matrix(samsungData1[,13]), pch=19,col=numericActivity, ylab = names(samsungData)[13])
plot(samsungData1$n, as.matrix(samsungData1[,14]), pch=19,col=numericActivity, ylab = names(samsungData)[14])
distanceMatrix <- dist(samsungData1[,13:15])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = numericActivity)
source("myplclust1.R")
myplclust1(hclustering, lab.col = numericActivity)
# now SVD
svd1 <- svd(scale(samsungData[samsungData$subjectID == 1, -c(1,2,3)]))
par(mfrow = c(1,2))
plot(svd1$u[,1], col = numericActivity, pch = 19)
plot(svd1$u[,2], col = numericActivity, pch = 19)
# let's find the maximum contributor
plot(svd1$v[,2], pch=19)
maxContrib <- which.max(svd1$v[,2])
names(samsungData)[maxContrib + 3]
distanceMatrix <- dist(samsungData[samsungData$subjectID == 1, c(13:15, maxContrib + 3)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = numericActivity)
myplclust1(hclustering, lab.col = numericActivity)
# k-means
kClust <- kmeans(samsungData[samsungData$subjectID==1,-c(1:3)], centers = 6)
table(kClust$cluster, samsungData$activity[samsungData$subjectID==1])
table(kClust$cluster)
kClust$cluster
# one trial
kClust <- kmeans(samsungData[samsungData$subjectID==1,-c(1:3)], centers = 6, nstart = 1)
table(kClust$cluster, samsungData$activity[samsungData$subjectID==1])
# 100 trials
kClust <- kmeans(samsungData[samsungData$subjectID==1,-c(1:3)], centers = 6, nstart = 100)
table(kClust$cluster, samsungData$activity[samsungData$subjectID==1])
# let's plot centers
par(mfrow=c(1,2))
plot(kClust$centers[1,1:10], pch=19,ylab = "Cluster №1 Center", xlab = "")
plot(kClust$centers[6,1:10], pch=19,ylab = "Cluster №6 Center", xlab = "")

# Air pollution case study
# PM2.5 files https://aqs.epa.gov/aqsweb/airdata/download_files.html
pm0 <- read.csv("daily_88101_1999.csv")
dim(pm0)
names(pm0)
# if we need to extract column names (in case of .txt)
# cnames <- readLines("daily_88101_1999.csv", 1)
# cnames <- strsplit(cnames, split = ",", fixed = T)
# cnames <- gsub("\"","", cnames[[1]], fixed = T)
# names(pm0) <- cnames
# names(pm0) <- make.names(cnames) # make syntactically coorect names
# 
x0 <- pm0$Arithmetic.Mean # this ih our pm2.5 value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0))  # proportion of missing values

pm1 <- read.csv("daily_88101_2018.csv")
dim(pm1)
x1 <- pm1$Arithmetic.Mean
str(x1)
summary(x1)  
mean(is.na(x1)) 

par(mfrow = c(1,1))
boxplot(x0,x1)  
ggplot() + 
  geom_boxplot(data = tibble(x0 = x0), aes("x0", x0)) +
  geom_boxplot(data = tibble(x1 = x1), aes("x1", x1))
boxplot(log10(x0),log10(x1)) # to see large outliers
# we see that mean goes down in 2018, but data range spread is much bigger
# we saw in summary(x1) that there are negative values. Let's check the cause
summary(x1)
negative <- x1 < 0
x1[negative]
str(negative)
sum(negative, na.rm = T) # 3529 negative values
mean(negative, na.rm = T) # 0.06% of total values, not much
# let's check wether there is dependency of negative on time of year
dates <- pm1$Date.Local
str(dates)
dates <- as.Date(as.character(dates), "%Y-%m-%d")
hist(dates, "month")
dates_df <- tibble(dates)
dates_df %>% mutate(month = as.POSIXlt(dates)$mon) %>%
  ggplot() + geom_histogram(aes(month))
hist(dates[negative], "month")
# let's explore how data changed on one monitor in New York state
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code,Site.Num)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code,Site.Num)))
head(site0)
site0 <- paste(site0[,1], site0[,2],sep = ".")
site1 <- paste(site1[,1], site1[,2],sep = ".")
# dplyr version
#site0 <- pm0 %>% filter(State.Code == 36) %>% 
#  select(County.Code, Site.Num) %>% mutate(Site = paste(County.Code, Site.Num, sep="."))
both <- intersect(site0,site1)
pm0$County.Site <- with(pm0, paste(County.Code, Site.Num, sep="."))
pm1$County.Site <- with(pm1, paste(County.Code, Site.Num, sep="."))
cnt0 <- subset(pm0, State.Code == 36 & County.Site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & County.Site %in% both)
split(cnt0, cnt0$County.Site)
sapply(split(cnt0, cnt0$County.Site), nrow)
sapply(split(cnt1, cnt1$County.Site), nrow) # let's choose 101.3
# in dplyr
#pm0 %>% 
#  mutate(County.S = paste(County.Code, Site.Num, sep=".")) %>%
#  filter(State.Code == 36 & County.S %in% both) %>%
#  group_by(County.S) %>% 
#  summarise(count = n()) # does not work, why? Answer: nedd to detach plyr, conflict with dplyr
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 101 & Site.Num == 3)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 101 & Site.Num == 3)
dim(pm1sub)
dim(pm0sub)
# let's illustrate pm2.5 variation vs. dates
# for 2018
dates1 <- as.Date(pm1sub$Date.Local)
x1sub <- pm1sub$X1st.Max.Value  
plot(dates1, x1sub)  
abline(h = mean(x1sub), col = "magenta") 
ggplot(pm1sub) + 
  geom_point(aes(dates1, x1sub), color = "blue", alpha = 0.5) +
  geom_hline(yintercept = mean(x1sub), color = "magenta")
# for 1999
dates0 <- as.Date(pm0sub$Date.Local)
x0sub <- pm0sub$X1st.Max.Value  
plot(dates0, x0sub)  
abline(h = mean(x0sub), col = "magenta") 
ggplot(pm0sub) + 
  geom_point(aes(dates0, x0sub), color = "blue", alpha = 0.5) +
  geom_hline(yintercept = mean(x0sub), color = "magenta")
# let's show both 1999 and 2018 on one graph (dates are different)
par(mfrow = c(1,2))
plot(dates0, x0sub)
abline(h = mean(x0sub), col = "magenta")
plot(dates1, x1sub)
abline(h = mean(x1sub), col = "magenta")
# y sclales are different and misconfusing
range(x0sub)
range(x1sub)  
range(dates0)  
range(dates1) 
rng <- range(x0sub, x1sub, na.rm = T)
par(mfrow = c(1,2))
plot(dates0, x0sub, ylim = rng)
abline(h = mean(x0sub), col = "magenta")
plot(dates1, x1sub, ylim = rng)
abline(h = mean(x1sub), col = "magenta")
# let's look at changes on state level
# we'll find average values in each state for 1999 and 2018 and connect them
mn0 <- with(pm0, tapply(X1st.Max.Value,State.Code, mean, na.rm = T))
summary(mn0)
mn1 <- with(pm1, tapply(X1st.Max.Value,State.Code, mean, na.rm = T))
summary(mn1)
d0 <- tibble(state = names(mn0), mean = mn0)
d1 <- tibble(state = names(mn1), mean = mn1)
mrg <- merge(d0,d1, by = "state")
dim(mrg)
par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 51), mrg[,2], xlim = c(1998,2019)))  
with(mrg, points(rep(2018, 51), mrg[,3]))  
segments(rep(1999, 51), mrg[,2],rep(2018, 51), mrg[,3])

# Course project 2 from Exploratory Data Analysis https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = "course_project_2_EDA.zip")
unzip("course_project_2_EDA.zip", exdir = "CourseProject2EDA")
NEI <- as_tibble(readRDS("./CourseProject2EDA/summarySCC_PM25.rds"))
NEI$SCC <- as.factor(NEI$SCC)
SCC <- as_tibble(readRDS("./CourseProject2EDA/Source_Classification_Code.rds"))
table(NEI$year)
dim(NEI)
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008
te <- sapply(split(NEI$Emissions, NEI$year), sum)
plot(names(te), te, pch = 19, col = "blue")
detach(package: plyr)
NEI %>% group_by(year) %>% summarise(total = sum(Emissions)) %>%
  ggplot() + geom_point(aes(year, total), color = "blue")
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
df_baltimore <- subset(NEI, fips == "24510")
te_baltimore <- sapply(split(df_baltimore$Emissions, df_baltimore$year), sum)
plot(names(te_baltimore), te_baltimore, col = "blue", pch = 19)
NEI %>% filter(fips == "24510") %>% group_by(year) %>%
  summarise(total = sum(Emissions)) %>%
  ggplot() + geom_point(aes(year, total), color = "blue")
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
NEI %>% filter(fips == "24510") %>% group_by(type, year) %>%
  summarise(total = sum(Emissions)) %>%
  ggplot() + geom_point(aes(year, total, color = type)) +
  geom_path(aes(year,total, color = type))
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# let's first try find attributes wich contain "coal" to identify ones for filtering
grep("[Cc]oal", SCC$SCC.Level.One, fixed = F)
grep("[Cc]oal", SCC$SCC.Level.Two, fixed = F)
grep("[Cc]oal", SCC$SCC.Level.Three, fixed = F)
coal1 <- grep("[Cc]oal", SCC$SCC.Level.Three, fixed = F, value = T)
coal2 <- grep("[Cc]oal", SCC$Short.Name, fixed = F, value = T)
length(coal1)
length(coal2) # let's choose coal2
coal2 <- grepl("[Cc]oal", SCC$Short.Name, fixed = F)
head(SCC[coal2,])
SCCsub <- SCC[coal2,]$SCC
head(SCCsub)
intersectionSCC <- as.factor(intersect(SCCsub,NEI$SCC))
NEIsub <- NEI %>% filter(SCC %in% intersectionSCC)
# let's check that NEIsub correspond to "coal" terms
SCC %>% filter(SCC %in% NEIsub$SCC) %>% select(Short.Name)
# let's show how emission from coal combustion-related sources changed from 1999–2008
NEIsub %>% group_by(year) %>%
  summarise(total = sum(Emissions)) %>%
  ggplot() + geom_point(aes(year, total), color = "blue")
# in Baltimore
NEIsub %>% filter(fips == 24510) %>% group_by(year) %>%
  summarise(total = sum(Emissions)) %>%
  ggplot() + geom_point(aes(year, total), color = "blue")
# let's compare Baltimore with Los Angeles
baltimoreCoal <- NEIsub %>% filter(fips == "24510") %>% group_by(year) %>%
  summarise(total = sum(Emissions)) 
losangelesCoal <- NEIsub %>% filter(fips == "06037") %>% group_by(year) %>%
  summarise(total = sum(Emissions))

ggplot() + 
  geom_point(data = baltimoreCoal, aes(year, total), color = "blue") +
  geom_point(data = losangelesCoal, aes(year, total), color = "red")
dev.copy(png,filename = "plot1.png")
dev.cur()
dev.off()


