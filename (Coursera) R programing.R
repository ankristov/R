#####################
# Functions #
####################

add2 <- function(x,y) {
  x + y
}

above <- function(x,n) {
  x[x > n]
}

columnmean <- function(df, removeNA) {
  ncolumn <- ncol(df)
  means <- numeric(ncolumn)
  for (i in 1:ncolumn) {
    means[i] <- mean(df[,i],na.rm = removeNA)
  }
  means
}

make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}
square <- make.power(2)
square(7)
make.power(2)(7)
square
ls(environment(square))
get("n", environment(square))

#####################
# Loops: lapply #
####################
x <- list(a=1:4,b=rnorm(10),c=c("a","b","c"),d=rnorm(100,5))
x[1]
x$a
x["a"]
lapply(x, mean) #error
lapply(x, typeof)
lapply(1:4, runif)
lapply(1:4, runif, min = 0, max = 10)

x <- list(a = matrix(1:4,2,2), b = matrix(1:6,3,2))
lapply(x, function(m){m[,1]})
lapply(x, function(m){m[,2]})
lapply(x, function(m){m[1,]})
lapply(x, function(m){m[2,]})

x <- list(a=1:4,b=rnorm(10),d=rnorm(100,5))

lapply(x, mean)
sapply(x, mean)
mean(x) #error

#apply
x <- matrix(rnorm(100),20,5)
str(x)
summary(x)
apply(x,2,mean) #looping through columns
apply(x,2,median)
apply(x,1,sum) #looping through rows
rowSums(x)
#rowSums(x) = apply(x,1,sum) but much more optimized
#rowMeans(x) = apply(x,1,mean) 
#colSums(x) = apply(x,2,sum) 
#colMeans(x) = apply(x,2,mean) 
apply(x,1, quantile,probs = c(0.25, 0.75))

a <- array(rnorm(2*2*10), c(2,2,10))
apply(a,c(1,2),mean) #looping through 3rd dimension
rowMeans(a,dims = 2)

#mapply
l <- list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))
l <- mapply(rep,1:4,4:1)
noise <- function(n,mean,sd) {
  rnorm(n,mean,sd)
}
noise(5,1,2)
noise(1:5,1:5,2) #works not correctly with vector arguments
mapply(noise,1:5,1:5,2)
list(noise(1,1,2),noise(2,2,2),
     noise(3,3,2),noise(4,4,2),
     noise(5,5,2)) #the same 

#split
library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)
lapply(s, function(x){colMeans(x[,c("Ozone","Solar.R","Wind")])})
sapply(s, function(x){colMeans(x[,c("Ozone","Solar.R","Wind")])})
sapply(s, function(x){colMeans(x[,c("Ozone","Solar.R","Wind")],na.rm = T)})

x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)
f1
f2
interaction(f1,f2)
split(x,list(f1,f2))
str(split(x,list(f1,f2)))
str(split(x,list(f1,f2),drop = T))


#####################
# Debugging #
####################
mean(x_ar_nu) #error
traceback()
lm(yy-xx)
traceback()
debug(lm)
lm(yy-xx)  
undebug(lm)

#####################
# str() #
####################
x <- rnorm(100,2,4)
str(x)
summary(x)

f <- gl(20,3)
f
str(f)
summary(f)       

library(datasets)
head(airquality)
str(airquality)
length(airquality$Ozone)

s <- split(airquality, airquality$Month)
str(s)

#####################
# Random numbers #
####################
x <- rnorm(10,20,2)
x
summary(x)
set.seed(1)
rnorm(5)
rnorm(5)
rnorm(5)
set.seed(1)
rnorm(5)

set.seed(20)
x <- rnorm(100)
summary(x)
e <- rnorm(100,0,2)
summary(e)
y <- 0.5 + 2*x + e
summary(y)
plot(x,y)

set.seed(13)
x <- rbinom(100,1,0.5)
summary(x)
e <- rnorm(100,0,2)
y <- 0.5 + 2*x + e
summary(y)
plot(x,y)

set.seed(1)
x <- rnorm(1000)
log.mu <- 0.5 + 0.3*x
y <- rpois(1000,exp(log.mu))
summary(y)
plot(x,y)

set.seed(1)
sample(10:20,7)
sample(10:20,7)
sample(letters,5)
sample(letters,5)
sample(10:20)
sample(10:20, replace = T)

x<-rnorm(100)
x
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
y
summary(y)
plot(x,y)

library(datasets)
data("airquality")
head(airquality)
set.seed(20)
idx <- seq_len(nrow(airquality))
idx
samp <- sample(idx,6)
samp
airquality[samp,]


############################
# Pre-programing Asignment #
############################
dataURL <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"
download.file(dataURL,"diet_data.zip")
unzip("diet_data.zip",exdir = "diet_data")
list.files(path = "diet_data")
andy <- read.csv('diet_data/andy.csv')
head(andy)
length(andy$Day)
dim(andy)
str(andy)
names(andy)
summary(andy)
andy[1,'Weight']
andy[30,'Weight']
min(andy$Weight)
max(andy$Weight)
mean(andy$Weight)
andy[which(andy$Day == 30),"Weight"]
andy[which(andy$Day > 20),"Weight"]
andy[which(andy$Day > 20),]
andy[which(andy[, "Day"] == 30), "Weight"]
subset(andy$Weight,andy$Day == 30)
files <- list.files("diet_data")
files[1]
head(read.csv(files[3])) #error
files_full <- list.files("diet_data",full.names = TRUE)
head(read.csv(files_full[3]))
david <- read.csv(files_full[2])
andy_david <- rbind(andy, david)
head(andy_david)
tail(andy_david)
all_data <- data.frame()
for (i in 1:length(files_full)) {
  all_data <- rbind(all_data, read.csv(files_full[i]))
}
median(all_data$Weight)
median(all_data$Weight, na.rm = TRUE)
all_data_30 <- all_data[which(all_data[,"Day"] == 30),]
all_data_30
median(all_data_30$Weight)

all_data_list <- lapply(files_full,read.csv) #list of dataframes, more effective than for loop
all_data_1 <- do.call(rbind, all_data_list) #one dataframe

############################
# Programing Asignment #
############################
data_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(data_url,'specdata.zip')
unzip("specdata.zip")
files <- list.files(directory, full.names = T)

#files <- list.files("specdata", full.names = T)
#file_200 <- read.csv(files[200])
#head(file_200)
#requested_data_list <- lapply(files[117:118],read.csv)
#requested_data <- do.call(rbind, requested_data_list)
#head(requested_data)

pollutantmean <- function(directory,pollutant,id=1:332) {
  requested_data_list <- lapply(files[id],read.csv)
  requested_data <- do.call(rbind,requested_data_list)
  mean(requested_data[, pollutant], na.rm = TRUE)
}
complete <- function(directory, id=1:332) {
  requested_data <- lapply(files[id], read.csv) #list of frames
  #head(requested_data[[1]])
  df_res <- data.frame()
  for (d in requested_data) {
    cs <- length(complete.cases(d))
    df <- data.frame(id = d$ID[1],nobs = cs)
    df_res <- rbind(df_res, df)
  }
  df_res
}

correlation <- function(directory, threshold = 0) {
  df <- complete(directory)
  df_thr <- df[which(df$nobs > threshold),]
  head(df_thr)
  data <- lapply(files[df_thr$id], read.csv)
  #data_1 <- data[[1]]
  #dim(data_1)
  #data_1 <- data_1[complete.cases(data_1),]
  #dim(data_1)
  
  cor_vector = c()
  for (d in data) {
    d <- d[complete.cases(d),]
    c <- cor(d$nitrate, d$sulfate)
    cor_vector <- rbind(cor_vector, c)
  }
  cor_vector
}

############################
# Programing Asignment # https://www.coursera.org/learn/r-programming/supplement/w1c7p/programming-assignment-3-instructions-hospital-quality
############################
dataURL <- 'https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip'
rawDataZIP <- download.file(dataURL, destfile = 'ProgAssignment3-data.zip')
rawData <- unzip('ProgAssignment3-data.zip',exdir = 'PrAs3')
outcome <- read.csv('PrAs3/outcome-of-care-measures.csv', stringsAsFactors = F,na.strings = 'Not Available')
head(outcome)
str(outcome)
names(outcome)
mortalityRate <- outcome[,11]
str(mortalityRate)
head(mortalityRate)
#mortalityRate <- as.numeric(mortalityRate) #does not work for factor-column wgich we receive when do not use stringsAsFactors in read.csv
#str(mortalityRate)
#head(mortalityRate)
hist(mortalityRate)
########################
library(readr)
dataURL <- 'https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip'
rawDataZIP <- download.file(dataURL, destfile = 'ProgAssignment3-data.zip')
# faster then read.csv, unzip automatically
rawData <- read_csv("ProgAssignment3-data.zip",col_types = "cccllccicncccccccccccccccccccccccccccccccccccc",  n_max = 10, progress = T) 

############################
# Storing data
############################
a <- data.frame(x = rnorm(100), y = runif(100))
b <- c(3,4.4, 1/3)
dump(c("a","b"), file = "data_store_text.R")
source("data_store_text.R")

save(a,b, file = "data_store_binary.rda")
load("data_store_binary.rda")

save.image(file="all_data_store_binary.RData")

a_s <- serialize(a, connection = NULL)
save(a_s, file="data_store_binary-serialized.RData")

############################
# Connections
############################
con <- file("all_data_store_binary.RData")
data <- readLines(con,100)
data

con <- file("Motivation letter.txt")
data <- readLines(con,100)
data
data[5]
writeLines("This last line added by R application. Hello world!", con)
data # WARNING: all data where deleted!

con <- file("Motivation letter.txt")
con <- gzfile("words.gz")
con <- url("https://promusculus.ru/robots.txt", "r")
x <- readLines(con)
head(x)
x

############################
# reading larg datasets
############################
initial <- read.table("daily_SPEC_1999.csv", nrows = 100)
classes <- sapply(initial, class)
tabAll <- read.table("daily_SPEC_1999.csv", colClasses = classes)
############################
# dplyr
############################
install.packages("dplyr")
library(dplyr)

chicago <- readRDS("chicago.rds")
str(chicago)
dim(chicago)
summary(chicago)
names(chicago)

# select
subset <- select(chicago, city:dptp)
head(subset)
subset <- select(chicago, c(city,date,no2tmean2))
head(subset)
subset <- select(chicago, city,date,no2tmean2)
head(subset)
summary(chicago$no2tmean2)
subset <- select(chicago, -c(city,date,no2tmean2))
head(subset)
head(chicago)

i <- match("no2tmean2", names(chicago))
i
head(chicago[,i])

subset <- select(chicago, ends_with("2"))
str(subset)
subset <- select(chicago, starts_with("d"))
str(subset)

#filter
subset <- filter(chicago, pm25tmean2 > 30)
str(subset)
summary(subset$pm25tmean2)

subset <- filter(chicago, pm25tmean2>30 & tmpd>80)
select(subset, date, tmpd, pm25tmean2)

# arrange
chicago <- arrange(chicago,date)
head(select(chicago,date,pm25tmean2), 10)
tail(select(chicago,date,pm25tmean2), 10)

chicago <- arrange(chicago,desc(date))
head(select(chicago,date,pm25tmean2), 10)
tail(select(chicago,date,pm25tmean2), 10)

# rename
names(chicago)
head(chicago[,1:5],3)
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)
head(chicago[,1:5],3)

# mutate
x <- c(10,10,10,NA,NA,10,10,NA)
mean(x)
mean(x, na.rm = T)

chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = T))
head(chicago)
mean(chicago$pm25, na.rm = T)

head(transmute(chicago,pm10detrend = pm10tmean2 - mean(pm10tmean2, na.rm = T),
               o3detrend = o3tmean2 - mean(o3tmean2, na.rm = T)))

# group_by
chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
head(chicago)
tail(chicago)
chicago_year <- group_by(chicago,year)
head(chicago_year)
summary(chicago_year)
class(chicago_year)
class(chicago)
summarize(chicago_year,pm25 = mean(pm25,na.rm=T),
          o3 = max(o3tmean2, na.rm = T),
          no2 = median(no2tmean2,na.rm = T))
  
qq <- quantile(chicago$pm25, seq(0,1,0.2), na.rm = T)
qq  
chicago <- mutate(chicago, pm25.quint = cut(pm25,qq))
chicago$pm25.quint  
head(chicago)
chicago_quint <- group_by(chicago, pm25.quint)
summarise(chicago_quint, o3 = mean(o3tmean2, na.rm = T),
          no2 = mean(no2tmean2, na.rm = T))

# %>%
mutate(chicago, pm25.quint = cut(pm25,qq)) %>%
  group_by(pm25.quint) %>%
  summarise(o3 = mean(o3tmean2, na.rm = T),
            no2 = mean(no2tmean2, na.rm = T))
xy <- mutate(chicago, month = as.POSIXlt(date)$mon + 1) %>%
  group_by(month) %>%
  summarise(pm25 = mean(pm25, na.rm = T),
            o3 = max(o3tmean2, na.rm = T),
            no2 = median(no2tmean2, na.rm = T))
plot(xy$month,xy$o3)


############################
# regular expressions
############################
x <- "<dd>Found on January 1, 2007</dd>"
x1 <- "<dd>Found on January 1, 2007</dd>kdsjfl kljlk KkK33Lkj <dd>Found on Feb 1, 20013</dd>m,mm,m,m (klkjlkj)kks k <dd>Found on July 13, 2000</dd>"
r <- regexpr("<dd>[F|f]ound(.*?)</dd>",x1)
r
r1 <- gregexpr("<dd>[F|f]ound(.*?)</dd>",x1)
r1
regmatches(x1,r)
r2 <- regmatches(x1,r1)
r2
sub("<dd>[F|f]ound on |</dd>","",x)
gsub("<dd>[F|f]ound on |</dd>","",x)
gsub("<dd>[F|f]ound on |</dd>","",x1)
r3 <- gsub("<dd>[F|f]ound on |</dd>","",r2)
r3
dates <- lapply(r2, sub, "<dd>[F|f]ound on |</dd>","") # NO
dates <- lapply(r2[[1]], sub, "<dd>[F|f]ound on |</dd>","") # NO
dates <- lapply(r2[[1]], function(a) {gsub("<dd>[F|f]ound on |</dd>","",a)}) # YES
dates
gsub("<dd>[F|f]ound on |</dd>","",r2[[1]][1])

homicides <- c( "39.33743900000, -76.66316500000, icon_homicide_bluntforce, 'p914', '<dl><dt><a href=\"htt\
p://essentials.baltimoresun.com/micro_sun/homicides/victim/914/steven-harris\">Steven Harris</\
a></dt><dd class=\"address\">4200 Pimlico Road<br />Baltimore, MD 21215</dd><dd>Race: Black<br\
/>Gender: male<br />Age: 38 years old</dd><dd>Found on July 29, 2010</dd><dd>Victim died at S\ cene</dd><dd>Cause: Blunt Force</dd><dd class=\"popup-note\"><p>Harris was found dead July 22 \ and ruled a shooting victim; an autopsy subsequently showed that he had not been shot,...</dd>\ </dl>'",
                "39.311024, -76.674227, iconHomicideShooting, 'p2', '<dl><dt>Leon Nelson</dt><dd class=\"a\
ddress\">3400 Clifton Ave.<br />Baltimore, MD 21216</dd><dd>black male, 17 years old</dd><dd>Found on January 1, 2007</dd><dd>Victim died at Shock Trauma</dd><dd>Cause: shooting</dd></dl>'",
                "39.30677400000, -76.59891100000, icon_homicide_shooting, 'p816', '<dl><dt><a href=\"http:\
//essentials.baltimoresun.com/micro_sun/homicides/victim/816/kenly-wheeler\">Kenly Wheeler</a>\
</dt><dd class=\"address\">1400 N Caroline St<br />Baltimore, MD 21213</dd><dd>Race: Black<br \
/>Gender: male<br />Age: 29 years old</dd><dd>Found on March  3, 2010</dd><dd>Victim died at S\
cene</dd><dd>Cause: Shooting</dd><dd class=\"popup-note\"><p>Wheeler\\'s body was&nbsp;found o\
n the grounds of Dr. Bernard Harris Sr.&nbsp;Elementary School</p></dd></dl>'")
r <- regexec("<dd>[F|f]ound on(.*?)</dd>", homicides)
r
r1 <- regmatches(homicides, r)
r1
dates <- sapply(r1, function(x) x[2])
dates
dates <- as.Date(dates, " %B %d, %Y")
dates

########################################
# Data Analysis Case Study: air quality 
# https://aqs.epa.gov/aqsweb/airdata/download_files.html#Annual
########################################
pm0 <- read.csv("daily_88101_1999.csv")
dim(pm0)
head(pm0)
names(pm0)
x0 <- pm0$X1st.Max.Value
x0[1:30]
summary(x0)
length(x0[is.na(x0)])
x0[is.na(x0)]
mean(is.na(x0)) # how much NA in data

pm1 <- read.csv("daily_88101_2019.csv")
x1 <- pm1$X1st.Max.Value
summary(x1)
mean(is.na(x1))

boxplot(log2(x0), log2(x1))

mean(x1 < 0)

# let's explore in which months negative values are observed

pm1_1 <- filter(pm1,X1st.Max.Value < 0)
pm1_1 <- select(pm1_1,X1st.Max.Value,Date.Local)  
pm1_1 <- mutate(pm1_1, Month = month.name[as.POSIXlt(as.Date(Date.Local,"%Y-%m-%d"))$mon + 1])
tab <- table(factor(pm1_1$Month, levels = month.name))
round(100*tab/sum(tab))
  
d <- pm1[which(pm1[,"X1st.Max.Value"] < 0),]$Date.Local
d <- as.POSIXlt(d) 
d <- month.name[d$mon + 1]
table(factor(d,levels = month.name))

# let's explore data from one monitor
# first let's choose monitor with maximum abservations
site0 <- unique(subset(pm0,State.Code == 36, c(County.Code,Site.Num)))
site1 <- unique(subset(pm1,State.Code == 36, c(County.Code,Site.Num)))

site0 <- paste(site0[,1],site0[,2],sep='.')
site1 <- paste(site1[,1],site1[,2],sep='.')

intersection <- intersect(site0,site1)
intersection

pm0$County.Site <- with(pm0,paste(County.Code, Site.Num, sep = "."))
pm1$County.Site <- with(pm1,paste(County.Code, Site.Num, sep = "."))

cnt0 <- subset(pm0, State.Code == 36 & County.Site %in% intersection)
cnt1 <- subset(pm1, State.Code == 36 & County.Site %in% intersection)

# choose monitor with maximum abservations
sapply(split(cnt0,cnt0$County.Site),nrow)
sapply(split(cnt1,cnt1$County.Site),nrow)

# let's choose 5.110
intersection.county <- 5
intersection.site <- 110

pm0sub <- subset(pm0, State.Code == 36 & County.Code == intersection.county & Site.Num == intersection.site)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == intersection.county & Site.Num == intersection.site)

dates0 <- as.Date(pm0sub$Date.Local, "%Y-%m-%d")
x0sub <- pm0sub$X1st.Max.Value
dates1 <- as.Date(pm1sub$Date.Local, "%Y-%m-%d")
x1sub <- pm1sub$X1st.Max.Value

rng <- range(x0sub,x1sub,na.rm = T)
par(mfrow = c(1,2), mar = c(4,5,2,1))
plot(dates0,x0sub,ylim = rng, ylab = expression(PM[2.5]*"("*mu*g/m^3*")"),xlab = "1999")
abline(h = median(x0sub, na.rm = T))
plot(dates1,x1sub, ylim = rng,ylab = expression(PM[2.5]*"("*mu*g/m^3*")"),xlab = "2019")
abline(h = median(x1sub, na.rm = T))

pm0_2 <- select(pm0,State.Code,State.Name,County.Code,Site.Num,Date.Local,X1st.Max.Value)
head(pm0_2)
pm0_2 <- filter(pm0_2, State.Code == 36)
head(pm0_2)

# let's anylise changes in state wide scope
#1999
mn0 <- with(pm0,tapply(X1st.Max.Value,State.Code,mean,na.rm = T))
mn0_1 <- group_by(pm0,State.Code) %>% summarise(mean(X1st.Max.Value))

mn1 <- with(pm1,tapply(X1st.Max.Value,State.Code,mean,na.rm = T))
mn1_1 <- group_by(pm1,State.Code) %>% summarise(mean(X1st.Max.Value))

d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0,d1,by="state")
head(mrg)

par(mfrow = c(1,1))
rng <- range(mrg[,2],mrg[,3])
with(mrg,plot(rep(1,40), mrg[,2], xlim = c(0.5,2.5),ylim=rng,xaxt='n',xlab='',ylab='State-wide Mean PM'))
with(mrg,points(rep(2,40),mrg[,3]))
segments(rep(1,40),mrg[,2],rep(2,40),mrg[,3])
axis(1,c(1,2),c("1999","2019"))
axis



