# downloading files
# check directory
if (!file.exists("data")) {
  dir.create("data")
}
fileURL <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileURL, destfile = "./data/cameras.csv",method = "curl") # method curl for Mac for https
# we need to keep track of date when file was downloaded
dateDownloaded <- date()
dateDownloaded

# reading local files
# csv
cameraData <- read.table("./data/cameras.csv")
head(cameraData)
readLines(con = file("./data/cameras.csv"), n = 1)
cameraData <- read.table("./data/cameras.csv", sep = ",", header = T)
head(cameraData)
cameraData <- read.csv("./data/cameras.csv")
str(cameraData) # reads all columns as factors!
cameraData <- read_csv("./data/cameras.csv")
# xlsx
# XLConnect - package with more options for xls
if (!file.exists("data")) {dir.create("data")}
fileURL <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
download.file(fileURL, destfile = "./data/cameras.xlsx", method = "curl")
dateDownloaded <- date()
install.packages("xlsx")
install.packages("rJava") # xlsx depends o this package
install.packages("xlsxjars") # xlsx depends o this package
library("xlsx")
cameraData <- read.xlsx("./data/cameras.xlsx", sheetIndex = 1, head = T)
colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./data/cameras.xlsx", sheetIndex = 1, head = T,
                              colIndex = colIndex, rowIndex = rowIndex)
# XML
# http://www.stat.berkeley.edu/~statcur/Workshop2/Presentations/XML.pdf
install.packages("XML")
library(XML)
library(methods)
fileURL <- "http://www.w3schools.com/xml/simple.xml"
download.file(fileURL, destfile = "./data/simpe.xml")
doc <- xmlTreeParse("http://www.w3schools.com/xml/simple.xml", useInternalNodes = T, asText = T) # error
rootNode <- xmlRoot(doc) # error
xmlName(rootNode) # error
doc <- xmlParse(file = fileURL) # error
doc <- xmlParse(file = "./data/simpe.xml") # works
print(doc)
rootNode <- xmlRoot(doc)
xmlSize(rootNode)
rootNode[1]
rootNode[[1]]
rootNode[[1]][[1]]
rootNode[[1]][[2]]
rootNode[[1]][[3]]
rootNode[[1]][[4]]
rootNode[[1]][[5]]
nrow(rootNode[[1]])
xmlDataFrame <- xmlToDataFrame("./data/simpe.xml")
xmlDataFrame <- as_tibble(xmlDataFrame)
names(xmlDataFrame)
library(xml2)
doc <- read_xml(fileURL)

# json
# tutorial www.r-bloggers.com/new-packages-jsonlite-a-smarter-json-encoderdecoder/
install.packages("jsonlite")
library(jsonlite)
jsonData <- fromJSON(url("api.github.com/users/jtleek/repos")) # error
download.file("api.github.com/users/jtleek/repos", destfile = "./data/simple.json")
jsonData <- fromJSON("./data/simple.json")
names(jsonData)
jsonData[1]
names(jsonData$owner)
jsonData$owner$login
myJSON <- toJSON(iris, pretty = T)
myJSON
iris2 <- fromJSON(myJSON)
head(iris2)
head(iris)

# data.table
# r-forge.r-project.org/scm/viewvc.php/pkg/NEWS?view=markup&root=datatable
# differences between data.table and data.frame http://stackoverflow.com/questions/13618488/what-you-can-do-with-data-frame-what-you-cant-in-data-table

# practical exercise
install.packages("swirl")
packageVersion("swirl")
library(swirl)
ls()
rm(list = ls())
install_from_swirl("Getting and Cleaning Data")
swirl()

# mysql
# http://cran.r-project.org/web/packages/RMySQL/RMySQL.pdf
# List of commands http://www.pantz.org/software/mysql/mysqlcommands.html
# Nice blog post http://www.r-bloggers.com/mysql-and-r/
install.packages("RMySQL")
library(RMySQL)
ucscDb <- dbConnect(MySQL(),user="genome",
                    host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb,"show databases;")
result
dbDisconnect(ucscDb)

hg19 <- dbConnect(MySQL(), 
                 user = "genome",
                 db = "hg19",
                 host = "genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
allTables
length(allTables)
allTables[1:20]
dbListFields(hg19,"affyU133Plus2")
dbGetQuery(hg19,"select count(*) from affyU133Plus2")
affyData <- dbReadTable(hg19, "affyU133Plus2")
affyData <- as_tibble(affyData)
head(affyData)
query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
query
affyMis <- fetch(query)
head(affyMis)
quantile(affyMis$misMatches)
affyMisSmall <- fetch(query, n = 10)
head(affyMisSmall)
dbClearResult(query) # important to clear
dim(affyMisSmall)
dbDisconnect(hg19) # important to close

promusculusLocalDB <- dbConnect(MySQL(),
                                user = "skoblile_wp",
                                password = "QJjuKllBG",
                                host = "127.0.0.1",
                                dbname = "skoblile_wp3",
                                port = 8888)
                                
# hdf5
# http://www.hdfgroup.org/
# https://bioconductor.org/install/
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("rhdf5")
library("rhdf5")
created = h5createFile("example.h5")
created
created = h5createGroup("example.h5","foo")
created = h5createGroup("example.h5","baa")
created = h5createGroup("example.h5","foo/foobaa")
h5ls("example.h5")
A = matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A")
h5ls("example.h5")
B = array(seq(0.1,2.0,by=0.1), dim = c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
h5ls("example.h5")
df = data.frame(1L:5L, seq(0,1,length.out = 5),
                c("ab","cde","fghi","a","s"), stringsAsFactors = F)
h5write(df,"example.h5", "df")
h5ls("example.h5")
readA = h5read("example.h5", "foo/A")
readB = h5read("example.h5", "foo/foobaa/B")
readdf = h5read("example.h5", "df")
readA
readB
readdf
h5write(c(12,13,14), "example.h5", "foo/A", index = list(1:3,1))
h5read("example.h5", "foo/A")
h5read("example.h5", "foo/A", index = list(1:3,1))

# reading from the web
# How Netflix reverse engineered Hollywood https://www.theatlantic.com/technology/archive/2014/01/how-netflix-reverse-engineered-hollywood/282679/
con <- url("http://promusculus.ru")
htmlCode <- readLines(con)
close(con)
htmlCode
library(XML)
url <- "https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
url <- "https://promusculus.ru"

html <- htmlTreeParse(url, useInternalNodes = T) # error: do not download but interpret url like xml
html <- htmlTreeParse(htmlCode, useInternalNodes = T) #works (it seems all these functions work only with downloaded content, not url)
xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//description", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)
library(httr)
# http://cran.r-project.org/web/packages/httr/httr.pdf
# a lot of examples how to scrap data from the web http://www.r-bloggers.com/ search "Web+Scrapping"
html2 <- GET(url)
content2 <- content(html2, as = "text")
parsedHtml <- htmlParse(content2, asText = T)
xpathSApply(parsedHtml, "//title", xmlValue)
pg2 <- GET("http://httpbin.org/basic-auth/user/passwd")
pg2
pg2 <- GET("http://httpbin.org/basic-auth/user/passwd",
           authenticate("user", "passwd"))
pg2
names(pg2)
google <-  handle("https://google.com")
pg1 <- GET(handle = google, path = "/")
pg2 <- GET(handle = google, path = "search")

# API
myapp <- oauth_app("twitter",
                   key = "yourKey",
                   secret = "yourSecret")
sig <- sign_oauth1.0(myapp,
                     token = "yourToken",
                     token_secret = "yourTokenSecret")
homeTL <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json")
json1 <- content(homeTL)
json2 <- jsonlite::fromJSON(toJSON(json1))
json2[1,1:4]
# How many characters are in the 10th, 20th, 30th and 100th lines of HTML from this page
con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
f <- readLines(con, n = 100)
f[10]
v <- c(f[10],f[20],f[30],f[100])
nchar(f[10])
nchar(v)
close(con)

# Summarizing Data
if (!file.exists("./data")) {dir.create("./data")}
fileURL <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv"
download.file(fileURL, "./data/restaurants.csv", method = "curl")
restData <- read.csv("./data/restaurants.csv")
head(restData, n = 3)
tail(restData, n = 3)
summary(restData)
str(restData)
levels(restData$name)
levels(restData$neighborhood)
levels(restData$Location.1)
levels(restData$policeDistrict)
quantile(restData$councilDistrict, na.rm = T)
quantile(restData$councilDistrict, probs = c(0.5,0.75,0.9))
table(restData$zipCode, useNA = "ifany")
table(restData$councilDistrict,restData$zipCode)
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
any(restData$councilDistrict == 5)
any(restData$councilDistrict == 15)
all(restData$zipCode > 0)
colSums(is.na(restData))
all(colSums(is.na(restData)) == 0)
table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))
restData[restData$zipCode %in% c("21212","21213"),]

data("UCBAdmissions")
DF <- as.data.frame(UCBAdmissions)
summary(DF)
head(DF)
xt <- xtabs(Freq ~ Gender + Admit, data = DF)
xt

warpbreaks$replicate <- rep(1:9,len = 54)
xt <- xtabs(breaks ~ ., data = warpbreaks)
xt
ftable(xt)

fakeData <- rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData), units = "Mb")

seq(1,10,by=2)
seq(1,10,length=3)
seq(along = c(1,2,8,25,100))

restData$nearMe <- restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)
restData$zipWrong <- ifelse(restData$zipCode < 0, T, F)
table(restData$zipWrong)
restData$zipGroups = cut(restData$zipCode, breaks = integer(quantile(restData$zipCode)))
table(restData$zipGroups)
restData$zcf <- factor(restData$zipCode) 
restData$zcf[1:10]
class(restData$zcf)

# Reshaping Data
library(reshape2)
head(mtcars)
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id.vars = c("carname", "gear", "cyl"), measure.vars = c("mpg", "hp"))
head(carMelt)
tail(carMelt)
cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData

# Merging Data
if (!file.exists("./data")) {dir.create("./data")}
fileURL1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileURL2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
df1 <- data.frame(id = sample(1:10), x = rnorm(10))
df2 <- data.frame(id = sample(1:10), y = rnorm(10))
join(df1,df2)
df1 <- data.frame(id = sample(1:10,8), x = rnorm(8))
join(df1,df2)
left_join(df1,df2)
right_join(df1,df2)

df1 <- data.frame(id = sample(1:10), x = rnorm(10))
df2 <- data.frame(id = sample(1:10), y = rnorm(10))
df3 <- data.frame(id = sample(1:10), y = rnorm(10))
dfList <- list(df1,df2,df3)
join_all(dfList)

library(nycflights13)
head(flights)
head(airports)
intersect(names(flights), names(airports))
f <- inner_join(flights,airports, by = c("dest" = "faa"))
select(f,dest, name)

# Match the data based on the country shortcode. 
# How many of the IDs match? Sort the data frame in descending order by GDP rank (so United States is last). 
# What is the 13th country in the resulting data frame?
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url1, "./data/file1.csv")
download.file(url2, "./data/file2.csv")
df1 <- as_tibble(read.csv("./data/file1.csv", stringsAsFactors = F))
df2 <- as_tibble(read.csv("./data/file2.csv", stringsAsFactors = F))
head(df1,20)
head(df2,20)
length(intersect(df1$X, df2$CountryCode))
length(df1$X)
length(df2$CountryCode)

# Editing Text Variables
if (!file.exists("./data")) {dir.create("./data")}
fileURL <- "http://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileURL, destfile = "./data/cameras.csv", method = "curl")
cameraData <- read.csv("./data/cameras.csv")
names(cameraData)
tolower(names(cameraData))
splitNames <-  strsplit(names(cameraData), "\\.")
splitNames
splitNames[[6]][1]
strsplit("Hi. My name is Andrew. Welcom to my world", split = "\\.")
strsplit("Hi. My name is Andrew. Welcom to my world", split = "\\ ")
sapply(splitNames, function(x){x[1]})
s <- c("solutions_id", "review_id", "time_left")
sub("_", "", s)
s <- "this_is_a_test_string"
sub("_", "", s)
gsub("_", " ", s)
grep("Alameda", cameraData$intersection)
grep("Alameda", cameraData$intersection, value = T)
grepl("Alameda", cameraData$intersection)
table(grepl("Alameda", cameraData$intersection))
cameraData[grepl("Alameda", cameraData$intersection),]
grep("JeffStreet", cameraData$intersection)
length(grep("JeffStreet", cameraData$intersection))
library(stringr)
nchar("Andrew Kristov")
substr("Andrew Kristov",8,14)
paste("Andrew", "Kristov")
paste0("Andrew", "Kristov")
str_trim("Andrew       ")
# Rules for variables:
# lowercase, descriptive (Diagnosis vs. Dx, TRUE/FALSE vs. 1/0, Male/Female vs. 1/0 or M/F), 
# not duplicated, without dots and underscore, 
# var with character -> factors

# Reqular expressions
# 9.11 any character between 9 and 11
# ^[Gg]ood|[Bb]ad
# ^([Gg]ood|[Bb]ad)
# [Gg]eorge( [Ww]\.)? [Bb]ush
# (.*) any caracter in parenthesis any number of times
# [0-9]+ at least one number
# [0-9]+ (.*)[0-9]+ at least one number followed by any characters folowed by at least one number
# {Bb}ush( +[^ ]+ +){1,5} debate - Bush or bush (followed by at least 1 space followed by any at least one not space followed by at least one space) and this is 1 to 5 times before 'debate' 
# "Bush has historical won all major debates he's done."
# +([a-zA-Z]+) +\1 at least one space followed by at least one letter followed by at least one space AND followed the same pattern (repetitive words)
# "time for bed, night night twitter!", "my tatoo is so so itchy today"
# ^s(.*)s will match longest possible string
# "sitting at starbucks" will match all string
# ^s(.*?)s will stop on first 's'

s <- "idsfjd dskf 9-11 lkdsjfd lksdjfds 9/11, skflsdkjf 203.169.114.66. sdklfj kkc 9:11:46 AM"
gregexpr("9.11", s)
grepl("9.11", s)
grep("9.11", s, value = T)

# Date
d1 <- date()
d2 <- Sys.Date()
class(d1)
class(d2)
format(d2,"%a %b %d") 
# %d day as number (0-31), 
# %a abbreviated weekday, %A unabbreviated weekday 
# %m month (0-12), %b abbreviated month, %B unabbreviated month
# %y two digits year, %Y four digits year
format(d2,"%d %a %A %m %b %B %y %Y") 
x <- c("1jan1960", "2feb1960", "12jul1983")
z <- as.Date(x, "%d%b%Y")
z
z[2]-z[1]
as.numeric(z[2]-z[1])
weekdays(d2)
months(d2)
julian(d2)  
library(lubridate)
ymd("20140108")
mdy("08/04/2013")
dmy("03-04-2013")
ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03", tz = "Pacific/Auckland")
?Sys.timezone
# http://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/
x <- dmy("12jul1983")
wday(x)
wday(x, label = T)

# Data recources
# http://gapminder.org
# http://www.asdfree.com - surveys data from US
# http://www.infochimps.com/marketplace - a lot of different datasets
# http://www.kaggle.com/
# Collections by famous data scientists 
# Hilary Mason bitly.com/bundles/hmason/1,
# Peter Skomoroch https://delicious.com/pskomoroch/datasets
# Jeff Hammerbarcher http://www.quora.com/Jeff-Hammerbacher/Introduction-to-Data-Science-Data-Sets
# Gregory Piatetsky-Shapiro http://www.kdnuggets.com/gps.html
# http://blog.mortardata.com/post/67652898761/6-dataset-lists-curated-by-data-scientists
# Stanford Large Newtork Data
# UCI Machine Learning
# KDD Nugets Datasets
# CMU Statlib
# Gene expression omnibus
# ArXiv Data
# Public Data Sets on Amazon Web Services

# Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.
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






