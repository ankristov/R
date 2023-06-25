# https://netpeak.net/ru/blog/kak-vizualizirovat-pokazatel-kachestva-klyuchevyh-slov-retsept-skripta-na-yazyke-r/

install.packages("devtools")
install.packages("RGoogleAnalytics")
install_github('jburkhardt/RAdwords')
install.packages("googlesheets")
remove.packages("googlesheets")
install.packages("ggplot2")
install.packages("RCurl")
install.packages("dplyr")
require("devtools")
require(RGoogleAnalytics)
require(RAdwords)
#require(googlesheets)
require(ggplot2)
require(RCurl)
library(dplyr)
system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")
curlVersion()$protocol

# Fix get data function for Google Ads
#getDataFix <- function (clientCustomerId, google_auth, statement, apiVersion = "201708",
#                     transformation = TRUE, changeNames = TRUE, includeZeroImpressions = FALSE,
#                     verbose = FALSE)
#{
#  access <- google_auth$access
#  credlist <- google_auth$credentials
#  if (as.numeric(Sys.time()) - 3600 >= access$timeStamp) {
#    access <- refreshToken(google_auth)
#  }
#  google.auth <- paste(access$token_type, access$access_token)
#  cert <- system.file("CurlSSL", "ca-bundle.crt", package = "RCurl")
#  data <- RCurl::getURL(paste("https://adwords.google.com/api/adwords/reportdownload/v",
#                              apiVersion, sep = ""), httpheader = c(Authorization = google.auth,
#                                                                    developerToken = credlist$auth.developerToken, clientCustomerId = clientCustomerId,
#                                                                    includeZeroImpressions = includeZeroImpressions), postfields = statement,
#                        verbose = verbose, ssl.verifypeer = TRUE)
#  valid <- grepl(attr(statement, "reportType"), data)
#  if (transformation & valid) {
#    data <- transformData(data, report = attributes(statement)$reportType,
#                          apiVersion = apiVersion)
#    if (changeNames) {
#      data <- changeNames(data)
#    }
#  }
#  data
#}






# Init Google Ads and Google Analytics
clienid <- "475692164369-e6jmtfc7o508tvsu2doqsb2vrtofj0cp.apps.googleusercontent.com"  #Client ID ???? google cloud platform
secret  <- "i4nl7KHuRDYFXH0OIfmaeVnj"  #Secret ???? Client ID ???? google cloud platform
adwords_token <- "n1IA_l_IpDghYJ4_vSkg9A"  #Token AdWords API
ga_view <- "ga:184056816"  #View ID Google Analytics
adwordsID <- "142-776-5331"  #Google Ads account ID

#Date period
start_period <- c(day = "01",month = "09",year = "2020")
end_period <- c(day = "28",month = "02",year = "2021")

# Google Analytics Authentication
ga_auth <- Auth(clienid,secret)
# Google Ads Authentication
adwords_auth <- doAuth(F)

# Load data from google analytics
ValidateToken(ga_auth)
query.init <- Init(start.date = paste(start_period["year"],start_period["month"],start_period["day"],sep = "-"),     
                   end.date = paste(end_period["year"],end_period["month"],end_period["day"],sep = "-"),     
                   dimensions = "ga:adwordsCriteriaID, ga:keyword, ga:adGroup, ga:campaign, ga:adwordsAdGroupID, ga:adwordsCampaignID",     
                   metrics = "ga:impressions",
                   filters = "ga:medium==cpc;ga:source==google",
                   table.id = ga_view,     
                   max.results = 10000)
ga.query <- QueryBuilder(query.init)
gaData <- GetReportData(ga.query, ga_auth, split_daywise = FALSE, paginate_query = FALSE)
head(gaData)
dim(gaData)
View(gaData)

#Load data from Google AdWords
body <- statement(select=c('AccountDescriptiveName','Id','FinalUrls','Status','AdGroupId','AdGroupStatus','CampaignId','CampaignStatus','Impressions','Clicks','Cost','Ctr','AveragePosition','CreativeQualityScore','PostClickQualityScore','SearchPredictedCtr','QualityScore'),
                  report="KEYWORDS_PERFORMANCE_REPORT",
                  start=paste0(start_period["year"],start_period["month"],start_period["day"]),
                  end=paste0(end_period["year"],end_period["month"],end_period["day"]))
#body <- statement(select=c('CampaignName','Clicks','Cost','Ctr'),
#                  report="CAMPAIGN_PERFORMANCE_REPORT",
#                  start="2020-10-01",
#                  end="2021-02-01")
adwordsData <- getData(clientCustomerId = adwordsID, google_auth = adwords_auth, statement = body, transformation = T, apiVersion = "201809")
#adwordsData <- getDataFix(clientCustomerId = adwordsID, google_auth = adwords_auth, statement = body, transformation = T, apiVersion = "201809",verbose=F)
head(adwordsData)
dim(adwordsData)
View(adwordsData)

# Merge data from Google Analytics and Google AdWords
totalDataRaw <- merge(adwordsData, 
                      gaData,      
                      by.x = c("KeywordID", "AdgroupID", "CampaignID"),      
                      by.y = c("adwordsCriteriaID","adwordsAdGroupID","adwordsCampaignID"),      
                      all.adwordsData = TRUE)
head(totalDataRaw)
dim(totalDataRaw)

totalData <- data.frame(Campaign = totalDataRaw$campaign,      
                        AdGroup = totalDataRaw$adGroup,      
                        KeyWord = totalDataRaw$keyword,      
                        Impressions = totalDataRaw$Impressions,      
                        Clicks = totalDataRaw$Clicks,      
                        Cost = totalDataRaw$Cost,      
                        CTR = totalDataRaw$CTR,      
                        Position = totalDataRaw$Position,      
                        Qualityscore = totalDataRaw$Qualityscore,      
                        Adrelevance = totalDataRaw$Adrelevance,      
                        Landingpageexperience = totalDataRaw$Landingpageexperience,      
                        Expectedclickthroughrate = totalDataRaw$Expectedclickthroughrate)
head(totalData,10)

# Categorize QualityScore on Low, Middle, High
totalData$Qualityscore <- as.numeric(totalData$Qualityscore)
totalData$QSGroup <- ifelse(totalData$Qualityscore <= 4, "Low", ifelse(totalData$Qualityscore <= 7, "Middle", "High"))
#head(totalData[,c("Qualityscore","QSGroup")],20)

# Visualization
#adrelevance <- totalData[,c(1,10)]
#landingpageexperience <- totalData[,c(1,11)]
#expectedclickthroughrate <- totalData[,c(1,12)]
#qsgroup <- totalData[,c(1,13)]
#avgQS <- aggregate(as.integer(totalData$Qualityscore) ~ totalData$Campaign, FUN = "mean")
#colnames(avgQS) <- c("Campaign","QS")
campaign_avgQS <- totalData %>% select(Campaign,Qualityscore) %>% group_by(Campaign) %>% summarise(Qualityscore=mean(Qualityscore))
#avgQS

# Visualization: mean quality score for campaigns
(g1 <- ggplot(campaign_avgQS, aes(x = Campaign, y = Qualityscore)) +  
  geom_bar(stat = "identity", position = "dodge", fill = "cyan4") +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +  
  ggtitle("Average Quality Score by Campaign"))

# Vizualization: Proportion of keywords with different Quality Score in campaigns
(g2 <- ggplot(totalData, aes(x = Campaign, fill = QSGroup)) +  
  geom_bar(stat = "count", position = "fill") +  
  scale_fill_manual(breaks=c("High","Middle","Low"),values=c(High = "forestgreen", Middle = "tan1" , Low = "firebrick1")) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7), axis.text.y = element_text(size = 7)) +  
  ggtitle("Number of keywords by Quality Score Group"))

# Vizualization: Proportion of keywords with different Adrelevance in Adgroups
(g3 <- ggplot(totalData, aes(x = AdGroup, fill = Adrelevance)) +  
  geom_bar(stat = "count", position = "fill") +   
  scale_fill_manual(values=c("forestgreen", "tan1" , "firebrick1","grey" )) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +  
  ggtitle("AdRelevance by Campaign"))

#Визуализация по качеству целевой страницы
(g4 <- ggplot(totalData, aes(x = Campaign, fill = Landingpageexperience)) +
  geom_bar(stat = "count", position = "fill") +  
  #scale_fill_manual(values=c("forestgreen", "tan1" , "firebrick1","grey" )) +
  scale_fill_brewer(palette = "Pastel2") +    #Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +  
  ggtitle("Landing page experience by Campaign"))









