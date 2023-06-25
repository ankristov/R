# https://netpeak.net/ru/blog/kak-otsenit-poteryannyi-dokhod-v-google-adwords-s-pomoshch-yu-yazyka-r/

#1. Установка и подключение требуемых пакетов
install.packages("curl")
require("curl")
install.packages("bitops")
require("bitops")
install.packages("devtools")
require("devtools")
install.packages("RGoogleAnalytics")
require(RGoogleAnalytics)
install_github('jburkhardt/RAdwords')
require(RAdwords)

#2. Объявление переменных
clienid <- "475692164369-e6jmtfc7o508tvsu2doqsb2vrtofj0cp.apps.googleusercontent.com" #Client ID из google 
secret <- "i4nl7KHuRDYFXH0OIfmaeVnj" #Client secret из google 
ga_view <- "ga:184056816" #ID представления из Google Analytics
adwords_id <- "309-670-8803" #ID аккаунта Google AdWords
#Период, за который необходимо определить объём потерянного дохода
start_period  <- c(day = "01",month = "01",year = "2016")
end_period    <- c(day = "20",month = "03",year = "2016")

#3. Аутентификация в сервисах.
#3.1. Аутентификация в Google Analytics
ga_auth <- Auth(clienid,secret)

#3.2. Аутентификация в Google AdWords
adwords_auth <- doAuth(F)

#4.Запрос данных из сервисов.
#4.1. Получение данных из Google Analytics
#4.1.1. Описание запроса к Google Analytics
query.list <- Init(start.date = paste(start_period["year"],start_period["month"],start_period["day"],sep = "-"),
                   end.date = paste(end_period["year"],end_period["month"],end_period["day"],sep = "-"),
                   dimensions = "ga:adwordsCampaignID, ga:campaign",
                   metrics = "ga:transactions,ga:transactionRevenue",                   
                   filters = "ga:medium==cpc, ga:source==google",                   
                   table.id = ga_view)
#4.1.2. Создание объекта API запроса к Google Analytics
ga.query <- QueryBuilder(query.list)
#4.1.3. Получение данных из Google Analytics в R
gaData <- GetReportData(ga.query, ga_auth, split_daywise = FALSE, paginate_query = FALSE)
#4.2. Получение данных из Google AdWords
#4.2.1. Описание API запроса к Google AdWords.
body <- statement(select=c('CampaignId','Impressions','Clicks','Cost','Ctr',
                           'SearchBudgetLostImpressionShare',
                           'SearchRankLostImpressionShare  ',
                           'ContentBudgetLostImpressionShare',                           
                           'ContentRankLostImpressionShare'),                  
                  report="CAMPAIGN_PERFORMANCE_REPORT",                  
                  start=paste0(start_period["year"],
                               start_period["month"],
                               start_period["day"]),                  
                  end=paste0(end_period["year"],
                             end_period["month"],
                             end_period["day"]))
#4.2.2. Отправка запроса в Google AdWords
adwordsData <- getData(clientCustomerId = adwords_id,                
                       google_auth = adwords_auth,                
                       statement = body,                
                       transformation = T,                
                       apiVersion = "201605")
#5. Подготовка итоговой таблицы.
#5.1. Соединяем данные из Google Analytics и Google AdWords в одну таблицу
totalData <- merge(gaData, adwordsData, 
                   by.x = "adwordsCampaignID", 
                   by.y = "CampaignID", 
                   all.x = TRUE)
#5.2. Заменяем пропущенные значения нулями
for (i in 1:length(totalData)){  
  totalData[which(is.na(totalData[i])),i] <- 0}
#5.3. Итоговые вычисления количества потерянных транзакций и дохода
totalData$lostImpressionByBudgetSearch  <- round(totalData$Impressions / (1-totalData$SearchLostIS_budget) - totalData$Impressions,0)
totalData$lostImpressionByRankSearch    <- round(totalData$Impressions / (1-totalData$SearchLostIS_rank) - totalData$Impressions,0)
totalData$lostImpressionByBudgetDisplay <- round(totalData$Impressions / (1-totalData$ContentLostIS_budget) - totalData$Impressions,0)
totalData$lostImpressionByRankDisplay   <- round(totalData$Impressions / (1-totalData$ContentLostIS_rank) - totalData$Impressions,0)
totalData$lostImpressionByBudget        <- totalData$lostImpressionByBudgetSearch + totalData$lostImpressionByBudgetDisplay
totalData$lostImpressionByRank          <- totalData$lostImpressionByRankSearch  + totalData$lostImpressionByRankDisplay
totalData$lostClicksByBudget            <- round(totalData$lostImpressionByBudget * (totalData$CTR),0)
totalData$lostClicksByRank              <- round(totalData$lostImpressionByRank * (totalData$CTR),0)
totalData$lostTransactionsByBudget      <- ifelse(is.nan(round(totalData$lostClicksByBudget * (totalData$transactions / totalData$Clicks),0)),0,round(totalData$lostClicksByBudget * (totalData$transactions / totalData$Clicks),0))
totalData$lostTransactionsByRank        <- ifelse(is.nan(round(totalData$lostClicksByRank * (totalData$transactions / totalData$Clicks),0)),0,round(totalData$lostClicksByRank * (totalData$transactions / totalData$Clicks),0))
totalData$lostTransactions              <- totalData$lostTransactionsByBudget + totalData$lostTransactionsByRank
totalData$lostRevenueByBudget           <- ifelse(is.nan(round(totalData$lostTransactionsByBudget * (totalData$transactionRevenue / totalData$transactions),0)), 0,round(totalData$lostTransactionsByBudget * (totalData$transactionRevenue / totalData$transactions),0))
totalData$lostRevenueByRank             <- ifelse(is.nan(round(totalData$lostTransactionsByRank * (totalData$transactionRevenue / totalData$transactions),0)), 0, round(totalData$lostTransactionsByRank * (totalData$transactionRevenue / totalData$transactions),0))
totalData$lostRevenue                   <- ifelse(is.nan(totalData$lostRevenueByBudget + totalData$lostRevenueByRank),0,totalData$lostRevenueByBudget + totalData$lostRevenueByRank)
#6. Выгрузка рассчитаной таблицы в csv файл
write.table(totalData, file='lostRevenue.csv', sep = ";", dec = ",", row.names = FALSE)
#7. Визуализация в виде круговой лиаграммы
lost_revenue <- c('полученный доход' = sum(totalData$transactionRevenue), 'потерянный по бюджету' = sum(totalData$lostRevenueByBudget), 'потерянный по рейтингу' = sum(totalData$lostRevenueByRank))
pie(lost_revenue,col = c("green", "red", "firebrick"))













