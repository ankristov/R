library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("VIM")
library(VIM)
library(lubridate)
install.packages("dlookr")
library(dlookr)
library(dplyr)

# load data
market_data <- read.csv("/Users/Andrew/Documents/R/data/JatApp - Marketing analysis/marketing_data.csv", encoding = "utf-8")

head(market_data)
#summary(market_data)
names(market_data)
str(market_data)
#typeof(market_data$Income)

# assign data types to columns
market_data[,c(3,4,21:28)] <-  lapply(market_data[,c(3,4,21:28)],factor)
market_data$Income <- as.numeric(gsub("[$,]","",market_data$Income))
#market_data$Income <- as.numeric(gsub("[,]","",market_data$Income)) # for complaince with Power BI (can convert to decimal , not .)
market_data$Dt_Customer <- as.Date(market_data$Dt_Customer,"%m/%d/%y")

#str(market_data) 

#unique(market_data$Dt_Customer)
#test <- tibble(market_data$Dt_Customer,lubridate::day(market_data$Dt_Customer))
#head(test)
#str(test)

# Missing values
skimr::skim(market_data)
missing_values <- market_data %>% summarize_all(funs(sum(is.na(.))/n()))
head(missing_values)
missing_values <- gather(missing_values, key = "feature", value = "missing_pct")
head(missing_values)
missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = 'red') +
  coord_flip() + theme_bw()
# Conclusion: missing values in Income. We can drop rows (only 24) but let's better
# Impute Income with median by Country+Education
market_data_fixed <- 
  market_data %>%
  group_by(Country, Education) %>%
  mutate(Income_fixed = ifelse(is.na(Income),
                               median(Income, na.rm = T),
                               Income))
# check
# 1 manually, equallity with group median
market_data %>% 
  group_by(Country, Education) %>% summarise(median(Income,na.rm = T))
view(
  market_data_fixed %>% filter(is.na(Income)) %>% select(c(Education,Income, Country, Income_fixed))
  )
# 2
sum(is.na(market_data_fixed$Income_fixed))



skimr::skim_tee(market_data_fixed, )   

write_csv(market_data_fixed,file = "/Users/Andrew/Documents/R/data/JatApp - Marketing analysis/marketing_data_fixed.csv")

# Outliers
summary(market_data_fixed) # outliers in income & income_fixed
#explore by marital status
market_data_fixed %>% 
  select(Marital_Status, Income_fixed) %>%
  ggplot(aes(Marital_Status, Income_fixed, fill=Marital_Status)) + 
  geom_boxplot()+
  scale_fill_brewer(palette = "Set3")
#explore by country
market_data_fixed %>% 
  select(Country, Income_fixed) %>%
  ggplot(aes(Country, Income_fixed, fill=Country)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::dollar)
# rows from dataset with outliers
market_data_fixed %>% filter(Income_fixed >100000)
# Coclusion: Outliers among Together in SA. Let's leave them.

# pivot data on products to investigate shares sold
market_data_fixed_pivoted <- market_data_fixed %>%
  pivot_longer(cols = starts_with("Mnt"), names_to = "Products", values_to = "Amount")
unique(market_data_fixed_pivoted$Products)
# rename
market_data_fixed_pivoted <- market_data_fixed_pivoted %>%
  mutate(Products = case_when(
    Products == "MntWines" ~ "Wines",
    Products == "MntMeatProducts" ~ "Meat",
    Products == "MntSweetProducts" ~ "Sweet",
    Products == "MntFruits" ~ "Fruits",
    Products == "MntFishProducts" ~ "Fish",
    Products == "MntGoldProds" ~ "Gold",
  ))
view(market_data_pivoted)
write.csv(market_data_fixed_pivoted,file = "/Users/Andrew/Documents/R/data/JatApp - Marketing analysis/marketing_data_fixed_pivoted.csv")



pairs(market_data_fixed %>% select(starts_with("Mnt")))

# count of rows for different segments (important for statistical comparison)
# Country
market_data_fixed %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  ggplot(aes(Country,n,fill=Country)) + 
  geom_col() + scale_fill_brewer(palette = "Set3")

#market_data_fixed %>%
#  pivot_longer(cols = everything(), names_to="key", values_to="value") #%>%
#  ggplot(aes(value)) +
#  facet_wrap(~key, scales = "free") +
#  geom_col() + scale_fill_brewer(palette = "Set3")

# histograms of all numeric values
market_data_fixed_numeric <- market_data_fixed[,sapply(market_data_fixed, is.numeric)]
market_data_fixed_numeric %>%
  pivot_longer(cols = everything(), names_to="key", values_to="value") %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()  
# boxplots of all numeric values
market_data_fixed_numeric %>%
  pivot_longer(cols = everything(), names_to="key", values_to="value") %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_boxplot() 
  
# correlation
library(corrplot)
library(RColorBrewer)
corrplot(corr = cor(market_data_fixed_numeric),
         type = "upper",
         col = brewer.pal(n=length(names(market_data_fixed_numeric)),name = "RdYlBu"))

  
