library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(stringr)


df <- read.csv('/Users/Andrew/Documents/My Tableau Repository/Datasources/Superstore_Orders_1.csv', 
               header = T,
               sep = '\t')

f <- file('/Users/Andrew/Documents/My Tableau Repository/Datasources/Sample - Superstore_Orders.csv', 'r')
f <- file('/Users/Andrew/Documents/My Tableau Repository/Datasources/Superstore_Orders_1.csv', 'r')
readLines(con=f,n=2)
close(f)

head(df)
str(df)
summary(df)
as.numeric(gsub("[(]", "-", gsub("[$)]","", df$Profit[0:5])))
# Transform numeric variables from string to numeric
df$Profit_numeric <- as.numeric(gsub("[(]", "-", gsub("[$),]","", df$Profit)))
df$Sales_numeric <- as.numeric(gsub("[(]", "-", gsub("[$),]","", df$Sales)))
df$Sales_Forecast_numeric <- as.numeric(gsub("[(]", "-", gsub("[$),]","", df$Sales_Forecast)))
# Transform categorical variables to factors
df[,c("Category", "City", "Country_Region", "Customer_ID", "Postal_Code", "Product_Name", "Region", "Segment", "Ship_Mode", "Ship_Status", "Sub_Category")] <- lapply(df[,c("Category", "City", "Country_Region", "Customer_ID", "Postal_Code", "Product_Name", "Region", "Segment", "Ship_Mode", "Ship_Status", "Sub_Category")], factor)
# Transform date variables to date
df[, c("Ship_Date_date", "Order_Date_date")] <- lapply(df[, c("Ship_Date", "Order_Date")], mdy) # if use Date as fun -> a lot of missing values and conversions are incorrect!
df$Order_Date_year <- year(df$Order_Date_date)
df$Order_Date_month_str <- month(df$Order_Date_date, label = T)
df$Order_Date_month_num <- month(df$Order_Date_date, label = F)

df$Order_Date_month_year_per_order <- df$Order_Date_year + df$Order_Date_month_num * 0.01
df$Order_Date_month_year_str <-  str_c(df$Order_Date_month, ' ', as.character(df$Order_Date_year))
df$Order_Date_month_year_factor <- factor(x = df$Order_Date_month_year_str, 
                                          #levels = df %>% select(Order_Date_month_year_str, Order_Date_month_year_per_order) %>% arrange(Order_Date_month_year_per_order) %>% select(Order_Date_month_year_str) %>% distinct(),
                                          ordered = T)
df$Order_Date_month_year_factor <- reorder(df$Order_Date_month_year_factor, df$Order_Date_month_year_per_ordering)
levels(df$Order_Date_month_year_factor)

# Missing values 
df %>% summarise_all(list(~sum(is.na(.))/n())) %>% gather(key = 'variables', value = 'missing_pct') %>% arrange(desc(missing_pct))
# Check rows with Postal_Code NA
df %>% filter(is.na(Postal_Code))
# Check rows with Order_Date_date NA
df %>% select(Order_Date, Order_Date_date) %>% arrange(Order_Date_date)
# Check NA in Sales and Profit (check removing , from gsub)
df %>% group_by(Category) %>% 
  summarise(n = n(), 
            sum_profit = sum(Profit_numeric, na.rm = T), 
            sum_sales = sum(Sales_numeric, na.rm = T), 
            sum_na_profit = sum(is.na(Profit_numeric)),
            sum_na_sales = sum(is.na(Sales_numeric)))
### result before remove , from Profit and Sales
# A tibble: 3 × 6
# Category            n sum_profit sum_sales sum_na_profit sum_na_sales
#   <chr>           <int>      <dbl>     <dbl>         <int>        <int>
# 1 Furniture        2121      21961    455168             4          169
# 2 Office Supplies  6026     114282    467535            27          122
# 3 Technology       1847     102809    381463            33          180
### result after remove , from Profit and Sales
# A tibble: 3 × 6
# Category            n sum_profit sum_sales sum_na_profit sum_na_sales
#   <chr>           <int>      <dbl>     <dbl>         <int>        <int>
# 1 Furniture        2121      18444    742006             0            0
# 2 Office Supplies  6026     122474    719127             0            0
# 3 Technology       1847     145429    836221             0            0
### check rows with Profit = NA, why? The cause of NA: need to remove , from string Profit and Sales ^
df %>% select(Profit, Profit_numeric, Sales, Sales_numeric) %>% filter(is.na(Profit_numeric))
#      Profit Profit_numeric   Sales Sales_numeric
# 1  ($1,665)             NA  $3,083            NA
# 2  ($1,360)             NA  $8,160            NA
# 3    $1,996             NA  $3,992            NA
# 4    $1,415             NA  $4,355            NA
# 5    $3,177             NA  $6,355            NA
# 6    $1,380             NA  $3,000            NA
# 7  ($3,840)             NA  $8,000            NA
# 8    $1,276             NA  $2,716            NA
### Statistical metrics
df %>% group_by(Category) %>% 
  summarise(n = n(), 
            sum_profit = sum(Profit_numeric, na.rm = T), 
            sum_sales = sum(Sales_numeric, na.rm = T), 
            mean_profit= mean(Profit_numeric),
            mean_sales = mean(Sales_numeric),
            mean_profit_trim = mean(Profit_numeric, trim = 0.1),
            mean_sales_trim = mean(Sales_numeric, trim = 0.1),
            median_profit = median(Profit_numeric),
            median_sales = median(Sales_numeric),
            sd_profit = sd(Profit_numeric),
            sd_sales = sd(Sales_numeric))
# Per sub-category
df %>% group_by(Sub_Category) %>% 
  summarise(n = n(), 
            sum_profit = sum(Profit_numeric, na.rm = T), 
            sum_sales = sum(Sales_numeric, na.rm = T), 
            mean_profit= mean(Profit_numeric),
            mean_sales = mean(Sales_numeric),
            mean_profit_trim = mean(Profit_numeric, trim = 0.1),
            mean_sales_trim = mean(Sales_numeric, trim = 0.1),
            median_profit = median(Profit_numeric),
            median_sales = median(Sales_numeric),
            sd_profit = sd(Profit_numeric),
            sd_sales = sd(Sales_numeric))
# Per sub-category
df %>% group_by(Sub_Category) %>% 
  summarise(n = n(), 
            sum_profit = sum(Profit_numeric, na.rm = T), 
            sum_sales = sum(Sales_numeric, na.rm = T), 
            mean_profit= mean(Profit_numeric),
            mean_sales = mean(Sales_numeric),
            mean_profit_trim = mean(Profit_numeric, trim = 0.1),
            mean_sales_trim = mean(Sales_numeric, trim = 0.1),
            median_profit = median(Profit_numeric),
            median_sales = median(Sales_numeric),
            sd_profit = sd(Profit_numeric),
            sd_sales = sd(Sales_numeric),
            ) %>%
  arrange(desc(mean_profit))
# Need to remove outliers to compaire averages. 
# Calculate 90% and 10% percetiles for each sub-category
df_summary_1 <- df %>% group_by(Sub_Category) %>% summarise(min_profit = min(Profit_numeric),
                                            p10_profit = quantile(Profit_numeric, probs = 0.10),
                                            p90_profit = quantile(Profit_numeric, probs = 0.90),
                                            max_profit = max(Profit_numeric),
                                            min_sales = min(Sales_numeric),
                                            p10_sales = quantile(Sales_numeric, probs = 0.10),
                                            p90_sales = quantile(Sales_numeric, probs = 0.90),
                                            max_sales = max(Sales_numeric))
df_summary_1
# Join
df_1 <- df %>% left_join(df_summary_1)
head(df_1)
# Check join
df_1 %>% filter(Sub_Category == 'Copiers')
# Plot stat metrics for subcategories
ggplot(data = df_1 %>% filter(Profit_numeric < p90_profit & Profit_numeric > p10_profit)) + 
  geom_boxplot(aes(x = Sub_Category, y = Profit_numeric, fill = Sub_Category))
# Plot Profit in subcategories on timeline
df_1_summarise_date_no_outliers <- df_1 %>% 
  filter(Profit_numeric < p90_profit & Profit_numeric > p10_profit) %>% 
  group_by(Order_Date_month_year_factor, Sub_Category) %>% 
  summarise(sum_profit = sum(Profit_numeric),
            mean_profit = mean(Profit_numeric),
            n_orders = n()) %>%
  arrange(Order_Date_month_year_factor)
head(df_1_summarise_date_no_outliers)
ggplot(data = df_1_summarise_date_no_outliers) +
  geom_col(aes(x = Order_Date_month_year_factor, y = sum_profit, fill = Sub_Category))
