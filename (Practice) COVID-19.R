library(tidyr)
library(lubridate)

ls(all.names = T)
rm(list = ls(all.names = T))

#############################################
# Data load and preprocess
#############################################
download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
              method = 'curl', destfile = 'data/time_series_covid19_confirmed_global.csv')
download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
              method = 'curl', destfile = 'data/time_series_covid19_deaths_global.csv')
download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
              method = 'curl', destfile = 'data/time_series_covid19_recovered_global.csv')

df_confirmed_global <- read_csv('data/time_series_covid19_confirmed_global.csv')
df_deaths_global <- read_csv('data/time_series_covid19_deaths_global.csv')
df_recovered_global <- read_csv('data/time_series_covid19_recovered_global.csv')

# Functions
make_df_tidy <- function(df, case_type){
  df <- df %>% select(-('Province/State')) %>% rename('Country' = 'Country/Region')
  df$type <- case_type            # type of cases (confirmed, deaths, recovered)
  names(df) <- tolower(names(df)) # column names to lower case
  df$date <- mdy(df$date)         # date string -> date type
  df
} 
make_df_tableau <- function(df1, df2, df3){
  df_res <- df1 %>% inner_join(df2, by = c('country', 'date', 'lat', 'long')) %>%
    inner_join(df3, by = c('country', 'date', 'lat', 'long'))
  df_res <- df_res %>% select(-c('type.x', 'type.y', 'type'))
  names(df_res) <- c('country', 'lat', 'long', 'date', 'confirmed', 'deaths', 'recovered')
  df_res
}

# transform to tidy format and unite into on dataframe
df_confirmed_global <- gather(df_confirmed_global, key = 'Date', value = 'Cases', -c('Province/State', 'Country/Region', 'Lat', 'Long'))
df_confirmed_global <- make_df_tidy(df_confirmed_global, 'confirmed')
df_confirmed_global %>% write_csv('data/time_series_covid19_confirmed_global.csv')
df_deaths_global <- gather(df_deaths_global, key = 'Date', value = 'Cases', -c('Province/State', 'Country/Region', 'Lat', 'Long'))
df_deaths_global <- make_df_tidy(df_deaths_global, 'deaths')
df_deaths_global %>% write_csv('data/time_series_covid19_deaths_global.csv')
df_recovered_global <- gather(df_recovered_global, key = 'Date', value = 'Cases', -c('Province/State', 'Country/Region', 'Lat', 'Long'))
df_recovered_global <- make_df_tidy(df_recovered_global, 'recovered')
df_recovered_global %>% write_csv('data/time_series_covid19_recovered_global.csv')

# Combine by rows for processing in R (cases in one column, type of case - in another. This is tidy principle: one row for one observation. It allows also show legend when using ggplot)
df <- rbind(df_confirmed_global, df_deaths_global, df_recovered_global)

# df$type to factors
df$type <- as.factor(df$type)
head(df)

df %>% write_csv('data/time_series_covid19_global.csv')

# df for Tableau (all types of cases in separate column)
df_tableau <- make_df_tableau(df1 = df_confirmed_global,
                              df2 = df_deaths_global,
                              df3 = df_recovered_global)
head(df_tableau)
df_tableau %>% write_csv('data/time_series_covid19_global_tableau.csv')
c(nrow(df_confirmed_global), nrow(df_deaths_global), nrow(df_recovered_global), nrow(df_tableau))



df %>% group_by(type) %>% summarise(total_cases = sum(cases))
table(df$type)
sum(is.na(df))
dim(df)
# US, recovered
df %>% filter(country == 'US' & type == 'recovered') %>% summarise(recovered = sum(cases), N = n())
# all countries
df %>% group_by(country, type) %>%
  summarise(number_cases = sum(cases), N = n()) 
# US, Brazil, Russia
df %>% group_by(country, type) %>%
  summarise(number_cases = sum(cases), N = n()) %>%
  filter(country %in% c('US', 'Brazil', 'Russia'))

df %>% summarise(total_cases = sum(cases))

# Total cases (Confirmed, Deaths, Recovered) in Italy
library(scales) # to access breaks (date like) functions
ggplot(data = filter(df, country == 'Italy')) +
  geom_point(aes(x = date, y = cases, color = type)) +
  scale_x_date(date_breaks = 'months', date_labels = '%Y-%b')
  #scale_x_continuous(breaks = 1:length(unique(format(df$date, '%b')))
  #                   labels = fct_relevel(as.factor(unique(format(df$date, '%b'))), month.abb))   # doesn't work: fct_relevel(as.factor(unique(format(df$date, '%b'))), month.abb)
  #                                # doesn't work: c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep')











