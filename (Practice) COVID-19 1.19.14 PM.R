library(tidyr)


download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',
              method = 'curl', destfile = 'data/time_series_covid19_confirmed_global.csv')
download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv',
              method = 'curl', destfile = 'data/time_series_covid19_deaths_global.csv')
download.file('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv',
              method = 'curl', destfile = 'data/time_series_covid19_recovered_global.csv')

df_confirmed_global <- read_csv('data/time_series_covid19_confirmed_global.csv')
df_deaths_global <- read_csv('data/time_series_covid19_deaths_global.csv')
df_recovered_global <- read_csv('data/time_series_covid19_deaths_global.csv')

# transform to tidy format and unite into on dataframe
df_confirmed_global <- gather(df_confirmed_global, key = 'Date', value = 'Cases', -c('Province/State', 'Country/Region', 'Lat', 'Long'))
df_confirmed_global$type <- 'confirmed'
df_deaths_global <- gather(df_deaths_global, key = 'Date', value = 'Cases', -c('Province/State', 'Country/Region', 'Lat', 'Long'))
df_deaths_global$type <- 'deaths'
df_recovered_global <- gather(df_recovered_global, key = 'Date', value = 'Cases', -c('Province/State', 'Country/Region', 'Lat', 'Long'))
df_recovered_global$type <- 'recovered'
# union
df <- rbind(df_confirmed_global, df_deaths_global, df_recovered_global)
df %>% write_csv('time_series_covid19_global.csv')


# transform column names to lowercase (tidy principle)
names(df) <-  tolower(names(df))
df <- df %>% rename('country' = 'country/region')
df <- df %>% select(-c('province/state'))
head(df)

# transform Date from string to date
library(lubridate)
df$date <- mdy(df$date)
head(df)

table(df$type)

library(scales) # to access breaks (date like) functions
ggplot(data = filter(df, country == 'Italy')) +
  geom_point(aes(x = date, y = cases, color = type)) +
  scale_x_date(date_breaks = 'months', date_labels = '%Y-%b')
  #scale_x_continuous(breaks = 1:length(unique(format(df$date, '%b')))
  #                   labels = fct_relevel(as.factor(unique(format(df$date, '%b'))), month.abb))   # doesn't work: fct_relevel(as.factor(unique(format(df$date, '%b'))), month.abb)
  #                                # doesn't work: c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep')











