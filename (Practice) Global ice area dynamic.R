
# https://lucidmanager.org/data-science/storytelling-with-data/
library(dplyr)
library(forcats)
library(tidyr)
library(readr)
library(ggplot2)

download.file("https://sites.google.com/site/arctischepinguin/home/sea-ice-extent-area/data/nsidc_global_nt_final_and_nrt.txt.gz", 
              destfile = "data/global-sea-ice.gz")
system('gunzip data/global-sea-ice.gz')

sea_ice <- read_csv('data/global-sea-ice', skip = 21)
head(sea_ice)

# Original plot
ggplot(sea_ice, aes(yearday, area, col = format(date, '%Y'))) +
  geom_line() +
  #scale_color_discrete(name = '') +
  labs(title = 'Global Sea Ice Area',
       subtitle = 'from NSIDC NASA Team sea ice concentration data',
       y = expression(Sea~Ice~Area~10^6~km^2))

# Facetted version
# Better representation for visualizing ice area dynamic 
sea_ice %>%
  mutate(Month = fct_relevel(as.factor(format(date, "%b")), month.abb)) %>%
  select(Month)
library(lubridate)
sea_ice %>%
  mutate(Month = as.factor(month(date, abbr = T, label = T))) %>%
  ggplot(aes(date, area)) + 
  geom_line(col = 'grey') +
  geom_smooth(method = 'lm') +
  facet_wrap(~Month) +
  labs(title = "Global Sea Ice Area",
       subtitle = "from NSIDC NASA Team sea ice concentration data",
       y = expression(Sea~Ice~Area~10^6~km^2)) +
  theme_light()
  
  













