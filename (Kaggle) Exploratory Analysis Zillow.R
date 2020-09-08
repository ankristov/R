# Exploratory Analysis Zillow
# https://www.kaggle.com/philippsp/exploratory-analysis-zillow

install.packages('corrplot')
install.packages('bit64')
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(bit64)

properties <- fread('/Users/Andrew/Desktop/Data Science/data/zillow-prize-1/properties_2016.csv', showProgress = T)
transactions <- fread('/Users/Andrew/Desktop/Data Science/data/zillow-prize-1/train_2016_v2.csv')
sample_submission <- fread('/Users/Andrew/Desktop/Data Science/data/zillow-prize-1/sample_submission.csv', showProgress = T)

# Rename some variables
head(properties)
View(properties)
names(properties)
str(properties)
summary(properties)
s <- summary(properties) # s - class table
dim(s)
typeof(s[7,7])
class(s)
s[7,7]
s[1,1]
s[2,1]

properties[taxamount > 3400000]
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26,
  area_pool = poolsizesum,
  area_lot = lotsizesquarefeet,
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt,
  num_story = numberofstories,
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,
  num_75_bath = threequarterbathnbr,
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,
  num_garage = garagecarcnt,
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_deliquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag,
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style = architecturalstyletypeid
)
summary(properties)
properties[,'tax_land']

head(transactions)
summary(transactions)
transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

table(properties$flag_fireplace)
table(is.na(properties$flag_fireplace))
table(properties$tax_delinquency)
table(is.na(properties$tax_delinquency))
properties <- properties %>%
  mutate(tax_delinquency = ifelse(tax_delinquency == 'Y', 1, 0), # mistake: assigning 0 to NA
         flag_fireplace = ifelse(flag_fireplace == 'Y', 1, 0),
         flag_tub = ifelse(flag_tub == 'Y', 1, 0))
table(properties$tax_delinquency)
table(properties$flag_fireplace)
table(properties$flag_tub)

# logerror
# Target variable for this competition is "logerror" field. So let us do some analysis on this field first
ggplot(transactions) + 
  geom_point(aes(x = 1:dim(transactions)[1]), y = sort(transactions$logerror)) +
  coord_cartesian(ylim = c(-5,5)) +
  scale_y_continuous(name = 'logerror', 
                   breaks = seq.int(from = -5, to=5, by=1),
                   labels = seq.int(from = -5, to=5, by=1)) +
  geom_hline(yintercept = 0, color = 'white') + # without this - error in scale_y_continuous ??
  xlab('Number of observations')

ggplot(transactions) +
  geom_histogram(aes(logerror), bins = 50) +
  xlim(-0.5,0.5)

# in analogy with python example let's replace outliers with 99% and 1% correspondigly
ulimit <- quantile(transactions$logerror, probs = 0.99); ulimit
max(transactions$logerror)
llimit <- quantile(transactions$logerror, probs = 0.01); llimit
tmp <- transactions
tmp[tmp$logerror > ulimit,]$logerror <- ulimit
tmp[tmp$logerror < llimit,]$logerror <- llimit
ggplot(tmp) + geom_histogram(aes(logerror), bins = 50) + xlim(llimit-0.1,ulimit+0.1)
max(tmp$logerror)

# Transaction dates
# There are three different time slots for transactions
tmp <- transactions %>% 
  mutate(year_month = make_date(year = year(date), month = month(date)))
head(tmp)
str(tmp)
table(tmp$year_month)
# there are only some of the transactions after 25.10 in the train set, because the rest is in the test set
tmp %>% 
  group_by(year_month) %>% count() %>%
  ggplot(aes(x = year_month, y = n)) +
    geom_bar(stat = "identity", fill = "red") +
    geom_vline(aes(xintercept = as.Date("2016-10-01")), size = 2)
tmp %>% mutate(month = month(year_month)) %>%
  group_by(month) %>%
  count() %>%
  ggplot() + geom_col(aes(x = month, y = n))

# Parcelid
dim(properties)
#table(properties$id_parcel)
table(table(properties$id_parcel))
dim(transactions)
#table(transactions$id_parcel)
names(table(transactions$id_parcel))
table(table(transactions$id_parcel)) # there some suplicates
# So most of the parcel ids are appearing only once in the dataset

# Properties
# Column types
properties %>% summarise_all(list(typeof)) %>% gather(key = 'Column', value = 'Type')
properties %>% summarise_all(list(class)) %>% gather(key = 'Column', value = 'Class')


# To get a feel for the data let’s first have a look at the distribution 
# of our outcome (logerror), i.e. the difference in log(Zestimate)-log(Saleprice)  
transactions %>%
  ggplot(aes(x = logerror)) +
  geom_histogram(bins = 400, fill = 'red') +
  theme_bw() + theme(axis.title = element_text(size = 16), 
                     axis.text = element_text(size = 14)) +
  ylab("Count") + coord_cartesian(xlim = c(-0.5, 0.5))
# absolute logerror 
transactions <- transactions %>%
  mutate(abs_logerror = abs(logerror),
         abs_error = exp(logerror))
head(transactions)  
transactions %>%
  ggplot(aes(x = abs_logerror)) +
  geom_histogram(bins = 400, fill = 'red') +
  theme_bw() + theme(axis.title =element_text(size = 16), axis.text = element_text(size=14))+
  ylab("Count") + coord_cartesian(x = c(0,.5))
max(transactions$abs_logerror)
# How does absolute log error change with time
transactions %>%
  mutate(year_month = make_date(year = year(date), month = month(date))) %>%
  group_by(year_month) %>% summarise(mean_abs_logerror = mean(abs_logerror)) %>%
  ggplot(aes(x = year_month, y = mean_abs_logerror)) +
  geom_line(size = 1.5, color = 'red') +
  geom_point(size = 5, color = 'red') + theme_bw()
# How does logerror change with time
transactions %>%
  mutate(year_month = make_date(year = year(date), month = month(date))) %>%
  group_by(year_month) %>% summarise(mean_logerror = mean(logerror)) %>%
  ggplot(aes(x = year_month, y = mean_logerror)) +
  geom_line(size = 1.5, color = 'red') +
  geom_point(size = 5, color = 'red') + theme_bw()

# Missing values
# We have seen many missing values in the data peeking. How many missing values are there for each feature? In fact, some features are missing nearly completely. So, we probably have to work more with the others
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n()))
head(missing_values)
missing_values <- gather(missing_values, key = "feature", value = "missing_pct")
head(missing_values)
missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = 'red') +
  coord_flip() + theme_bw()
good_features <- filter(missing_values, missing_pct < 0.25)
good_features

# Correlation: all numeric features
cor_tmp <- transactions %>% left_join(properties, by = 'id_parcel') %>%
  select(good_features$feature)
col_numeric <- cor_tmp %>% summarise_all(funs(typeof)) %>% 
  gather(key = 'column_name', value = 'type') %>%
  filter(type == 'integer' | type == 'double')
cor_tmp_numeric <- cor_tmp %>% select(one_of(c(col_numeric$column_name)))
head(cor_tmp_numeric)
cor(cor_tmp_numeric) # how to fill NA with mean?? changed good_features missing values to 0.25 from 0.75
cor(cor_tmp_numeric, use='complete.obs')
corrplot(cor(cor_tmp_numeric, use = 'complete.obs'), type = 'lower')
# let's check what's wrong
sum(is.na(cor_tmp_numeric)) 
na_df <- as.data.frame(apply(is.na(cor_tmp_numeric), 2, sum) )
head(na_df)
names(na_df) <- c('na_number')
sort(na_df$na_number)
na_df %>% arrange(desc(na_number)) 

# Correlation: num_features
str(good_features)
# vars <- good_features$feature
vars <- good_features$feature[str_detect(good_features$feature, 'num_')]
vars
cor_tmp <- transactions %>% left_join(properties, by = 'id_parcel')
tmp <- cor_tmp %>% select(one_of(c(vars, 'abs_logerror')))
# tmp <- cor_tmp %>% select(one_of(c(vars, 'abs_logerror'))) %>% 
#  select(-zoning_landuse_county, -zoning_property)
head(tmp)
str(tmp)
cor(tmp, use='complete.obs')
corrplot(cor(tmp, use='complete.obs'), type='lower')
tmp <- cor_tmp %>% select(one_of(c(vars, 'logerror')))
corrplot(cor(tmp, use='complete.obs'), type='lower')

# Correlation: area_features
vars <- good_features$feature[str_detect(good_features$feature, 'area_')]
vars
tmp <- cor_tmp %>% select(one_of(c(vars, "abs_logerror")))
corrplot(cor(tmp, use="complete.obs"), type = "lower")
tmp <- cor_tmp %>% select(one_of(c(vars, "logerror")))
corrplot(cor(tmp, use="complete.obs"), type = "lower")

# Correlation: tax_features
?setdiff
vars <- good_features$feature[str_detect(good_features$feature, 'tax_')]; vars
vars <- setdiff(vars, c('tax_delinquency', 'tax_year')); vars
tmp <- cor_tmp %>% select(one_of(vars, 'abs_logerror')); head(tmp)
corrplot(cor(tmp, use = 'complete.obs'), type = "lower")
tmp <- cor_tmp %>% select(one_of(vars, 'logerror'))
corrplot(cor(tmp, use = 'complete.obs'), type = "lower")
# Overall, correlations with log error are quite small. This is a hint 
# that predictions are quite good already. 
# However, we might still find some features or at least ranges of 
# feature values where predictions can still be improved

# Built year
# Let’s plot the distribution of build year for the houses. Most houses 
# were built around 1950. There are not many older houses, neither many new houses >2000
cor_tmp %>% 
  ggplot(aes(x = build_year)) + 
  geom_line(stat = 'density', color = 'red', size = 1.2) + theme_bw()
# How does the absolute logerror change with build_year?
cor_tmp %>%
  group_by(build_year) %>%
  summarise(mean_abs_logerror = mean(abs(logerror)), n()) %>%
  ggplot(aes(x = build_year, y = mean_abs_logerror)) +
  geom_smooth(color = 'grey40') +
  geom_point(color = 'red') +
  coord_cartesian(ylim = c(0,0.25)) + 
  theme_bw()
# Predictions are better for newer houses. As we saw in the figure above
# we have way fewer older houses. However, what is interesting is that 
# there are many houses with build year around 1950, but predictions for 
# those houses are not too good

# How does the logerror change with build_year?
cor_tmp %>%
  group_by(build_year) %>%
  summarise(mean_logerror = mean(logerror), n()) %>%
  ggplot(aes(x = build_year, y = mean_logerror)) +
  geom_smooth(color = 'grey40') +
  geom_point(color = 'red') +
  coord_cartesian(ylim = c(0,0.075)) + 
  theme_bw()

# Where does Zestimate predict well?
# To get a quick feel where zestimate predicts well, we can group our 
# absolute logerror into different percentiles, e.g. the percentile 
# with best predictions (top 10%), worst predictions (worst 10%) and 
# typical predictions (50% around the median)
summary(cor_tmp$logerror)
summary(cor_tmp$abs_logerror)
library(Hmisc) # for cut2
transactions <- transactions %>% 
  mutate(percentile = cut(abs_logerror, 
                          quantile(abs_logerror, probs = c(0, 0.1, 0.25, 0.75, 0.9, 1), 
                                   names = F), 
                          include.lowest = T, 
                          labels = F)
         )
head(transactions)
table(transactions$percentile)
str(transactions)
#transactions <- transactions %>% 
#  mutate(percentile = cut2(x = abs_logerror, 
#                         cuts = quantile(abs_logerror, probs = c(0, 0.1, 0.25, 0.75, 0.9, 1), 
#                                   names = T), 
#                          minmax = T, 
#                          onlycuts = F)
#  )
# head(transactions)
tmp1 <- transactions %>%
  filter(percentile == 1) %>%
  sample_n(5000) %>% 
  left_join(properties, by = 'id_parcel'); head(tmp1)
tmp2 <- transactions %>%
  filter(percentile == 5) %>%
  sample_n(5000) %>% 
  left_join(properties, by = 'id_parcel'); head(tmp2)
tmp3<- transactions %>%
  filter(percentile == 3) %>%
  sample_n(5000) %>% 
  left_join(properties, by = 'id_parcel')

tmp1 <- tmp1 %>% mutate(type = 'best_fit'); head(tmp1)
tmp2 <- tmp2 %>% mutate(type = 'worst_fit')
tmp3 <- tmp3 %>% mutate(type = 'typical_fit')
tmp <- bind_rows(tmp1, tmp2, tmp3)
tmp <- tmp %>% 
  mutate(type = factor(type, levels = c('worst_fit', 'typical_fit', 'best_fit')))
head(tmp)
str(tmp)
tmp$type

col_pal <- "Set1"

tmp %>% ggplot(aes(x=latitude, fill = type, color = type)) +
  geom_line(stat = "density", size = 1.2) + theme_bw() +
  scale_fill_brewer(palette = col_pal) +
  scale_color_brewer(palette = col_pal)
# We can see that rows resulting in the worst predictions have a lower 
# density for lower latitude values, but a higher density for intermediate 
# latitudes (around 34000000)
# We can examine this effect more closely and plot the absolute logerror as a function of latitude.
tmptrans <- transactions %>%
  left_join(properties, by = 'id_parcel')
tmptrans %>% ggplot(aes(x = latitude, y = abs_logerror)) +
  geom_smooth(color = 'red') + theme_bw()
# Having seen the example, we can look at other features quickly, to see which are associated with absolute logerror.
# abs_logerror vs longitude
tmp %>%
  ggplot(aes(x = longitude, color = type, fill = type)) +
  geom_density(alpha = 0.5)
tmp %>%
  ggplot(aes(x = longitude, color = type, fill = type)) +
  geom_line(stat = 'density')
tmptrans %>%
  ggplot(aes(x = longitude, y = abs_logerror)) +
  geom_smooth(color = 'red') + theme_bw()
# abs_logerror vs area_total_finished
tmp %>%
  ggplot(aes(x = area_total_finished, color = type, fill = type)) +
  geom_line(stat = 'density')
tmptrans %>% ggplot(aes(x = area_total_finished, y = abs_logerror)) +
  geom_smooth(color = 'red')
# abs_logerror vs area_lived_finished 
tmp %>%
  ggplot(aes(x = area_live_finished, color = type, fill = type)) +
  geom_line(stat = 'density')
tmptrans %>% ggplot(aes(x = area_live_finished, y = abs_logerror)) +
  geom_smooth(color = 'red')
# abs_logerror vs num_room 
tmp %>%
  ggplot(aes(x = num_room, color = type, fill = type)) +
  geom_line(stat = 'density')
tmptrans %>% ggplot(aes(x = num_room, y = abs_logerror)) +
  geom_smooth(color = 'red')
# abs_logerror vs num_unit 
tmp %>%
  ggplot(aes(x = num_unit, color = type, fill = type)) +
  geom_line(stat = 'density')
tmp %>%
  ggplot(aes(x = num_unit, color = type, fill = type)) +
  geom_density(alpha = 0.5, scaled = T)
tmptrans %>% ggplot(aes(x = num_unit, y = abs_logerror)) +
  geom_smooth(color = 'red')
# abs_logerror vs build_year
tmp %>%
  ggplot(aes(x = build_year, color = type, fill = type)) +
  geom_line(stat = 'density')
tmptrans %>% ggplot(aes(x = build_year, y = abs_logerror)) +
  geom_smooth(color = 'red')
# abs_logerror vs tax_total
tmp %>%
  ggplot(aes(x = tax_total, color = type, fill = type)) +
  geom_line(stat = 'density') + coord_cartesian(xlim = c(0, 1e+6))
tmp %>% ggplot(aes(x = tax_total, y = abs_logerror)) +
  geom_smooth(color = 'red') + coord_cartesian(xlim = c(0, 1e+6))
# abs_logerror vs tax_building
tmp %>%
  ggplot(aes(x = tax_building, color = type, fill = type)) +
  geom_line(stat = 'density') + coord_cartesian(xlim = c(0, 1e+6))
# Where does Zestimate over or underpredict?
# A high absolute logerror tells us predictions are not that good. But 
# this could either be driven by underpredicting or overpredicting sale price
tmptrans <- tmptrans %>% mutate(over_under = ifelse(logerror < 0, "under", "over"))
tmptrans$over_under <- factor(tmptrans$over_under, levels = c("under", "over"))
head(tmptrans)
str(tmptrans)
# Again, we see that there is no clear correlation. However, for some 
# range in the data Zestimate tends to overpredict more than to underpredict 
# (see where the red line is above the blue one)
tmptrans %>% ggplot(aes(x = latitude, y = abs_logerror)) +
  geom_smooth(aes(color = over_under))
tmptrans %>% ggplot(aes(x = longitude, y = abs_logerror)) +
  geom_smooth(aes(color = over_under))
# Both for latitude and longitude there is a range where Zestimate both under- and overpredicts.
# Where is that?
leaflet() %>%
  addTiles() %>%
  fitBounds(-118.5, 33.8, -118.25, 34.15) %>%
  addRectangles(-118.5, 33.8, -118.25, 34.15) %>%
  addMiniMap()
# For properties with small calculated total area, Zestimate seems to overpredict.
tmptrans %>% ggplot(aes(x = area_total_calc, y = abs_logerror)) +
  geom_smooth(aes(color = over_under)) 
# Whereas for actual finished area there is no such effect
tmptrans %>% ggplot(aes(x = area_total_finished, y = abs_logerror)) +
  geom_smooth(aes(color = over_under))
range(filter(tmptrans, over_under == 'over' & !is.na(area_total_finished))$area_total_finished)
range(filter(tmptrans, over_under == 'under' & !is.na(area_total_finished))$area_total_finished)
# abs_logerror vs area_lot
tmptrans %>% ggplot(aes(x = area_lot, y = abs_logerror, color = over_under)) +
  geom_smooth() + coord_cartesian(xlim = c(0, 1e+6), ylim = c(0, 0.1)) 
# abs_logerror vs num_room
tmptrans %>% ggplot(aes(x = num_room, y = abs_logerror, color = over_under)) +
  geom_smooth() 
# abs_logerror vs build_year
tmptrans %>% ggplot(aes(x = build_year, y = abs_logerror, color = over_under)) +
  geom_smooth() 
# abs_logerror vs tax_total
tmptrans %>% ggplot(aes(x = tax_total, y = abs_logerror, color = over_under)) +
  geom_smooth() + coord_cartesian(xlim = c(0, 1e+6), ylim = c(0, 0.2)) 
# Where are all those properties?
# Show 2,000 of the properties on the map
lat <- range(properties$latitude/1e06, na.rm = T)
lon <- range(properties$longitude/1e06, na.rm = T)
tmp <- properties %>%
  sample_n(2000) %>% 
  select(id_parcel, longitude, latitude) %>%
  mutate(lon = longitude/1e06, lat = latitude/1e06) %>%
  select(id_parcel, lat, lon) %>%
  left_join(transactions, by = 'id_parcel')
head(tmp)  
leaflet(tmp) %>%
  addTiles() %>%
  fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
  addCircleMarkers(stroke = F) %>%
  addMiniMap()
# Map absolute logerror
# Show the absolute logerror on map. Red = higher.
tmp <- transactions %>%
  sample_n(2000) %>%
  left_join(properties, by = 'id_parcel') %>%
  select(id_parcel, longitude, latitude, abs_logerror) %>%
  mutate(lon = longitude/1e06, lat = latitude/1e06) %>%
  select(id_parcel, lat, lon, abs_logerror)
qpal <- colorQuantile(palette = "YlOrRd", tmp$abs_logerror, n = 7)
leaflet(tmp) %>%
  addTiles() %>%
  fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
  addCircleMarkers(stroke = F, color = ~qpal(abs_logerror), 
                   fillOpacity = 1) %>%
  addLegend("bottomright", pal = qpal, values = ~abs_logerror,
            title = "Absolute logerror", opacity = 1) %>%
  addMiniMap()

# What do these features mean?
# I was really unsure about the meaning of the variables: ‘rawcensustractandblock’
# A wikipedia search revealed: Census tracts represent the smallest territorial unit for which population data are available in many countries.[3] In the United States, census tracts are subdivided into block groups and census blocks. In the U.S., census tracts are “designed to be relatively homogeneous units with respect to population characteristics, economic status, and living conditions” and “average about 4,000 inhabitants”.
str(properties$rawcensustractandblock)
head(properties$rawcensustractandblock)
# Looks like a number. However a closer look reveals its actually a number composed of two parts, separated by a dot.
as.character(properties$rawcensustractandblock[1:10])
# Ok, so far so good. So from what I read this number consisits of: FIPS Code (6037) - Tract Number (8002.04) - And block Number (1)
# Let’s specifically add the tract number and block number information
properties <- properties %>% 
  mutate(census = as.character(rawcensustractandblock), 
         tract_number = str_sub(census, 5, 11),
         tract_block = str_sub(census, 12))
head(select(properties, census, tract_number, tract_block))
# An important note:
# For round 1, the use of external data is not permitted. So save the pleasent anticipation for round 2 :-)
# FIPS codes can be looked up here (https://www.ffiec.gov/census/Default.aspx)
 #6037 Los Angeles
# 6059 Orange County
# 6111 Ventura County
# Now, that you have the tract number you can get information such as tract income level, median family income, tract population, etc.
# For our example “6037 8002.04” lets look up some information: https://www.ffiec.gov/census/report.aspx?year=2016&county=037&tract=8002.04&state=06&report=demographic
# For example: The median familiy income 2016 is $146,472.

