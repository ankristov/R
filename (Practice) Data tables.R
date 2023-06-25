# Data tables
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

library(dplyr)
library(nycflights13)
library(ggplot2)
head(flights)
flights_dt <- data.table(flights) # data.table version
# – Get all the flights with “JFK” as the origin airport in the month of June
flights %>% filter(origin == 'JFK' & month == 6) # dplyr
flights[origin == 'JFK' & month == 6] # doesn't work
flights_dt[origin == 'JFK' & month == 6] # data.table
# – Get the first two rows from flights
flights[1:2,] # dplyr
flights_dt[1:2] # data.table
# – Sort flights first by column origin in ascending order, and then by dest in descending order
flights %>% arrange(origin, desc(dest)) %>% select(origin, dest)
flights_dt[order(origin, -dest)]
# – Select arr_delay column, but return it as a vector
flights %>% select(arr_delay) # as data.frame
flights_dt[, arr_delay] # as vector
ans <- flights_dt[, list(arr_delay)] # as data.table
ans <- flights_dt[, .(arr_delay)] # the same
head(ans)
# – Select both arr_delay and dep_delay columns
flights %>% select(arr_delay, dep_delay)
ans <- flights_dt[, .(arr_delay, dep_delay)]
ans <- flights_dt[, list(arr_delay, dep_delay)]
head(ans)
# – Select both arr_delay and dep_delay columns and rename them to delay_arr and delay_dep.
ans <- flights_dt[, .(delay_arr = arr_delay, delay_dep = dep_delay)]
head(ans)
# – How many trips have had total delay < 0?
flights %>% mutate(total_delay = arr_delay + dep_delay) %>% 
  filter(total_delay < 0) %>% count()
flights_dt_small <- flights_dt[1:5,]; flights_dt_small
flights_dt_small[, sum((arr_delay + dep_delay) < 0)]
# – Calculate the average arrival and departure delay for all flights with “JFK” as the origin airport in the month of June.
flights %>% filter(origin == 'JFK' & month == 6) %>% 
  filter(!is.na(arr_delay) & !is.na(dep_delay)) %>%
  summarize(mean_arr_delay = mean(arr_delay), mean_dep_delay = mean(dep_delay))
flights_dt[origin == 'JFK' & month == 6L,
           .(mean_arr_delay = mean(arr_delay), mean_dep_delay = mean(dep_delay))] # NA
flights_dt[origin == 'JFK' & month == 6L & !is.na(arr_delay) & !is.na(dep_delay),
           .(mean_arr_delay = mean(arr_delay), mean_dep_delay = mean(dep_delay))]
# – How many trips have been made in 2014 from “JFK” airport in the month of June?
flights %>% filter(month == 6 & origin == 'JFK') %>% count()
flights_dt[origin == 'JFK' & month == 6L, length(dest)]
flights_dt[origin == 'JFK' & month == 6L, .N]
flights_dt[origin == 'JFK' & month == 6L, .(.N)]
# – Select both arr_delay and dep_delay columns the data.frame way.
flights %>% select(arr_delay, dep_delay)
flights_dt[, c("arr_delay", "dep_delay")]
cols <- c("arr_delay", "dep_delay")
flights_dt[, ..cols]
flights_dt[, cols, with = F]
flights_dt[, !c("arr_delay", "dep_delay")]
flights_dt[, -c("arr_delay", "dep_delay")]
# – How can we get the number of trips corresponding to each origin airport?
flights %>% group_by(origin) %>% count()
flights_dt[, .(.N), by = .(origin)]
# – How can we get the total number of trips for each origin, dest pair for carrier code "AA"?
names(flights)
table(flights$carrier)
flights %>% filter(carrier == 'AA') %>% 
  group_by(origin, dest) %>% count() %>% arrange(n)
flights_dt[carrier == 'AA', .N, by = .(origin, dest)]
# – How can we get the average arrival and departure delay for each orig,dest pair for each month for carrier code "AA"?
ans <- flights %>% filter(carrier == 'AA') %>% 
  filter(!is.na(arr_delay) & !is.na(dep_delay)) %>% 
  group_by(origin, dest, month) %>% 
  summarise(arr_delay_mean = mean(arr_delay), dep_delay_mean = mean(dep_delay))
ans
ans %>% filter(origin == 'EWR' & dest == 'DFW') %>%
  ggplot(aes(x = month, y = arr_delay_mean)) + geom_point(size = 3, color = 'blue') +
  geom_line(color = 'blue')
ans <- flights_dt[carrier == 'AA', .(mean(arr_delay), mean(dep_delay)),
                  by = .(origin, dest, month)]
ans
# – So how can we directly order by all the grouping variables?
ans <- flights %>% filter(carrier == 'AA') %>% 
  filter(!is.na(arr_delay) & !is.na(dep_delay)) %>% 
  group_by(origin, dest, month) %>% 
  summarise(arr_delay_mean = mean(arr_delay), dep_delay_mean = mean(dep_delay)) %>%
  arrange(origin, dest, month)
ans
ans <- flights_dt[carrier == 'AA', .(mean(arr_delay), mean(dep_delay)),
                  keyby = .(origin, dest, month)]
ans
