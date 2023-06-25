######################################
### Dates and Times with lubridate ###
######################################

library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

# Date/time from string
ymd("2017-01-31")
class(ymd("2017-01-31"))
ymd("01-2017-31") # error
myd("01-2017-31") # buono
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")

# Date/time from individual components
# Instead of a single string, sometimes you’ll have the individual components
# of the date-time spread across multiple columns
flights %>% select(year, month, day, hour, minute)
flights %>% select(year, month, day, hour, minute) %>%
  mutate(
    departure = make_datetime(year, month, day, hour, minute)
  )
# Let’s do the same thing for each of the four time columns in flights
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))
flights_dt
# With this data, I can visualize the distribution of departure times across the year:
flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 s = 1 day
# Or within a single day:
flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 min

# Date/time from other types
# You may want to switch between a date-time and a date
today()
as_datetime(today())
now()
as_date(now())
# Sometimes you’ll get date/times as numeric offsets from the “Unix Epoch,” 
# 1970-01-01. If the offset is in seconds, use as_datetime(); if it’s in days, 
# use as_date()
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

# 1. What happens if you parse a string that contains invalid dates?
# ymd(c("2010-10-10", "bananas"))
ymd(c("2010-10-10", "bananas"))
# 2. What does the tzone argument to today() do? Why is it important?
?today()
# 3. Use the appropriate lubridate function to parse each of the following dates:
d1 <- "January 1, 2010"
mdy(d1)
d2 <- "2015-Mar-07"
ymd(d2)
d3 <- "06-Jun-2017"
dmy(d3)
d4 <- c("August 19 (2015)", "July 1 (2015)") 
mdy(d4)
d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)
parse_date(x = d5, format = "%m/%d/%y")

# Date-Time Components
dt <- ymd_hms("2016-07-08 12:34:56")
dt
year(dt)
month(dt)
mday(dt)
yday(dt)
wday(dt)
month(dt, label = T)
wday(dt, label = T)
# We can use wday() to see that more flights depart during the week than on the weekend
head(flights_dt)
flights_dt %>%
  mutate(wday = wday(dep_time, label = T)) %>%
  ggplot() + geom_bar(aes(x = wday))
flights_dt %>%
  mutate(wday = wday(dep_time, label = T)) %>%
  group_by(wday) %>% summarise(N = n()) %>%
  ggplot(aes(wday, N)) + geom_point() + geom_line()
# There’s an interesting pattern if we look at the average departure delay by
# minute within the hour. It looks like flights leaving in minutes 20–30 and 
# 50–60 have much lower delays than the rest of the hour!
real_dep <- flights_dt %>%
  mutate(m = minute(dep_time)) %>%
  group_by(m) %>%
  summarize(avg = mean(arr_delay, na.rm = T),
            n = n())
ggplot(real_dep) + geom_line(aes(m, avg))
ggplot(real_dep) + geom_line(aes(m, n))
# Interestingly, if we look at the scheduled departure time we don’t see such a strong pattern:
sched_dep <- flights_dt %>%
  mutate(m = minute(sched_dep_time)) %>%
  group_by(m) %>%
  summarise(avg = mean(arr_delay, na.rm = T),
            n = n())
ggplot(sched_dep) + geom_line(aes(m, avg))
# So why do we see that pattern with the actual departure times? Well, like 
# much data collected by humans, there’s a strong bias toward flights leaving
# at “nice” departure times. Always be alert for this sort of pattern whenever 
# you work with data that involves human judgment!
ggplot(sched_dep) + geom_line(aes(m, n)) # don't agree! "beautiful" pattern on schedule dep time not real!

# Rounding date/time 
# plot the number of flights per week:
flights_dt %>%
  mutate(week_floor = floor_date(dep_time, "week"),
         week_ceiling = ceiling_date(dep_time, "week"),
         week_round = round_date(dep_time, "week")) %>%
  select(dep_time, week_floor, week_ceiling, week_round)
  
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) + geom_line()
 
# Setting components of date/time
dt <- ymd_hms("2016-07-08 12:34:56"); dt
year(dt) <- 2010; dt
month(dt) <- 01; dt
hour(dt) <- hour(dt) + 1; dt
# Alternatively, rather than modifying in place, you can create a new datetime
# with update(). This also allows you to set multiple values at once:
update(dt, year = 2020, month = 2, mday = 2, hour = 2, min = 33, sec = 49)
# If values are too big, they will roll over:
ymd("2015-02-01") %>% update(mday = 30)
ymd("2015-02-01") %>% update(hour = 400)
# You can use update() to show the distribution of flights across the course of the day for every day of the year:
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  select(dep_time, dep_hour) %>%
  ggplot(aes(dep_hour)) + geom_freqpoly(binwidth = 300)
# Setting larger components of a date to a constant is a powerful tech‐ nique that allows you to explore patterns in the smaller components.



