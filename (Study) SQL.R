library(RMySQL)
library(dplyr)
# To set up a connection to a MySQL database using dplyr, we must specify the four pa-
# rameters outlined above, and save the resulting object using the src_mysql() function
db <- src_mysql(dbname = "wordpress_promusculus", host = "127.0.0.1", port = 8889,
                user = "andrew", password = "and304rew7867")
db <- src_mysql(dbname = "wordpress_promusculus", host = "127.0.0.1", port = 8889,
                default.file = "/Applications/MAMP/Library/.my.cnf.txt",
                user = NULL, password = NULL)
db
# Next, we can retrieve data using the tbl function and the sql() command
res <- tbl(db, sql("SELECT * FROM wp_users"))
res <- tbl(db, sql("SELECT * FROM wp_posts"))
res <- tbl(db, sql("SELECT * FROM wp_usermeta"))
res
class(res)
# DB employees
db <- src_mysql(dbname = "employees", host = "127.0.0.1", port = 8889,
                default.file = "/Applications/MAMP/Library/.my.cnf.txt",
                user = NULL, password = NULL)
db
class(db)
employees <- tbl(db, sql("SELECT * FROM employees"))
departments <- tbl(db, sql("SELECT * FROM departments"))
salaries <- tbl(db, sql("SELECT * FROM salaries"))

# Using DBI
# For a closer connection to the SQL server, we use DBI
# A connection object can be created using the dbConnect() function, which works 
# similarly to the dplyr connection we created above.
library(DBI)
con <- dbConnect(drv = RMySQL::MySQL(), dbname = "airlines", 
                 host = "127.0.0.1", port = 3306,
                 user = "andrew", password = "And304rew7867")
con
class(con)
# Next, we use the dbGetQuery() function to send an SQL command to the server and 
# retrieve the results.
res <- dbGetQuery(con, "SELECT * FROM flights WHERE year = 2013 AND day = 13")
head(res)
class(res) # data frame!
res <- dbGetQuery(con, "SELECT emp_no,first_name, last_name FROM employees")
head(res)
# Unlike the tbl() function from dplyr, dbGetQuery() can execute arbitrary SQL 
# commands, not just SELECT statements. So we can also run EXPLAIN, DESCRIBE, and 
# SHOW commands
dbGetQuery(con, "EXPLAIN SELECT emp_no,first_name, last_name FROM employees")
dbGetQuery(con, "DESCRIBE employees")
dbGetQuery(con, "SHOW DATABASES")
dbListTables(con)
# Note that the db object that we created with dplyr is of class src mysql. However, 
# the con connection object we created with DBI is of class MySQL Connection. Although 
# they were created with all of the same information, they are not the same. 
# However, the db object contains an object functionally equivalent to con. 
# Namely, db$con
class(db$con)
# Thus, once you have a created a connection to your database through dplyr, you 
# can use all of the DBI functions without having to create a new connection
dbGetQuery(db$con, "SHOW TABLES")

# Database quering using SQL
install.packages("mdsr")
library(mdsr)
db <- src_scidb(dbname = "airlines")
flights <- tbl(db, "flights")
flights
carriers <- tbl(db, "carriers")
carriers
# flights and carriers objects as if they were data frames, they are not, in fact, 
# data.frames. Rather, they have class tbl mysql, and more generally, tbl. A tbl 
# is a special kind of object created by dplyr that behaves similarly to a data.frame
class(flights)
# Consider retrieving the top on-time carriers with at least 100 flights arriving at 
# JFK in September 1996
library(dplyr)
q <- flights %>%
  filter(year == 1996 & month == 9) %>%
  filter(dest == "JFK") %>%
  inner_join(carriers, by = c("carrier" = "carrier")) %>%
  group_by(name) %>%
  summarise(N = n(),
            pct_ontime = sum(arr_delay <= 15) / n()) %>%
  filter(N >= 100) %>%
  arrange(desc(pct_ontime))
head(q, 4)  
# What is actually happening is that dplyr translates our pipeline into SQL. We can 
# see the translation by passing the pipeline through the show query() function
show_query(q)
# the translate sql() function provides translation between R commands and SQL commands
translate_sql(mean(arr_delay))
stringr::str_c("this", "is", "a", "string", sep = ' ')
# This is a good thing—since it allows you to pass arbitrary SQL code through. But
# you have to know what you are doing. Since there is no SQL function called paste0(), 
# this will throw an error, even though it is a perfectly valid R expression
carriers %>%
  mutate(name_code = paste0(name, "(", carrier, ")")) # in the book here an error
# Because carriers is a tbl sql and not a data.frame, the MySQL server is actually 
# doing the computations here. The dplyr pipeline is simply translated into SQL 
# and sub- mitted to the server. To make this work, we need to replace paste0() with 
# its MySQL equivalent command, which is CONCAT.
carriers %>%
  mutate(name_code = CONCAT(name, "(", carrier, ")"))
# Another alternative is to pull the carriers data into R using the collect() function 
# first, and then use paste0() as before.2 The collect() function breaks the connection 
# to the MySQL server and returns a data.frame (which is also a tbl df)
carriers %>%
  collect() %>%
  mutate(name_code = paste0(name, "(", carrier, ")"))
class(carriers %>% collect()) # now type is data frame
# In general, all objects in your R workspace are stored in memory. Note that the 
# carriers object that we created earlier occupies very little memory (since the 
# data still lives on the SQL server), whereas collect(carriers) pulls the data 
# into R and occupies much more memory.
# You can find out how much memory an object occupies in R using the object.size() function
object.size(carriers)
print(object.size(carriers), units = "Kb")
object.size(collect(carriers))
print(object.size(collect(carriers)), units = "Kb")
# For a typical R user, this means that it can be difficult or impossible to work 
# with a data set stored as a data.frame that is larger than a few Gb. The following 
# bit of code will illustrate that a data set of random numbers with 100 columns
# and 1 million rows occupies more than three-quarters of a Gb of memory on this
# computer
n <- 100 * 1000000
x <- matrix(runif(n), ncol = 100)
dim(x)
print(object.size(x), units = "Mb")

# The SQL data manipulation language
# https://github.com/beanumber/airlines
# The etl package (on CRAN) provides the generic framework for the airlines package. 
# Since the airlines package currently lives on GitHub and not on CRAN, you have to 
# install it using devtools
install.packages("devtools")
library(devtools)
devtools::install_github("beanumber/airlines")
library(airlines)
system("mysql -e 'CREATE DATABASE IF NOT EXISTS airlines;'") # error, created in terminal
db <- dplyr::src_mysql(dbname = "airlines", host = '127.0.0.1', port = 8889,
                       user = 'andrew', password = 'and304rew7867')

# Once we have a database connection, we create an etl object, initialize the 
# database, and then populate it with data. Please note that to update the database 
# with all 30 years worth of flights may take a few hours
ontime <- etl("airlines", db = db, dir = "~/dumps/airlines")
ontime %>%
  etl_init() %>%
  etl_update(years = 1987:2016)
# airlines package streamlines construction an SQL database containing over 169 
# million flights. These data come directly from the U.S. Bureau of Transportation
# Statistics. In what follows, we access a remote SQL database that we have already 
# set up using the airlines package. Note that this database is relational, and 
# thus it consists of many tables. We can list the tables with
# SHOW TABLES;
library(DBI)
con <- dbConnect(drv = RMySQL::MySQL(), dbname = "employees", 
                 host = "127.0.0.1", port = 8889,
                 user = "andrew", password = "and304rew7867")
res <- dbGetQuery(conn = con, statement = "SHOW TABLES;")
res 
# DESCRIBE dbname;
res <- dbGetQuery(conn = con, statement = "DESCRIBE employees;")
res
# SELECT ... FROM ... JOIN ... WHERE ... GROUP BY ... HAVING ... ORDER BY ... LIMIT
dbGetQuery(conn = con, statement = "SELECT * FROM employees LIMIT 0,10;")
dbGetQuery(conn = con, statement = "SELECT emp_no,first_name,last_name FROM employees LIMIT 0,10;")
dbGetQuery(conn = con, statement = "SELECT emp_no,first_name,last_name FROM employees WHERE first_name = 'Georgi' AND last_name = 'Facello';")

# 13.3 Extended example: Building a database
# In this example, we will illustrate how to set up a MySQL database for the 
# babynames data using the command line and SQL, but not R
library(babynames)
library(dplyr)
head(babynames)
head(births)
head(dplyr::arrange(births, desc(year)))
# Since SQL tables conform to a row-and-column paradigm, our goal during the transform
# phase is to create CSV files for each of the tables. In this example we will create 
# tables for the babynames and births tables.  We will simply write these data to 
# CSV files using the write.csv() command. Since the babynames table is very long 
# (nearly 1.8 million rows), we will just use the more recent data.
babynames %>% filter(year > 1975) %>%
  write.csv(file = "babynames.csv", row.names = F)
births %>% 
  write.csv(file = "births.csv", row.names = F)
list.files(".", pattern = ".csv")
list.files(path = "/Users/Andrew/Desktop/Coursera", pattern = ".csv")
# The third step is the trickiest part—we need to define the columns precisely. 
# The use of str(), summary(), and glimpse() are particularly useful for matching 
# up R data types with MySQL data types. Please see the MySQL documentation for 
# more information about what data types are supported
glimpse(babynames)
str(babynames)
glimpse(births)
str(births)

# check how fields are separated in file (for LOAD DATA LOCAL INFILE ...)
readLines(con = '/Users/Andrew/Desktop/Coursera/babynames.csv', 2) 

# Once the MySQL database has been created, the following commands can be used 
# to access it from R using dplyr
db <- src_mysql(dbname = "babynameDB", host = "127.0.0.1", port = 8889,
                default.file = "/Applications/MAMP/Library/.my.cnf.txt",
                user = NULL, password = NULL)
babynames <- tbl(db, "babynames")
births <- tbl(db, "births")
head(babynames)
head(births)
babynames %>% filter(name == "Melissa")

# 13.6 Exercises: building DB
# The exercises about flights assume that you have access to a SQL database that 
# has been populated with the appropriate flight delay data. Please see the src 
# scidb() function in the mdsr package for access to these data on a pre-populated 
# server. To create your own database, use see the airlines package.
?mdsr::src_scidb()
library(mdsr)
library(dplyr)
airlinesDB <- src_scidb("airlines")
airlinesDB
library(airlines)

flights <- tbl(airlinesDB, "flights")
dim(flights)
head(flights)
flights_sub <- flights %>% filter(month %in% c(1,2)) 
flights_sub %>% write.csv("flights.csv", row.names = F)
glimpse(flights)
summary(flights)
range(flights$year)
flights_sub %>% na.omit() %>% count(year, month, day, dep_time, tailnum) %>% filter(n>1)

airports <- tbl(airlinesDB, "airports")
dim(airports)
head(airports)
head(airlines::airports)
airports %>% write.csv("airports.csv", row.names = F)
glimpse(airports)
range(airports$tz)
table(airports$tz)
range(airports$alt)
table(airports$dst)
max(nchar(airports$t))

carriers <- tbl(airlinesDB, "carriers")
carriers <- airlines::carriers
carriers$name <- as.character(carriers$name)
head(carriers)
dim(carriers)
carriers %>% write.csv('carriers.csv', row.names = F)
glimpse(carriers)
max(nchar(carriers$name))

planes <- tbl(airlinesDB, "planes")
planes <- airlines::planes
head(planes)
dim(planes)
planes %>% write.csv('planes.csv', row.names = F)
glimpse(planes)

# 
library(nycflights13)
??nycflights13
head(nycflights13::flights)
head(nycflights13::airports)

# 
install.packages('usdanutrients')

# From SQL exercise
# Let's breaks down the percentage of flights that were on time, had a delay of 
# 15 to 119 minutes, or were delayed longer than two hours. We can pull the data 
# for this figure with the following query. Here, in order to plot these results, 
# we need to actually bring them back into R. To do this, we will use the functionality 
# provided by the DBI package
library(DBI)
con <- dbConnect(drv = RMySQL::MySQL(), host = '127.0.0.1', dbname = 'airlines',
                 port = 3306, 
                 user = 'andrew', password = 'And304rew7867')
res <- dbGetQuery(conn = con, 
                  statement = "SELECT * FROM flights LIMIT 0,10;")
res
query <- "
SELECT
  o.carrier, c.name,
  sum(1) as numFlights,
  sum(if(arr_delay > 0 AND arr_delay <=119, 1, 0)) as numFlightsShortDelay,
  sum(if(arr_delay > 120, 1, 0)) as numFlightsLongDelay
FROM flights o
JOIN carriers c ON o.carrier = c.carrier
WHERE year=2013
GROUP BY carrier
ORDER BY numFlightsShortDelay desc;
"
res <- DBI::dbGetQuery(con, query)
typeof(res)
class(res)
head(res)
res1 <- res %>%
  mutate(name = gsub('Air(lines|ways| Lines)', '', name),
         name = gsub('(Inc\\.|Co\\.|Corporation)', '', name),
         name = gsub('\\(.*\\)', '', name),
         name = gsub(' *$', '', name))
cbind(res$name,res1$name)
res <- res1

carriers2014 <- res %>%
  mutate(groupName = ifelse(name %in% c("Envoy Air", "American Eagle"), "American", name),
         groupName = ifelse(groupName == "AirTran", "Southwest", groupName)) %>%
  group_by(groupName) %>%
  summarise(numFlights = sum(numFlights),
            wShortDelay = sum(numFlightsShortDelay),
            wLongDelay = sum(numFlightsLongDelay)) %>%
  mutate(wShortDelayPct = wShortDelay/numFlights,
         wLongDelayPct = wLongDelay/numFlights,
         delayed = wShortDelayPct + wLongDelayPct,
         ontime=1-delayed)
carriers2014

carriers_tidy <- carriers2014 %>%
  select(groupName, wShortDelayPct, wLongDelayPct, delayed) %>%
  tidyr::gather(key = "delay_type", value = "pct", -groupName, -delayed) 
carriers_tidy

delay_chart <-  
  ggplot(data = carriers_tidy, aes(x = reorder(groupName, pct, max), y = pct)) +
  geom_bar(stat='identity', aes(fill = delay_type)) +
  scale_fill_manual(name = NULL, values = c("red", "gold"),
                    labels = c("Fligths Delayed 120+ Minutes, Canceled ot Diverted",
                               "Flights Delayed 15-119 Minutes")) +
  scale_y_continuous(limits = c(0,1)) +
  coord_flip() +
  ggtitle("Southwest's Delays Are Short; United's Are Long") +
  ylab(NULL) + xlab(NULL) 
delay_chart
delay_chart +
  geom_text(data = filter(carriers_tidy, delay_type == "wShortDelayPct"),
            aes(label = paste0(round(pct*100,1), "%"), hjust="right")) +
  geom_text(data=filter(carriers_tidy, delay_type == "wLongDelayPct"),
            aes(y=delayed - pct, label = paste0(round(pct*100,1), "% ")),
            hjust = "left", nudge_y = 0.01)
delay_chart
# In another graphic in the article, FiveThirtyEight reports the slowest and 
# fastest airports among the 30 largest airports.
queryDest <- "
  SELECT dest, sum(1) as numFlights, avg(arr_delay) as avgArrivalDelay
  FROM flights o
  WHERE year = 2013
  GROUP BY dest
  ORDER BY numFlights;
"
dest <- dbGetQuery(con, queryDest)
dest
queryOrig <- "
  SELECT origin, sum(1) as numFlights, avg(dep_delay) as avgDepartDelay
  FROM flights o
  WHERE year = 2013
  GROUP BY origin
  ORDER BY numFlights;
"
origin <- DBI::dbGetQuery(con, queryOrig)
origin
dest %>% 
  left_join(origin, by = c("dest" = "origin")) %>%
  select(dest, avgDepartDelay, avgArrivalDelay) %>%
  arrange(desc(avgDepartDelay))
origin %>% filter(origin == 'LGA')
dest %>% filter(dest == 'LGA')
# We can rank airlines by average arrival delay time by carrier, after controlling 
# for the routes. First, we compute the average arrival delay time for each route. 
query <- "
  SELECT origin, dest,
    sum(1) as numFlights,
    avg(arr_delay) as avgDelay
  FROM flights o
  WHERE year = 2013
  GROUP BY origin, dest;
"
routes <- dbGetQuery(con, query)
head(routes)
# Next, we perform the same calculation, but this time, we add carrier to the GROUP BY clause
query <- "
  SELECT origin, dest,
    o.carrier, c.name as carrierName,
    sum(1) as numFlights,
    avg(arr_delay) as avgDelay
  FROM flights o
  LEFT JOIN carriers c ON o.carrier = c.carrier
  WHERE year = 2013
  GROUP BY origin, dest, o.carrier;
"
routes_carrier <- dbGetQuery(con, query)
head(routes_carrier)
# Next, we merge these two data sets, matching the routes traveled by each 
# carrier with the route averages across all carriers
routes_aug <- left_join(routes_carrier, routes,
                        by = c("origin" = "origin", "dest" = "dest"))
head(routes_aug)
# Note that routes_aug contains both the average arrival delay time for each 
# carrier on each route that it flies (avgDelay.x) as well as the average 
# arrival delay time for each route across all carriers (avgDelay.y). We can 
# then compute the difference between these times, and aggregate the weighted 
# average for each carrier.
routes_aug %>% 
  group_by(carrier) %>%
  summarise(carrier_name = gsub("\\(.*\\)", "", first(carrierName)),
            numRoutes = n(),
            numFlights = sum(numFlights.x),
            wAvgDelay = sum(numFlights.x * (avgDelay.x - avgDelay.y), na.rm = T) / sum(numFlights.x)) %>%
  arrange(wAvgDelay)


cbind(carriers$name,first(carriers$name))[1:20,]
######################
# Function: Rearrange numbers in reverse order
f <- function(N){
  orgN <- N
  newN <- 0
  while(orgN){
    n <- orgN %% 10
    newN <- newN * 10 + n
    orgN <- orgN %/% 10
  }
  newN
}
f(12345)
f(482972)
# using string representation
f1 <- function(N){
  orgNasStr <- as.character(N)
  newNasStr <- ''
  for(i in 1:length(orgNasStr)){
    newNasStr <- newNasStr + orgNasStr[length(orgNasStr)-i-1]
  }
  newNasStr
}
f1(12345)

#######################


