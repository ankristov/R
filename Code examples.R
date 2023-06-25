###########################################
### How To Connect R With SQL
###########################################
# https://predictivehacks.com/how-to-connect-r-with-sql/
# 1 Connection
library(DBI)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = 'airlines',
                 host = '127.0.0.1',
                 port = 8889,
                 user = 'root',
                 password = 'root')
# Remove the Credentials from your Code
con <- dbConnect(RMySQL::MySQL(),
                 dbname = 'airlines',
                 host = '127.0.0.1',
                 port = 8889,
                 user = rstudioapi::askForPassword('Database user'),
                 password = rstudioapi::askForPassword('Database password'))

tables <- dbListTables(con)
tables
str(tables)

# 2 Import tables from server to R
flights <- dbReadTable(con, 'flights')
head(flights)
# import all tables
tables <- dbListTables(con)
tables <- lapply(tables, dbReadTable, conn = con)
head(tables[[1]])
head(tables[[2]])
head(tables[[3]])

# 3 Import data from queries
data <- dbGetQuery(con, "select origin, count(*) from flights where origin in ('EWR', 'JFK') group by origin;")
data
data <- dbGetQuery(con, "select * from flights;")
head(data)

query <- dbSendQuery(con, 'select * from flights;')
dbFetch(query, n = 1) # get the first two rows
dbFetch(query, n = -1) # get all rows
dbClearResult(data)

###########################################
### Axis breaks like date components
###########################################

library(scales) # to access breaks (date like) functions
ggplot(data = filter(df, country == 'Italy')) +
  geom_point(aes(x = date, y = cases, color = type)) +
  scale_x_date(date_breaks = 'months', date_labels = '%b')


###########################################
### Transform months to abbreviations
###########################################


sea_ice %>%
  mutate(Month = fct_relevel(as.factor(format(date, "%b")), month.abb)) %>%
  
###########################################
### Add legend to plot
###########################################
# scale_color_identity()
ggplot(luv_colours, aes(u, v)) +
  geom_point(aes(colour = col), size = 3) +
  scale_color_identity() +
  coord_equal()

df <- data.frame(
  x = 1:4,
  y = 1:4,
  colour = c("red", "green", "blue", "yellow")
)
df
ggplot(df, aes(x, y)) + geom_tile(aes(fill = colour))
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = colour)) +
  scale_fill_identity() # to use colors specified in our dataset and in aes()

# To get a legend guide, specify guide = "legend"
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = colour)) +
  scale_fill_identity(guide = "legend")
# But you'll typically also need to supply breaks and labels:
ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = colour)) +
  scale_fill_identity("trt", labels = letters[1:4], breaks = df$colour,
                      guide = "legend")

# cyl scaled to appropriate size
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(size = cyl))
# cyl used as point size
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(size = cyl)) +
  scale_size_identity(guide='legend')

#####################################
# Change continuous colors on plot
#####################################
library(nycflights13)
head(flights)
head(airports)
flights %>% group_by(dest) %>% 
  summarise(avg_delay = mean(dep_delay, na.rm = T)) %>%
  left_join(airports, by = c('dest' = 'faa')) %>%
  ggplot(aes(lon, lat)) +
  borders('state') + 
  geom_point(aes(color = avg_delay + 10)) +
  scale_color_gradient(low = "green", high = "red", guide = 'colorbar') +
  coord_quickmap()
#####################################
# Color gradient
#####################################
df <- data.frame(
  x = runif(100),
  y = runif(100),
  z1 = rnorm(100),
  z2 = abs(rnorm(100))
)
df
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z2))
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z1)) +
  scale_colour_gradientn(colours = terrain.colors(10))
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradientn(colours = terrain.colors(10))
ggplot(df, aes(x, y)) +
  geom_point(aes(colour = z2)) +
  scale_colour_gradient(low = "white", high = "black")
##################################################
# Using Brewer color palette
##################################################
col_pal <- "Set1"

tmp %>% ggplot(aes(x=latitude, fill = type, color = type)) +
  geom_line(stat = "density", size = 1.2) + theme_bw() +
  scale_fill_brewer(palette = col_pal) +
  scale_color_brewer(palette = col_pal)
##################################################
# Compare execution time for different functions
##################################################
install.packages("microbenchmark")
library(microbenchmark)
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)
##################################################
# Matrix correlation 
##################################################

data("SAheart")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/thefactmachine/logisticRegression/master/SAHeart.csv")
SAheart <- read.csv(text = x,  header = TRUE, sep = ",")
head(SAheart)
summary(SAheart)
str(SAheart)
pairs(subset(SAheart,select = -chd), col = as.factor(SAheart$chd))
# correlation between variables
SAheart1 <- SAheart %>% mutate(famhist = as.numeric(famhist))
M <- abs(cor(SAheart1))
diag(M) <- 0
which(M > 0.6, arr.ind = T)
##################################################
# cut() for visualizing continuous variables
##################################################
# cut() function for continuous variables
wageCut <- cut(Wage$wage, breaks = c(0, 50.000, 100.000,150.000, 320.000))
table(wageCut)
library(dplyr)
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  count(education, wageBin) %>%
  ggplot() + geom_tile(aes(x = education, y = wageBin, fill = n))
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  ggplot() + geom_boxplot(aes(x = wageBin, y = age, fill = wageBin))
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  ggplot() + geom_boxplot(aes(x = reorder(wageBin, age, median), y = age, fill = wageBin))
Wage %>% mutate(wageBin = cut(Wage$wage, breaks = 20.000 * (0:16))) %>%
  ggplot(aes(x = reorder(wageBin, age, median), y = age)) + 
  geom_boxplot(aes(fill = wageBin)) +
  geom_jitter(color = "black", alpha = 0.1) + 
  coord_flip()

install.packages("Hmisc")
library(Hmisc)
wageCut <- cut2(Wage$wage,g = 3) # cut2() includes the most lef and right intervals, cut() no, and more flexible
table(wageCut)
Wage %>% mutate(wageBin = cut2(wage,g = 3)) %>%
  ggplot() + geom_boxplot(aes(x = wageBin, y = age, fill = wageBin)) +
  geom_jitter(aes(x = wageBin, y = age),alpha = 0.2)
t1 <- table(wageCut, Wage$jobclass)
t1
prop.table(t1,margin = 1)
Wage %>% mutate(wageBin = cut2(wage,g = 3)) %>%
  count(wageBin, jobclass) %>%
  ggplot() + geom_tile(aes(y = wageBin, x = jobclass, fill = n))
# density plot
ggplot(data = Wage) +
  geom_density(aes(x = wage, color = education))
ggplot(data = Wage) +
  geom_density(aes(x = wage, y = ..density.., color = education))
##################################################
# Manipulate AND stat_function()
##################################################
library(RColorBrewer)
cols <- brewer.pal(8, "Set1")
library(manipulate)
mu0 <- 30
myplot <- function(sigma,mua,n,alpha) {
  g <- ggplot(data.frame(mu = c(27,36)), aes(x = mu))
  g <- g + stat_function(fun = dnorm, geom = "line",
                         args = list(mean = mu0, sd = sigma/sqrt(n)),
                         size = 1, color = cols[1])
  g <- g + stat_function(fun = dnorm, geom = "line",
                         args = list(mean = mua, sd = sigma/sqrt(n)),
                         size = 1, color = cols[2])
  xitc <- mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
  g <- g + geom_vline(xintercept = xitc, size = 1, color = cols[3])
  g
}
manipulate(
  myplot(sigma, mua, n, alpha),
  sigma = slider(1, 10, step = 1, initial = 4),
  mua = slider(30,35, step = 1, initial = 32),
  n = slider(1,50,step = 1, initial = 16),
  alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

##################################################
# Reverse number (EPAM interview task)
##################################################
rev_num <- function(num){
  temp <- num %/% 10
  new <- num %% 10
  while (temp) {
    new <- new * 10 + temp %% 10
    #print(temp %% 10)
    temp <- temp %/% 10
  }
  new
}
rev_num(12345) 

##################################################
# Bootstrap, ggplot for multiple lines
##################################################
library(ElemStatLearn); data(ozone)
head(ozone)
# order just for illustration
ozone <- ozone[order(ozone$ozone),]
# fit loess model on 10 bagged samples
m <- matrix(data = NA, nrow = dim(ozone)[1], ncol = 10)
for (i in 1:10){
  indexes <- sample(1:dim(ozone)[1], replace = T)
  ozone_sample <- ozone[indexes,]
  ozone_sample <- ozone_sample[order(ozone_sample$ozone),]
  fit <- loess(temperature ~ ozone, data = ozone_sample, span = 0.2, na.action = 'na.omit')
  m[,i] <- predict(fit, newdata = data.frame(ozone = 1:dim(ozone)[1]))
}
m
# visualize 
# with ggplot it's little bit tricky..
require(reshape2)
df <- data.frame(m)
df['index'] <- 1:dim(df)[1]
df
df_melted <- melt(df, id = 'index')
head(df_melted)
sum(is.na(m))
ggplot(data = df_melted) + geom_point(data = ozone, aes(ozone, temperature)) +
  geom_line(aes(x = index, y = value, group = variable), color = 'grey') +
  geom_line(data = df, aes(x = 1:dim(df)[1], y = apply(df[, 1:10], 1, mean), color = 'red'))

##################################################
# ggplot2 + ggtext
remotes::install_github('wilkelab/ggtext')

library(ggplot2)
library(ggtext)
data("iris")
ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
  geom_point() +
  facet_wrap(~ Species) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(
      size = 12
    )
  )
##################################################
# Quick plot of hists of all vatiables
# https://drsimonj.svbtle.com/quick-plot-of-all-variables
market_data_fixed[,sapply(market_data_fixed, is.numeric)] %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()
# or
market_data_fixed[,sapply(market_data_fixed, is.numeric)] %>%
  pivot_longer(cols = everything(), names_to="key", values_to="value") %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()
##################################################
# 
##################################################

##################################################
# 
##################################################

##################################################
# 
##################################################

##################################################
# 
##################################################







