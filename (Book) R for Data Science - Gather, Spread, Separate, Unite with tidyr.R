library(tidyr)

table1
table2
table3
table4a
table4b
table5

# Gathering
table4a
gather(table4a, key = "year", value = "cases", '1999','2000')
gather(table4a, '1999','2000', key = "year", value = "cases")
tidy4a <-  table4a %>% gather(c(`1999`, `2000`), key = "year", value = 'cases')
tidy4b <- table4b %>% gather(c(`1999`, `2000`), key = "year", value = 'population')
left_join(tidy4a, tidy4b)

head(iris)
mini_iris <- iris[c(1,51,101),]
mini_iris
gather(mini_iris, key = "flower_att", value = "measurements",Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
df <- gather(mini_iris, key = "flower_att", value = "measurements",-Species)
df

# Spreading
table2
spread(table2, key = type, value = count)
spread(df, key = flower_att, value = measurements)
mini_iris

# Why are gather() and spread() not perfectly symmetrical?
library(tibble)
stocks <- tibble(
  year =c(2015,2015,2016,2016), 
  half=c(1, 2, 1, 2), 
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stocks %>% spread(key = year, value = return) 
stocks %>% 
  spread(key = year, value = return) %>%
  gather(key = "year", value = "return", `2015`:`2016`) # do not see problem...

# Why does spreading this tibble fail? How could you add a new column to fix the problem?
people <- tribble(
  ~name, ~key, ~value, 
  #-----------------|--------|------
  "Phillip Woods",    "age",       45,
  "Phillip Woods",    "height",   186,
  #"Phillip Woods",    "age",       50,
  "Jessica Cordero",  "age",       37,
  "Jessica Cordero",  "height",   156
)
people
people %>% spread(key = key, value = value)

# Tidy this simple tibble. Do you need to spread or gather it? What are the variables?
preg <- tribble(
  ~pregnant, ~male, ~female, 
  "yes",     NA,    10, 
  "no",      20,    12
)
preg
preg %>% gather(c(male,female), key = sex, value = N)
preg %>% gather(c(male,female), key = sex, value = N)

# Separate
table3
separate(table3, col = rate, into = c("cases", "population"), sep = '/')
separate(table3, col = rate, into = c("cases", "population"), sep = '/', convert = T)
separate(table3, col = year, into = c("century", "year"), sep = 2)
# What do the extra and fill arguments do in separate()? Experiment with the 
# various options for the following two toy datasets
# extra argument
t <- tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) 
t %>% separate(col = x, into = c('one', 'two', 'three'), extra = 'warn')
t %>% separate(x, c('one', 'two', 'three'), extra = 'drop')
t %>% separate(x, c('one', 'two', 'three'), extra = 'merge')
# fill argument
t <- tibble(x = c("a,b,c", "d,e", "f,g,i"))
t %>% separate(x, c('one', 'two', 'three'), fill = 'warn')
t %>% separate(x, c('one', 'two', 'three'), fill = 'right')
t %>% separate(x, c('one', 'two', 'three'), fill = 'left')
# remove argument
t %>% separate(x, c('one', 'two', 'three'), fill = 'right', remove = F)

# Unite
table5
unite(data = table5, col = new, century, year)
unite(data = table5, col = new, century, year, sep = "")

# Missing values
stocks <- tibble(
  year =c(2015,2015,2015,2015,2016,2016,2016), 
  qtr =c( 1, 2, 3, 4, 2, 3, 4), 
  return=c(1.88,0.59,0.35, NA,0.92,0.17,2.66)
)
stocks
# There are two missing values in this dataset:
# - return for the fourth quarter of 2015 (explicit)
# - return for the first quarter of 2016 (implicit)
# The way that a dataset is represented can make implicit values explicit
# For example, we can make the implicit missing value explicit by putting years in the column
spread(data = stocks, key = year, value = return)
# how to remove observations with missing values (if they are not important)
stocks %>% 
  spread(key = year, value = return) %>%
  gather(key = year, value = return, '2015':'2016', na.rm = T)
# another important tool for making implicit misiing values explicit
complete(data = stocks, year, qtr)
# Sometimes when a data source has primarily been used for data entry, 
# missing values indicate that the previous value should be carried forward
treatment <- tribble(
  ~ person, ~ treatment, ~response, 
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9, 
  "Katherine Burke", 1, 4
)
treatment
treatment %>% 
  fill(person)
fill(data = treatment, person)

# Case Study
head(who)
?who
dim(who)
# The best place to start is almost always to gather together the columns 
# that are not variables
# There are a lot of missing values in the current representation, so for now 
# we’ll use na.rm just so we can focus on the values that are present
library(dplyr)
library(tidyr)
who1 <- who %>% gather(new_sp_m014:newrel_f65, 
                       key = 'key', 
                       value = 'cases',
                       na.rm = T)
who1 <- who %>% gather(starts_with('new'), 
                       key = 'key', 
                       value = 'cases',
                       na.rm = T)
head(who1)
dim(select(who, starts_with('new')))
dim(select(who, new_sp_m014:newrel_f65))
# We can get some hint of the structure of the values in the new key column 
# by counting them
who1 %>% count(key)
who1 %>% group_by(key) %>% summarise(n = n())
# colnames are slightly inconsistent because instead of new_rel we have newrel
table(who1$key)
who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, 'newrel', 'new_rel'))
head(who2)
table(who2$key)
stringr::str_replace(string = 'Andrew Skobliakov', 
                     pattern = 'Skobliakov',
                     replacement = 'Kristov')
# We can separate the values in each code with two passes of separate(). The 
# first pass will split the codes at each underscore
who3 <- who2 %>%
  separate(col = key, into = c('new','type','sexage'), sep = '_')
head(who3)
# Then we might as well drop the new column because it’s constant in this dataset. 
# While we’re dropping columns, let’s also drop iso2 and iso3 since they’re redundant
who3 %>% count(new)
dim(who3)
who4 <- who3 %>% select(-new, -iso2, -iso3)
head(who4)
# Next we’ll separate sexage into sex and age by splitting after the first character
who5 <- who4 %>% separate(col = sexage, into = c('sex','age'), sep = 1)
who5
# Typically isn’t how you’d work interactively. Instead, you’d gradually 
# build up a complex pipe
who %>%
  gather(key = code, value = value, new_sp_m014:newrel_f65, na.rm = T) %>%
  mutate(code = stringr::str_replace(code, 'newrel', 'new_rel')) %>%
  separate(col = code, into = c('new', 'var', 'sexage')) %>%
  select(-new, -iso2, iso3) %>%
  separate(col = sexage, c('sex', 'age'), sep = 1)


who %>% gather(key = 'code', value = 'value', starts_with('new'), na.rm = T) %>%
  mutate(code = stringr::str_replace(code, pattern = "newrel", replacement = "new_rel")) %>%
  separate(col = code, into = c('new', 'type', 'sexage'), sep = "_")  %>%
  separate(col = sexage, into = c('sex', 'age'), sep = 1) %>%
  select(-iso2, -iso3, -new)



















