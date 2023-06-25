#######################################################################
# Working with Statistics Canada Data in R, Part 1: What is CANSIM?
# https://dataenthusiast.ca/?p=127

install.packages('cansim')
library(cansim)
library(tidyverse)

# So why do we have to use tidyverse in addition to cansim (apart from the fact that tidyverse is probably the best data processing software in existence)? Well, data in Statistics Canada CANSIM repository has certain bugs features that one needs to be aware of, and which can best be fixed with the instruments included in tidyverse:
# First, it is not always easy to find and retrieve the data manually. After all, you’ll have to search through thousands of data tables looking for the data you need. Online search tool often produces results many pages long, which are not well sorted by relevance (or at least that is my impression).
# Second, StatCan data is not in the tidy format, and usually needs to be transformed as such, which is important for convenience and preventing errors, as well as for plotting data.
# Third, don’t expect the main principles of organizing data into datasets to be observed. For example, multiple variables can be presented as values in the same column of a dataset, with corresponding values stored in the other column, instead of each variable assigned to its own column with values stored in that column (as it should be). If this sounds confusing, the code snippet below will give you a clear example.
# Next, the datasets contain multiple irrelevant variables (columns), that result form how Statistics Canada processes and structures data, and do not have much substantive information.
# Finally, CANSIM datasets contain a lot of text, i.e. strings, which often are unnecessarily long and cumbersome, and are sometimes prone to typos. Moreover, numeric variables are often incorrectly stored as class “character”.
# If you’d like an example of a dataset exhibiting most of these issues, let’s looks at the responses from unemployed Aboriginal Canadians about why they experience difficulties in finding a job. To reduce the size of the dataset, let’s limit it to one province. Run line-by-line:

jobdif_0014 <- get_cansim('41-10-0014') %>% filter(GEO == 'Saskatchewan')
head(jobdif_0014)

# Examine the dataset - lots of redundant variables.
View(jobdif_0014)

# All variables except VALUE are of class "character",
map(jobdif_0014, class)
lapply(jobdif_0014, class)
str(jobdif_0014)

# although some contain numbers, not text
map(jobdif_0014, head)
lapply(jobdif_0014, head)

# Column "Statistics" contains variables’ names instead of values,
unique(jobdif_0014$Statistics)
table(jobdif_0014$Statistics)

# while corresponding values are in a totally different column.
head(jobdif_0014$VALUE, 10)

# Now we can start retrieving CANSIM data. How we do this, depends on whether we already know the table or vector numbers. If we do, things are simple: just use get_cansim to retrieve data tables, or get_cansim_vector to retrieve vectors.
# But usually we don’t. One option is to use StatCan’s online search tool. Eventually you will find what you’ve been looking for, although you might also miss a few things – CANSIM is not that easy to search and work with manually.
# A much better option is to let R do the tedious work for you with a few lines of code.

# Working with Statistics Canada Data in R, Part 2: Retrieving CANSIM Data
# https://dataenthusiast.ca/?p=188

# 1 Searching by Index

# Let’s look for CANSIM tables that refer to Aboriginal (Indigenous) Canadians anywhere in the tables’ titles, descriptions, keywords, notes, etc.
# create an index to subset list_cansim_tables() output
dim(list_cansim_tables())
index <- list_cansim_tables() %>% 
  # Map(grepl, '(?i)aboriginal|(?i)indigenous', .) %>%  # list of length number_columns in previous table, each list length - num rows in previous table
  Map(grepl, 'aboriginal|indigenous', ignore.case = TRUE, .) %>%  # the same
  Reduce('|', .) %>%  # logical or with all values (all list from Map)
  which() # gives indexes where True
View(index)
length(index)
class(index) # Map returns list
index[20]

# list all tables with Aboriginal data, drop redundant cols
tables <- list_cansim_tables()[index,] %>%
  select(c("title", "keywords", "notes", "subject",
           "date_published", "time_period_coverage_start",
           "time_period_coverage_end", "url_en", 
           "cansim_table_number"
           ))
dim(tables)
# Let’s look in detail at what this code does. First, we call the list_cansim_tables function, which returns a tibble dataframe, where each row provides a description of one CANSIM table. To get a better idea of list_cansim_tables output, run
glimpse(list_cansim_tables())
str(list_cansim_tables())
# Then we search through the dataframe for Regex patterns matching our keywords. Note the (?i) flag – it tells Regex to ignore case when searching for patterns; alternatively, you can pass ignore.case = TRUE argument to grepl. The Map function allows to search for patterns in every column of the dataframe returned by list_cansim_tables. This step returns a very long list of logical values.
# Our next step is to Reduce the list to a logical vector, where each value is either FALSE if there was not a single search term match per CANSIM table description (i.e. per row of list_cansim_tables output), or TRUE if there were one or more matches. The which function gives us the numeric indices of TRUE elements in the vector.
# Finally, we subset the list_cansim_tables output by index. Since there are many redundant columns in the resulting dataframe, we select only the ones that contain potentially useful information.

# 2 Searching with dplyr::filter

# There is also a simpler approach, which immediately returns a dataframe of tables
tables <- list_cansim_tables() %>%
  filter(grepl('(?i)aboriginal|(?i)indigenous', title)) %>%
  select(c("title", "keywords", "notes", "subject",
         "date_published", "time_period_coverage_start",
         "time_period_coverage_end", "url_en", 
         "cansim_table_number"))
View(tables)
dim(tables)
# However, keep in mind that this code would search only through the column or columns of the list_cansim_tables output, which were specified inside the grepl call (in this case, in the title column). This results in fewer tables listed in tables: 60 vs 73 you get if you search by index (as of the time of writing this). Often simple filtering would be sufficient, but if you want to be extra certain you haven’t missed anything, search by index as shown above.

# 3 Saving Search Results
# Finally, it could be a good idea to externally save the dataframe with the descriptions of CANSIM data tables in order to be able to view it as a spreadsheet. Before saving, let’s re-arrange the columns in a more logical order for viewers’ convenience, and sort the dataframe by CANSIM table number.
# re-arrange columns for viewing convenience
tables <- tables[c("cansim_table_number", "title", "subject", 
                   "keywords", "notes", "date_published", 
                   "time_period_coverage_start", 
                   "time_period_coverage_end", "url_en")] %>%
  arrange(cansim_table_number)
View(tables)
# and save externally
# write_csv(tables, '/Users/Andrew/Documents/R/data/temp.txt')
write_delim(tables, '/Users/Andrew/Documents/R/data/temp.txt', delim = '|')
# ( ! ) Note that I am using write_delim function instead of a standard write.csv or tidyverse::write_csv, with | as a delimiter. I am doing this because there are many commas inside strings in CANSIM data, and saving as a comma-separated file would cause incorrect breaking down into columns in a spreadsheet software.
# Now, finding the data tables can be as simple as looking through the tables dataframe or through the selected_data_tables.txt file.

# 4 Retrieving Data Tables
# In order for the examples here to feel relevant and practical, let’s suppose we were asked to compare and visualize the weekly wages of Aboriginal and Non-Aboriginal Canadians of 25 years and older, living in a specific province (let’s say, Saskatchewan), adjusted for inflation.
# Since we have already searched for all CANSIM tables that contain data about Aboriginal Canadians, we can easily figure out that we need CANSIM table #14-10-0370. Let’s retrieve it:
wages_0370 <- get_cansim('14-10-0370')
View(wages_0370)
# Note that a small number of CANSIM tables are too large to be downloaded and processed in R as a single dataset. However, below I’ll show you a simple way how you can work with them.

# 5 Cleaning Data Tables
# CANSIM tables have a lot of redundant data, so let’s quickly examine the dataset to decide which variables can be safely dropped in order to make working with the dataset more convenient
names(wages_0370)
glimpse(wages_0370)
# ( ! ) Before we proceed further, take a look at the VECTOR variable – this is how we can find out individual vector codes for specific CANSIM data. More on that below.
# Let’s now subset the data by province, drop redundant variables, and rename the remaining in a way that makes them easier to process in the R language (follow The tidyverse Style Guide by Hadley Wickham. For instance, variable names should use only lowercase letters, numbers, and underscores instead of spaces
wages_0370 <- wages_0370 %>%
  filter(GEO == 'Saskatchewan') %>%
  select(-c(2,3,7:12,14:24)) %>%
  # setNames(c("year", "group", "type", 
  #           "age", "current_dollars")) %>%
  rename(year = REF_DATE, group = 'Aboriginal group', type = Characteristic,
         age = 'Age group', value = VALUE)

head(wages_0370)
# Next, let’s explore the dataset again. 
map(wages_0370[1:4], unique)
map(wages_0370[1:4], table)
# Now we can decide how to further subset the data.
# We obviously need the data for as many years as we can get, so we keep all the years from the year variable.
# For the group variable, we need Aboriginal and Non-Aboriginal data, but the “Aboriginal” category has two sub-categories: “First Nations” and “Métis”. It is our judgement call which to go with. Let’s say we want our data to be more granular and choose “First Nations”, “Métis”, and “Non-Aboriginal population”.
# We are only interested in the weekly wages, i.e. “Average weekly wage rate”. 0f course here median wages would be much usefull but CANSIM provides only average. Using average wages is not a commonly accepted way of analyzing wages, as it allows a small number of people with very high-paying jobs to distort the data, making wages look higher than they actually are. But well, one can only work with the data one has.
# Finally, we need only one age group: “25 years and over”.
# Having made these decisions, we can subset the data. We also drop two categorical variables (type and age) we no longer need, as both these variables would now have only one level each
wages_0370 <- wages_0370 %>% 
  filter(grepl('25 years', age) &
         grepl('First Nations|Métis|Non-Aboriginal population', group) &
         grepl('weekly wage', type)) %>%
  select(-c('type', 'age'))
#grepl('25 years', wages_0370$age)
head(wages_0370)

# All steps with a single block of code using Pipe %>%  
wages_0370 <- get_cansim('14-10-0370') %>%
  filter(GEO == 'Saskatchewan') %>%
  select(-c(2,3,7:12,14:24)) %>%
  setNames(c("year", "group", "type", 
             "age", "current_dollars")) %>%
  filter(grepl("25 years", age) &
         grepl("First Nations|Métis|Non-Aboriginal", group) &
         grepl("weekly wage", type)) %>%
  select(-c('type', 'age'))
head(wages_0370)

# 6 Retrieving Data Vectors
# How to Find the Right Vector
# Now that we have our weekly wages data, let’s adjust the wages for inflation, otherwise the data simply won’t be meaningful. In order to be able to do this, we need to get the information about the annual changes in the Consumer Price Index (CPI) in Canada, since the annual change in CPI is used as a measure of inflation. Let’s take a look at what CANSIM has on the subject
# # list tables with CPI data, exclude the US
cpi_tables <- list_cansim_tables()  %>%
  filter(grepl('Consumer Price Index', title) &
           !grepl('United States', title))
View(cpi_tables)
# First, we can use other sources to find out exactly what vectors to use. For example, we can take a look at how the Bank of Canada calculates inflation. According to the Bank of Canada’s “Inflation Calculator” web page, they use CANSIM vector v41690973 (Monthly CPI Indexes for Canada) to calculate inflation rates. So we can go ahead and retrieve this vector
# retrieve vector data
cpi_monthly <- get_cansim_vector('v41690973', '2007-01-01', 
                                 end_time = '2018-12-31')
# Since the data in the wages_0370 dataset covers the period from 2007 till 2018, we retrieve CPI data for the same period. The function takes two main arguments: vectors – the list of vector numbers (as strings), and start_time – starting date as a string in YYYY-MM-DD format. Since we don’t need data past 2018, we also add an optional end_time argument, which takes a string in the same format as start_time. Let’s take a look at the result of our get_cansim_vector call:
View(cpi_monthly)
library(ggplot2)
ggplot(cpi_monthly) + geom_point(aes(x = REF_DATE, y = VALUE))
# The resulting dataset contains monthly CPI indexes (take a look at the REF_DATE variable). However, our wages_0370 dataset only has the annual data on wages. What shall we do?
# Well, one option could be to calculate annual CPI averages ourselves
cpi_monthly <- cpi_monthly %>%
  #mutate(year = str_remove(REF_DATE, '-.{2}-.{2}')) %>%
  mutate(year = str_remove(REF_DATE, '-.*-01')) %>%
  group_by(year) %>%
  summarise(cpi = round(mean(VALUE), 2))
  #transmute(cpi = round(mean(VALUE), 2)) #%>%
  #unique()
head(cpi_monthly)
# Alternatively, we could look through CANSIM tables to find annual CPI values that have already been calculated by Statistics Canada.
# Thus, the second way to find which vectors to use, is by looking through the relevant CANSIM tables. This might be more labor-intensive, but can lead to more precise results. Also, we can do this if we can’t find vector numbers from other sources.
# Let’s look at cpi_tables. Table # 18-10-0005 has “Consumer Price Index, annual average” in its title, so this is probably what we need
# get CANSIM table with annual CPI values
cpi_annual_table <- get_cansim('18-10-0005')
View(cpi_annual_table)
# explore the data
map(cpi_annual_table[1:4], unique)
unique(cpi_annual_table$VECTOR)
# Turns out, the data is much more detailed than in the vector v41690973! Remember that in wages_0370 we selected the data for a specific province (Saskatchewan)? Well, table # 18-10-0005 has CPI breakdown by province and even by specific groups of products. This is just what we need! However, if you run unique(cpi_annual_table$VECTOR), you’ll see that table # 18-10-0005 includes data from over 2000 different vectors – it is a large dataset. So, how do we choose the right vector? By narrowing down the search
# find out vector number from CANSIM table
cpi_annual_table %>%
  rename(product = 'Products and product groups') %>%
  filter(GEO == 'Saskatchewan' & product == 'All-items') %>%
  select(VECTOR) %>%
  unique() 
# This gives us CANSIM vector number for the “all items” group CPI for the province of Saskatchewan: v41694489

# Using StatCan Data Search Tool to Find Vectors
# get and clean wheat exports data
library(lubridate)
weat_exports <- 
  get_cansim_vector("v1063958702", "2002-01-01", "2019-10-31") 
object.size(weat_exports)
weat_exports <- weat_exports %>%
  mutate(ref_date = lubridate::ymd(REF_DATE)) %>%
  rename(dollar = VALUE) %>%
  select(-c(1, 3:9))
head(weat_exports)
# check object size
object.size(weat_exports)

# Cleaning Vector Data
# Let’s now get provincial annual CPI data and clean it up a bit, removing all the redundant stuff and changing VALUE variable name to something in line with The tidyverse Style Guide
cpi_sk <- get_cansim_vector("v41694489", "2007-01-01", "2018-12-31") %>%
  mutate(year = str_remove(REF_DATE, '-01-01')) %>%
  rename(cpi = VALUE) %>%
  select(-c(1,3:9))
head(cpi_sk)
# feed the vector number directly into get_cansim_vector()
cpi_sk <- cpi_annual_table %>%
  rename(product = 'Products and product groups') %>%
  filter(GEO == 'Saskatchewan' & product == 'All-items') %>%
  select(VECTOR) %>%
  unique() %>%
  as.character() %>%
  get_cansim_vector(., "2007-01-01", "2018-12-31") %>%
  mutate(year = str_remove(REF_DATE, '-01-01')) %>%
  rename(cpi = VALUE) %>%
  select(-c(1,3:9))
head(cpi_sk)

# Joining Data (and Adjusting for Inflation)
# Now that we have both weekly wages data (wages_0370) and CPI data (cpi_sk), we can calculate the inflation rate and adjust the wages for inflation. The formula for calculating the inflation rate for the period from the base year to year X is: 
# (CPI in year X – CPI in base year) / CPI in base year
# If we wanted the inflation rate to be expressed as a percentage, we would have multiplied the result by 100, but here it is more convenient to have inflation expressed as a proportion:
# calculate the inflation rate
cpi_sk$inflate <- (cpi_sk$cpi - cpi_sk$cpi[1]) / cpi_sk[1]
head(cpi_sk)
# Now join the resulting dataset to wages_0370 with dplyr::left_join. Then use the inflation rates data to adjust wages for inflation with the following formula: base year $ = current year $ / (1 + inflation rate)
# join inflation rate data to wages_0370; adjust wages for inflation
wages_0370_1 <- wages_0370 %>%
  left_join(cpi_sk, by = 'year') %>%
  mutate(dollars_2007 = as.matrix(round(current_dollars / (1 + inflate), 2)))

wages_0370 <- wages_0370_1 %>% select(year, group, dollars_2007)
head(wages_0370)
names(wages_0370)


#######################################################################
# Working with Statistics Canada Data in R, Part 3: Visualizing CANSIM Data
# https://dataenthusiast.ca/?p=530
# So we have retrieved CANSIM data on the weekly wages of Aboriginal and Non-Aboriginal Canadians of 25 years and older, living in Saskatchewan, and the CPI data for the same province. We then used the CPI data to adjust the wages for inflation, and saved the results as wages_0370 dataset. To get started, let’s take a quick look at the dataset, what types of variables it contains, which should be considered categorical, and what unique values categorical variables have
# explore wages_0370 before plotting
View(wages_0370)
map(wages_0370, class)
map(wages_0370, unique)
glimpse(wages_0370)

# Preparing Data for Plotting
# At this point, our data is almost ready to be plotted, but we need to make one final change. Looking at the unique values, we can see that the first two variables (year and group) should be numeric (integer) and categorical respectively, while the rest are continuous (as they should be).
# ( ! ) It is a good practice to always convert categorical variables to factors.
# So, let’s do it: convert year to an integer, and group to a factor. Before doing so, let’s remove the word “population” from “Non-Aboriginal population” category, so that our plot’s legend takes less space inside the plot. We can also replace accented “é” with ordinary “e” to make typing in our IDE easier. Note that the order is important: first we edit the string values of a “character” class variable, and only then convert it to a factor. Otherwise, our factor will have missing levels.
# ( ! ) Converting a categorical variable to a factor should be the last step in cleaning your dataset.
wages_0370 <- wages_0370 %>%
  mutate(group = str_replace_all(group, c(' population' = '', 'é' = 'e'))) %>%
  mutate(group = as.factor(group)) %>%
  mutate(year = as.integer(year))
# author recomments:
#wages_03701 <- wages_0370 %>%
#  mutate(group = str_replace_all(group, c(' population' = '', 'é' = 'e'))) %>%
#  mutate_at('year', as.integer) %>%
#  mutate_if(is.character, as.factor)

head(wages_0370)

# Plotting with ggplot2
install.packages("ggrepel")
library("ggrepel")
install.packages('svglite')
library(svglite)

plot_wages_0370 <- wages_0370 %>% 
  ggplot(aes(x = year, y = dollars_2007, color = group)) +
  geom_point(size = 2.5, show.legend = T) + 
  geom_line(size = 1.2, show.legend = T) +
  geom_label_repel(aes(label = round(dollars_2007)),
             fontface = 'bold',
             label.size = .5,
             size = 3.5,
             show.legend = F) +
  coord_cartesian(ylim = c(650,1000)) +
  scale_x_continuous(breaks = 2007:2018) +
  scale_y_continuous(name = '2007 dollars',
                     breaks = seq(650, 1000, by = 50)) +
  scale_color_manual(values = c('First Nations' = 'tan3',
                                'Non-Aboriginal' = 'royalblue',
                                'Metis' = 'forestgreen')) +
  theme_bw() +
  theme(plot.title = element_text(size = 12,
                                  face = 'bold',
                                  hjust = .5,
                                  margin = margin(b = 10)),
        plot.caption = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'grey85'),
        axis.text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, face = 'bold',
                                    margin = margin(r = 10)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 11, face = 'bold')) +
  labs(title = 'Average Weekly Wages, Adjusted for Inflation,\nby Aboriginal Group, 25 Years and Older',
       caption = 'Wages data: Statistics Canada Data Table 14-10-0370\nInflation data: Statistics Canada Data Vector v41694489')
plot_wages_0370

# Save
ggsave('data/plot_wages_0370.svg', plot_wages_0370)










