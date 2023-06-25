con <- file(description = '/Users/Andrew/Desktop/Coursera/data/Border_Crossing_Entry_Data.csv')
readLines(con = con, n = 10)
df <- read_csv(file = '/Users/Andrew/Desktop/Coursera/data/Border_Crossing_Entry_Data.csv')
head(df)
df$Date
df$Date <- as.POSIXct(df$Date)
View(df)
names(df)
# rename some vars
df <- df %>% rename(Port_name = `Port Name`)
df <- df %>% rename(Port_code = `Port Code`)
# separate Location into Longitude and Latitude
df %>% separate(col = Location, into = c('pre', 'coord'), sep = 6) %>%
  select(pre, coord)
# try col_date parser for Date 
df <- read_csv(file = '/Users/Andrew/Desktop/Coursera/data/Border_Crossing_Entry_Data.csv',
               col_types = cols(
                 `Port Name` = col_character(),
                 State = col_character(),
                 `Port Code` = col_double(),
                 Border = col_character(),
                 Date = col_datetime(format = "%d/%m/%Y %I:%M:%S %p"),
                 Measure = col_character(),
                 Value = col_double(),
                 Location = col_character()
               ))
head(df)
df$Date
problems(df)
problems(df)$actual
#
table(df$Year)
table(df$State)
table(df$Border)  
table(df$Measure)  
# visualization
ggplot(df) + geom_boxplot(aes(x = State, y = log(Value), fill = State))
ggplot(df) + geom_boxplot(aes(x = Border, y = log(Value), fill = Border))
ggplot(df) + geom_boxplot(aes(x = Measure, y = log(Value), fill = Measure))
ggplot(df) + geom_boxplot(aes(x = reorder(Measure, Value), y = log(Value), fill = Measure))
# plot Crossings vs Year
# separate Date to Year
format(df$Date, '%Y') # char
format(df$Date, '%m') # char
lubridate::year(df$Date) # numeric
table(format(df$Date, '%Y'))
table(format(df$Date, '%m'))
df <- df %>% mutate(Year = lubridate::year(df$Date))
str(df)
df %>% group_by(Year) %>% summarise(n_cross = sum(Value)) %>%
  ggplot() + geom_line(aes(Year, n_cross))
df %>% group_by(Year, Border) %>% summarise(n_cross = sum(Value)) %>%
  ggplot() + geom_line(aes(Year, n_cross, color = Border))
df %>% group_by(Year, Border, Measure) %>% summarise(n_cross = sum(Value)) %>%
  ggplot() + geom_line(aes(Year, n_cross, color = Measure))
# plot Crossing by car vs year on Canada and Mexico borders
df %>% group_by(Year, Border, Measure) %>% filter(Measure == 'Personal Vehicle Passengers') %>%
  summarise(n_cross = sum(Value)) %>%
  ggplot() + geom_line(aes(Year, n_cross, color = Border))
# plot Crossing by different ways vs year on Canada borders
df %>% group_by(Year, Border, Measure) %>% filter(Border == 'US-Canada Border') %>%
  summarise(n_cross = sum(Value)) %>%
  ggplot() + geom_line(aes(Year, n_cross, color = Measure))
# plot Crossing by different ways vs year on Mexico borders
df %>% group_by(Year, Border, Measure) %>% filter(Border == 'US-Mexico Border') %>%
  summarise(n_cross = sum(Value)) %>%
  ggplot() + geom_line(aes(Year, n_cross, color = Measure))
# number of ports on each border
# ???
nPortsMexico <- dim(table(filter(df, Border == 'US-Mexico Border')$Port_code))
nPortsCanadadim <- (table(filter(df, Border == 'US-Canada Border')$Port_code))
ggplot(df) + geom_boxplot(aes(Border, `Port Code`))
df %>% count(Border, Port_code) %>% nrow()
df %>% count(Border, Port_code) %>% filter(Border == 'US-Canada Border') %>%
  nrow()
df %>% count(Border, Port_code) %>% filter(Border == 'US-Mexico Border') %>%
  nrow()
colors <- brewer.pal(8, 'Set3')
df %>% count(Border, Port_code) %>% 
  ggplot() + geom_histogram(aes(Border), stat = 'count', fill = colors[1]) # +
# how number of ports relates to number of crossings
df %>% count(Border) # +
df %>% count(Border, wt = Value) # +
df %>%  group_by(Border) %>% summarise(Value_sum = sum(Value))
df %>% ggplot() + geom_bar(aes(x = Border, weight = Value, fill = colors[2])) # +



