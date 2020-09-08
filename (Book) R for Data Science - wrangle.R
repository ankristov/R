library(tidyverse)

# tibble
tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)

tribble(
  ~x, ~y, ~z,
  #--/--/--
  "a", 2, 3.6,
  "b", 1, 8.5
)

tibble(
  a = lubridate::now() + runif(1e3) + 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters,1e3, replace = T)
)
# how to view more rows and columns
nycflights13::flights %>%
  print(n = 20, width = Inf)
options(tibble.print_max = 10)
options(tibble.width = Inf)

nycflights13::flights %>%
  View()

read_csv("a,b,c\n1,2,3")
read_csv("a,b,c
         1,2,3")
read_csv("A comment we whant to skip 
         a,b,c
         1,2,3")
read_csv("#A comment we whant to skip 
a,b,c\n1,2,3", comment = "#")

read_csv("x,y\n1,'a,b'")
read_delim("x,y\n1,'a,b'", quote = "'",delim = ",")

# Parsers
str(parse_logical(c("TRUE","FALSE", "NA")))
str(parse_integer(c("1","3","5","3")))
str(parse_date(c("2010-01-01","1983-07-12")))
x <- parse_integer(c("123","345","abc","123.45"))
problems(x)
parse_double("1.23")
parse_double("1,23")
parse_double("1,23", locale = locale(decimal_mark = ","))
parse_double("The Pi number is 3.1415...")
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")
parse_number("The Pi number is 3.1415...")
parse_number("$123,456,789")
parse_number("$123.456.789")
parse_number("$123.456.789", locale = locale(grouping_mark = "."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))
# What happens if you try and set decimal_mark and group ing_mark to the same character?
parse_number("112.223.456.233", locale = locale(decimal_mark = ".", grouping_mark = "."))
d1 <- "January 1, 2010"
parse_date(d1, format = "%B %d, %Y")
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
challenge <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(x = col_integer(), y = col_character()))
problems(challenge)
head(challenge)
tail(challenge)
challenge <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(y = col_date()))
problems(challenge)
head(challenge)
tail(challenge)
challenge <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(x = col_double(), y = col_date()))
head(challenge)
tail(challenge)
challenge2 <- read_csv(readr_example("challenge.csv"),
                       guess_max = 1001) # only one more row is inough to correctly guess type
challenge2
# Sometimes it’s easier to diagnose problems if you just read in all the columns as character vectors:
challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()))
head(challenge)
tail(challenge)
type_convert(challenge2)
# to see what we have in file before parsing
read_lines(readr_example("challenge.csv"), n_max = 200, skip = 1000)










