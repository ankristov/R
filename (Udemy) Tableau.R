library(ggplot2)
library(dplyr)
office <- read.csv(file = '/Users/Andrew/Desktop/Tableau/P1-OfficeSupplies-MAC.csv')
head(office)
str(office)
ggplot(office) +
  geom_col(aes(x = Rep, y = Units, fill = Region)) +
  facet_grid(cols = vars(Region)) +
  geom_
office %>% group_by(Rep) %>% summarise(unitSum = sum(Units))
           