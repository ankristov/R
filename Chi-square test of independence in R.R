# https://statsandr.com/blog/chi-square-test-of-independence-in-r/

df <- iris
head(df)
df$size <- ifelse(iris$Sepal.Length > median(iris$Sepal.Length), 'big','small')
table(df$Species, df$size)
library(ggplot2)
ggplot(data=df) + geom_bar(aes(x=Species,fill=size),position='stack')
ggplot(data=df) + geom_bar(aes(x=Species,fill=size),position = 'fill')
ggplot(data=df) + geom_bar(aes(x=Species,fill=size),position = 'dodge')
test <- chisq.test(df$Species,df$size)
test
test$statistic
test$p.value
test$method
summary(table(df$Species, df$size))
