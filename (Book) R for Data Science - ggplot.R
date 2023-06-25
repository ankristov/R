install.packages("tidyverse")
library(tidyverse)
install.packages(c("nucflights13","gapminder","Lahman"))

mpg
sapply(mpg,table)

#################################
# geom_point
#################################

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x=cyl,y=hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x=drv,y=class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class, shape = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy),color = "blue")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 3)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class),shape = 3)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 0)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 0, color = "red")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 15)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 15, color = "red")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 22)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 22, fill = "red", color = "blue")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), shape = 22, fill = "red", color = "blue", size = 5, stroke = 3)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = cty))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class, shape = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ cyl, nrow = 2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ fl, nrow = 2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ hwy, nrow = 2)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(cyl ~ drv)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(cyl ~ .)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ .)

#################################
# geom_smoth
#################################

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(color = "blue") + 
  geom_point(mapping = aes(color = class))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), color = "blue") + 
  geom_point(mapping = aes(color = class))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv),) +
  geom_point() + geom_smooth(se = F)

#################################
# histograms
#################################
diamonds
str(diamonds)
sapply(select(diamonds,cut,color,clarity), table)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth)
  )
ggplot(data = diamonds) + 
  stat_ellipse(mapping = aes(x = cut, y = depth))
ggplot(data = diamonds) +
  geom_col(mapping = aes(x = cut, y = depth))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop..,group = 1))
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = color, y = ..prop.., group = 1))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color = cut))
# position 
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(alpha = 1/4, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) +
  geom_bar(fill = NA, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill")
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(position = "stack")
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")
ggplot(data = mpg) + geom_point(aes(displ,hwy))
ggplot(data = mpg) + geom_point(aes(displ,hwy), position = "jitter")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter")
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(width = 1)
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_count(position = "jitter")

ggplot(data = mpg) +
  geom_boxplot(aes(class, hwy, color = class))
ggplot(data = mpg) +
  geom_boxplot(aes(class, hwy, color = class)) +
  coord_flip()

bar <- ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut), show.legend = F, width = 1) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

p <- ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter") +
  geom_abline() +
  coord_fixed() +
  theme(aspect.ratio = 1)
p
p + geom_vline(xintercept = 20)
p + geom_vline(xintercept = c(10,20,30))
p + geom_hline(yintercept = 20)
p + geom_hline(yintercept = mean(mpg$hwy))
p + geom_abline(intercept = 5)
p + geom_abline(intercept = 30, slope = 0.1)
p + geom_smooth(se = F)
