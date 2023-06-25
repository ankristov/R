library(tidyverse)
library(ggplot2)

df <- read.csv(file = '/Users/Andrew/Documents/Docs/Analytics/A:B testing/dataset/ASOS Digital Experiments Dataset/osfstorage-archive/asos_digital_experiments_dataset.csv')
head(df)
View(df)
str(df)
unique(df$experiment_id)

# choose experiments and calculate cumulative metrics
df_small <- df %>% filter(experiment_id == '54a85a', metric_id == 1)
head(df_small)
View(df_small)

df_small_v0 <- df_small %>% filter(variant_id == 0)%>% mutate(count_c_cum = cumsum(count_c),
                                                              count_t_cum = cumsum(count_t),
                                                              mean_c_cum = cummean(mean_c),
                                                              mean_t_cum = cummean(mean_t),
                                                              mean_t_c_cum_dif = mean_t_cum - mean_c_cum,
                                                              variance_c_cum = cummean(variance_c),
                                                              variance_t_cum = cummean(variance_t))
df_small_v1 <- df_small %>% filter(variant_id == 1)%>% mutate(count_c_cum = cumsum(count_c),
                                                              count_t_cum = cumsum(count_t),
                                                              mean_c_cum = cummean(mean_c),
                                                              mean_t_cum = cummean(mean_t),
                                                              mean_t_c_dif = mean_t_cum - mean_c_cum,
                                                              variance_c_cum = cummean(variance_c),
                                                              variance_t_cum = cummean(variance_t))
df_small_v2 <- df_small %>% filter(variant_id == 2)%>% mutate(count_c_cum = cumsum(count_c),
                                                              count_t_cum = cumsum(count_t),
                                                              mean_c_cum = cummean(mean_c),
                                                              mean_t_cum = cummean(mean_t),
                                                              mean_t_c_dif = mean_t_cum - mean_c_cum,
                                                              variance_c_cum = cummean(variance_c),
                                                              variance_t_cum = cummean(variance_t))
head(df_small_v0)
View(df_small_v1)
summary(df_small_v0)
# plot cum means for t and c
df_small_v1 %>%
  ggplot() + 
  geom_line(aes(x = time_since_start, y = mean_c_cum), color = 'grey') +
  geom_line(aes(x = time_since_start, y = mean_t_cum), color = 'green') 
df_small_v1 %>%
  ggplot() + 
    geom_line(aes(x = time_since_start, y = mean_t_c_dif), color = 'grey')
# plot cum variances for t and c
df_small_v1 %>%
  ggplot() + 
  geom_line(aes(x = time_since_start, y = variance_c_cum), color = 'grey') +
  geom_line(aes(x = time_since_start, y = variance_t_cum), color = 'green') 
df_small_v1 %>%
  ggplot() + 
  geom_line(aes(x = time_since_start, y = mean_t_c_dif), color = 'grey')

df[, c()]