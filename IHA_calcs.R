# script to clean and modify original csv's downloaded from USGS 

library(tidyverse)
library(RcppRoll)
pre_data <- read_csv("data/IHA/CO-river-Lees_1930-1941.csv")
post_data <- read_csv("data/IHA/CO-river-Lees_1990-2000.csv")

# pre-data wrangling -----------------------------------------------


head(pre_data)

pre_data <- pre_data %>%
  arrange(date) %>%
  select(date, daily_mean_cfs) %>%
  mutate(y = year(date),
         m = month(date),
         d = day(date),
         j_date = yday(date),
         consecutive_date = row_number()) %>%
  filter(y < 1940) %>%
  rename(cfs = daily_mean_cfs)

pre_data %>%
  group_by(y) %>%
  count() %>%
  ungroup() %>%
  reframe(sum(n))

write_csv(pre_data, "data/IHA/pre_data.csv")
write_tsv(pre_data, "data/IHA/pre_data.tsv")

# Post-data wrangling -----------------------------------------------------

post_data <- post_data %>%
  arrange(date) %>%
  select(date, daily_mean_cfs) %>%
  mutate(y = year(date),
         m = month(date),
         d = day(date),
         j_date = yday(date),
         consecutive_date = row_number()) %>%
  filter(y < 2000) %>%
  rename(cfs = daily_mean_cfs)

post_data %>%
  group_by(y) %>%
  count() %>%
  ungroup() %>%
  reframe(sum(n))

write_csv(post_data, "data/IHA/post_data.csv")
write_tsv(post_data, "data/IHA/post_data.tsv")

# pre-data IHA ------------------------------------------------------------

# Group 1 mean monthly values
group1_pre <- pre_data %>%
  group_by(m) %>%
  summarize(m_mean = mean(cfs),
            m_sd = sd(cfs))

# group 2 annual extremes

# annual min and max
pre_data %>%
  group_by(y) %>%
  summarise(min_1 = min(cfs),
            max_1 = max(cfs)) %>%
  summarise(mean_min_1 = mean(min_1), 
            mean_max_1 = mean(max_1))

# 3-day min and max
pre_data %>%
  group_by(y) %>%
  reframe(mean_3 = roll_mean(cfs, n = 3)) %>%
  group_by(y) %>%
  summarise(min_3 = min(mean_3),
            max_3 = max(mean_3))

# 7-day min and max
pre_data %>%
  group_by(y) %>%
  reframe(mean_7 = roll_mean(cfs, n = 7)) %>%
  group_by(y) %>%
  summarise(min_7 = min(mean_7),
            max_7 = max(mean_7))

# 30-day min and max
pre_data %>%
  group_by(y) %>%
  reframe(mean_30 = roll_mean(cfs, n = 30)) %>%
  group_by(y) %>%
  summarise(min_30 = min(mean_30),
            max_30 = max(mean_30))

# 90-day min and max
pre_data %>%
  group_by(y) %>%
  reframe(mean_90 = roll_mean(cfs, n = 90)) %>%
  group_by(y) %>%
  summarise(min_90 = min(mean_90),
            max_90 = max(mean_90))

# group 3 min
pre_data %>%
  group_by(y) %>%
  filter(cfs == min(cfs)) %>%
  select(y, cfs, j_date)

# group 3 max
pre_data %>%
  group_by(y) %>%
  filter(cfs == max(cfs)) %>%
  select(y, cfs, j_date)

# Number of times per year > 75%
pre_data %>%
  mutate(q75 = quantile(cfs, probs = 0.75)) %>%
  filter(cfs > q75) %>%
  mutate(c_lag = consecutive_date - lag(consecutive_date)) %>%
  filter(c_lag > 1) %>%
  select(y, c_lag) %>%
  group_by(y) %>%
  count()

# Average Number of days > 75%
g3_days_in <- pre_data %>%
  mutate(q75 = quantile(cfs, probs = 0.75)) %>%
  filter(cfs > q75) %>%
  mutate(c_lag = consecutive_date - lag(consecutive_date)) %>%
  filter(!is.na(c_lag)) %>%
  select(y, m, d, j_date, consecutive_date, c_lag)

days_group = vector(mode = "numeric", length = nrow(g3_days_in))
counter = 1
days_group[1] = counter
for (i in 2:nrow(g3_days_in)){
  c_lag = g3_days_in$c_lag
  if(c_lag[i+1] - c_lag[i] < 0){
    counter = counter + 1
  } 
days_group[i] = counter
}
days_group[length(days_group)] = counter
days_group

g3_days_in$days_group = days_group

g3_days_in %>%
  group_by(y, days_group) %>%
  count() %>%
  group_by(y) %>%
  summarise(mean(n))

# Number of times per year < 25%
pre_data %>%
  mutate(q25 = quantile(cfs, probs = 0.25)) %>%
  filter(cfs < q25) %>%
  mutate(c_lag = consecutive_date - lag(consecutive_date)) %>%
  filter(c_lag > 1) %>%
  select(y, c_lag) %>%
  group_by(y) %>%
  count()

# Average Number of days < 25%
g3_days_in_25 <- pre_data %>%
  mutate(q25 = quantile(cfs, probs = 0.25)) %>%
  filter(cfs< q25) %>%
  mutate(c_lag = consecutive_date - lag(consecutive_date)) %>%
  filter(!is.na(c_lag)) %>%
  select(y, m, d, j_date, consecutive_date, c_lag)

days_group = vector(mode = "numeric", length = nrow(g3_days_in_25))
counter = 1
days_group[1] = counter
for (i in 2:nrow(g3_days_in_25)){
  c_lag = g3_days_in_25$c_lag
  if(c_lag[i+1] - c_lag[i] < 0){
    counter = counter + 1
  } 
  days_group[i] = counter
}
days_group[length(days_group)] = counter
days_group

g3_days_in_25$days_group = days_group

g3_days_in_25 %>%
  group_by(y, days_group) %>%
  count() %>%
  group_by(y) %>%
  summarise(mean(n))


# group 5 positive
pre_data %>%
  mutate(cfs_lag = cfs - lag(cfs)) %>%
  filter(cfs_lag > 0) %>%
  summarise(mean_positive = mean(cfs_lag))

# group 5 positive
pre_data %>%
  mutate(cfs_lag = cfs - lag(cfs)) %>%
  filter(cfs_lag < 0) %>%
  summarise(mean_negative = mean(cfs_lag))

# group 5 number of rises
pre_data %>%
  mutate(cfs_lag = cfs - lag(cfs)) %>%
  filter(cfs_lag > 0) %>%
  mutate(c_lag = consecutive_date - lag(consecutive_date)) %>%
  filter(c_lag > 1) %>%
  select(y, c_lag) %>%
  group_by(y) %>%
  count() %>%
  ungroup() %>%
  summarize(mean(n))

# group 5 number of falls
pre_data %>%
  mutate(cfs_lag = cfs - lag(cfs)) %>%
  filter(cfs_lag < 0) %>%
  mutate(c_lag = consecutive_date - lag(consecutive_date)) %>%
  filter(c_lag > 1) %>%
  select(y, c_lag) %>%
  group_by(y) %>%
  count() %>%
  ungroup() %>%
  summarize(mean(n))



# Deviations and per cent magnitude ---------------------------------------

# deviation magnitude: post_value - pre_value
# % : deviation_magnitude / pre_value
