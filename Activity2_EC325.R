# install.packages(c("dplyr", "lubridate", "ggplot2", "gridExtra"))

library(gridExtra)
library(dplyr)
library(lubridate)
library(ggplot2)

streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

# Parse our date
streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")
year(streamH$dateF)

# Join site info to stream gauge height
floods <- full_join(streamH, siteInfo, by= 'siteID')

#subset stream height to just include peace river
peaceH <- streamH[streamH$siteID == 2295637,]

# subset floods to just include peace river
peace <- floods %>% filter(siteID == 2295637)

# subset floods where height of river is greater or equal to 10
example <- floods %>% filter(gheight.ft >= 10)

plot(peaceH$dateF, peaceH$gheight.ft, type="l", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

max.ht <- floods %>%
  group_by(names) %>%
  summarise(max_ht_ft=max(gheight.ft),
            mean_height = mean(gheight.ft))

min_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  # slice_min(datetime) 
  summarize(min_date = min(datetime))
min_date

par(mfrow = c(2, 2))
# Histogram of Peace River
peace <- floods %>% filter(siteID == 2295637)
hist(peace$gheight.ft, 
     main = "Peace River Stream Stage Frequency",
     ylim = c(0,400),
     xlab = "Stream Stage (ft)",
     col = 'blue')

# Histogram of FishEating Creek
FishEating_creek <- floods %>% filter(siteID == 2256500)
hist(FishEating_creek$gheight.ft, 
     main = "FishEating Creek Stream Stage Frequency",
     xlab = "Stream Stage (ft)",
     ylim = c(0,900),
     xlim= c(5,11),
     col = 'yellow')

# Histogram of Santa Fe Creek
Santa_Fe <- floods %>% filter(siteID == 2322500)
hist(Santa_Fe$gheight.ft, 
     main = "Santa Fe Creek Stream Stage Frequency",
     xlab = "Stream Stage (ft)",
     ylim = c(0, 500),
     xlim = c(0,20),
     col = 'purple')

# Histogram of Withlacoochee River
Withlacoochee <- floods %>% filter(siteID == 2312000)
hist(Withlacoochee$gheight.ft, 
     main = "Withlacoochee Stream Stage Frequency",
     xlab = "Stream Stage (ft)",
     ylim = c(0, 400),
     xlim = c(8,20),
     col = 'green')
par(mfrow = c(1, 1))

# Homework Question 1
peace <- floods %>% filter(siteID == 2295637)
FishEating_creek <- floods %>% filter(siteID == 2256500)
Santa_Fe <- floods %>% filter(siteID == 2322500)
Withlacoochee <- floods %>% filter(siteID == 2312000)

peace_plot <- ggplot(peace, aes(x = dateF, y = gheight.ft)) +
  geom_line() + 
  labs(title = "Peace Stream Height (ft)", x = "Date", y = "Stage Height (ft)")

fish_eating_plot <- ggplot(FishEating_creek, aes(x = dateF, y = gheight.ft)) +
  geom_line() + 
  labs(title = "Fish Eating Creek Stream Height (ft)", x = "Date", y = "Stage Height (ft)")

santa_fe_plot <- ggplot(Santa_Fe, aes(x = dateF, y = gheight.ft)) +
  geom_line() + 
  labs(title = "Santa Fe Stream Height (ft)", x = "Date", y = "Stage Height (ft)")

withlacoochee_plot <- ggplot(Withlacoochee, aes(x = dateF, y = gheight.ft)) +
  geom_line() + 
  labs(title = "Withlacoochee Stream Height (ft)", x = "Date", y = "Stage Height (ft)")

grid.arrange(peace_plot, fish_eating_plot, santa_fe_plot, withlacoochee_plot, ncol = 2)

# Homework Question 2

#tutorial method
max.cat <- floods %>%
  group_by(names) %>% 
  filter(gheight.ft >= major.ft) %>% 
  summarize(earliest_date = min(datetime)) 

moderate.cat <- floods %>%
  group_by(names) %>% 
  filter(gheight.ft >= moderate.ft) %>% 
  summarize(earliest_date = min(datetime)) 

flood.cat <- floods %>%
  group_by(names) %>% 
  filter(gheight.ft >= flood.ft) %>% 
  summarize(earliest_date = min(datetime)) 

action.cat <- floods %>%
  group_by(names) %>% 
  filter(gheight.ft >= action.ft) %>% 
  summarize(earliest_date = min(datetime)) 

# My method
first_occurrence_flood_category <- floods %>%
  group_by(names) %>%
  summarize(
    first_action_ft_occurrence = ifelse(any(gheight.ft >= action.ft), 
                                        min(datetime[gheight.ft >= action.ft], na.rm = TRUE), 
                                        NA),
    first_flood_ft_occurrence = ifelse(any(gheight.ft >= flood.ft), 
                                       min(datetime[gheight.ft >= flood.ft], na.rm = TRUE), 
                                       NA),
    first_moderate_ft_occurrence = ifelse(any(gheight.ft >= moderate.ft), 
                                          min(datetime[gheight.ft >= moderate.ft], na.rm = TRUE), 
                                          NA),
    first_major_ft_occurrence = ifelse(any(gheight.ft >= major.ft), 
                                       min(datetime[gheight.ft >= major.ft], na.rm = TRUE), 
                                       NA)) %>%
  mutate(
    flood_less_action = as.numeric(difftime(first_flood_ft_occurrence, 
                                            first_action_ft_occurrence, 
                                            units = "days")),
    moderate_less_flood = as.numeric(difftime(first_moderate_ft_occurrence, 
                                              first_flood_ft_occurrence, 
                                              units = "days")),
    major_less_moderate = as.numeric(difftime(first_major_ft_occurrence, 
                                              first_moderate_ft_occurrence, 
                                              units = "days")))


# Homework Question 3
max_stream_stage <- floods %>%
  group_by(names) %>% 
  filter(gheight.ft > major.ft) %>%  
  summarize(max_gheight = max(gheight.ft, na.rm = TRUE),  major_ft = first(major.ft)) %>%
  mutate(gheight_less_major = max_gheight - major_ft) 






