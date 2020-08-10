
library(tidytuesdayR)
library(tidyverse)

## Get the data ####
tt_data <- tidytuesdayR::tt_load('2020-08-04')
readme(tt_data)

energy_types <- tt_data$energy_types

percent_solar <-  energy_types   %>% 
  filter(level == "Level 1")   %>% 
  mutate(country_name = case_when(
    country == "EL" ~ "Greece",
    country == "UK"  ~ "UK",
    TRUE ~ as.character(country_name))) %>% 
  gather(year, energy, 5:7) %>%
  group_by(country_name, year) %>%
  mutate(percent = 100*energy/sum(energy)) %>%
  ungroup() %>%
  filter(type == "Solar")   %>% 
  select(c("country_name", "type", "year", "percent")) %>%
  pivot_wider(names_from = c(type,year), values_from = percent) %>%
  mutate(Change_17to16 = Solar_2017- Solar_2016, Change_18to17 = Solar_2018- Solar_2017) %>%
  select(c("country_name", "Change_17to16", "Change_18to17" ))

plot2017 <- percent_solar %>% 
  ggplot(aes(country_name, Change_17to16)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) + 
  coord_flip() +
  xlab("") +
  ylab("Percentage Change") +
  scale_y_continuous(limits=c(-0.5,1)) + 
  ggtitle("Percent Solar Energy Change from 2016 to 2017 for European Countries")

dev.new() 
plot2018 <- percent_solar %>% 
  ggplot(aes(country_name, Change_18to17)) + 
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) + 
  coord_flip() +
  xlab("") +
  ylab("Percentage Change") +
  scale_y_continuous(limits=c(-0.5,1)) + 
  ggtitle("Percent Solar Energy Change from 2017 to 2018 for European Countries")



