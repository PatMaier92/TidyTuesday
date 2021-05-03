##### TidyTuesday 2021-04-27 #####
###         CEO's              ###
### Author: Patrizia Maier     ###


# get packages 
library(tidyverse)
library(gender)
library(genderdata)
library(extrafont)
library(ggtext)
library(cowplot)
library(patchwork)
library(waffle)
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 
# windowsFonts() # to check available options 


# get tidy tuesday data 
tuesdata <- tidytuesdayR::tt_load('2021-04-27')
departures <- tuesdata$departures


# add gender information
first_name <- departures$exec_fullname %>% 
  strsplit(split=" ") %>% # split names
  rvest::pluck(1) %>%  # get first name
  unlist() 

gender_guess <- first_name %>% 
  gender(years = c(1920, 1985), method = "ssa") %>% # apply gender estimation
  select(!starts_with("year")) %>% 
  distinct() # remove duplicates 

departures <- departures %>% 
  bind_cols(first_name) %>% 
  rename("first_name"="...20") %>% 
  left_join(gender_guess, by=c("first_name"="name")) %>%  
  mutate(gender=factor(gender),
         departure_code=factor(departure_code))

summary(departures$gender)
# female   male   NA's 
#   342   8438    643 

female_ceos <- departures %>% 
  filter(gender=="female")
# problem: many "female" are actually "male" as indicated by "notes" (Mr. X, he, ...)


# plot
data_gender <- departures %>% 
  group_by(fyear) %>% 
  count(gender) %>% 
  drop_na(gender) %>% 
  complete(gender, fill=list(n=0)) %>% 
  filter(fyear>=1992 & fyear <2020) 

p1 <- ggplot(data_gender, aes(fill = gender, values = n)) +
  geom_waffle(color = "#f7f2e8", size = .5, n_rows = 5, flip = TRUE) +
  facet_wrap(~fyear,  nrow = 1, strip.position = "bottom") +
  scale_fill_manual(values = c("#ef8f10", "#9f969b")) + 
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 5, # make this multiplier the same as n_rows
                     expand = c(0,0)) +
  coord_equal(clip="off") + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f7f2e8", linetype = "blank"),
        plot.title = element_markdown(size=30), 
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size=16),
        plot.caption = element_markdown(size=8),
        panel.grid = element_blank(),
        panel.spacing.x = unit(0.75, "lines"),
        axis.ticks.y = element_line(),
        legend.position = "none", 
        legend.title = element_blank(),
        legend.direction = "horizontal",
        text=element_text(family = "Bahnschrift", size=11),
        strip.switch.pad.wrap = unit(0, "cm"),
        strip.text = element_text(angle = 45)) +
  labs(title="CEO departures in major US firms", 
       subtitle="highlighting **<span style = 'color:#ef8f10;'>female</span>** CEO's",
       caption = "\nDataviz: Patrizia Maier | Data: Gentry et al. 2021 | Firms from S&P 1500 index") 

# save
ggsave("C:/Users/Patrizia/Data/Data Science/TidyTuesday/ceo_gender_2021.jpeg",
       width = 30, height = 20, units = "cm")


data_departure <- departures %>% 
  filter(fyear>=1992 & fyear<2020) %>% 
  filter(departure_code %in% c(1, 2, 3, 4, 5, 6)) %>% 
  drop_na(departure_code) %>% 
  group_by(departure_code) %>% 
  count(gender) %>% 
  drop_na(gender) %>% 
  complete(gender, fill=list(n=0)) 

mylabels <- as_labeller(c(`1` = "Death", `2` = "Illness", 
                          `3` = "Low performance", `4` = "Legal violations", 
                          `5` = "Retirement", `6` = "Career change",
                          `7` = "Other", `8` = "Missing", `9` = "Error"))

p2 <- ggplot(data_departure, aes(fill = gender, values = n)) +
  geom_waffle(color = "#f7f2e8", size = .5, n_rows = 25, flip=T) +
  facet_wrap(~departure_code,  nrow = 1, strip.position = "bottom", labeller=mylabels) +
  scale_fill_manual(values = c("#ef8f10", "#9f969b")) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 20) +
  coord_equal(clip="off") + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#f7f2e8", linetype = "blank"),
        plot.title = element_markdown(size=30), 
        plot.title.position = "plot",
        plot.subtitle = element_markdown(size=16),
        plot.caption = element_markdown(size=8),
        panel.grid = element_blank(),
        panel.spacing.x = unit(0.75, "lines"),
        axis.ticks.y = element_line(),
        legend.position = "none", 
        legend.title = element_blank(),
        legend.direction = "horizontal",
        text=element_text(family = "Bahnschrift", size=11),
        strip.switch.pad.wrap = unit(0, "cm"),
        strip.text = element_text(angle = 0)) +
  labs(title="CEO departures in major US firms", 
       subtitle="highlighting **<span style = 'color:#ef8f10;'>female</span>** CEO's",
       caption = "\nDataviz: Patrizia Maier | Data: Gentry et al. 2021 | Firms from S&P 1500 index") 

# save
ggsave("C:/Users/Patrizia/Data/Data Science/TidyTuesday/ceo_departures_2021.jpeg",
       width = 35, height = 20, units = "cm")