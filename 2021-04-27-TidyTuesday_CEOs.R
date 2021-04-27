##### TidyTuesday 2021-04-27 #####
###         CEO's              ###
### Author: Patrizia Maier     ###


# get packages 
library(tidyverse)
library(gender)
library(genderdata)
library(extrafont)
library(cowplot)
library(patchwork)
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


summary(departures$departure_code)
#   1    2    3    4    5    6    7    8    9 NA's 
#  84   97 1320  195 3598  183 2133   53   93 1667

# relevel! 
ord_levels <- c("Involuntary_death", "Involuntary_illness", 
                    "Involuntary_low_performer", "Involuntary_legal_violation", 
                    "Voluntary_retired", "voluntary_career",
                    "Other", "Missing", "Error")

criminal_ceos <- departures %>% 
  filter(departure_code=="4")



# plot
ggplot(departures, aes(x=gender)) + 
  geom_bar()

ggplot(departures, aes(x=departure_code)) + 
  geom_bar()


# save
ggsave("C:/Users/Patrizia/Data/Data Science/TidyTuesday/ceo_2021.png")