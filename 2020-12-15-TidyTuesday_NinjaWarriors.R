##### TidyTuesday 2020-12-15 #####
###       Ninja Warriors       ###


# get packages 
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(extrafont)
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 


# get data 
tuesdata <- tidytuesdayR::tt_load('2020-12-15')
ninja_warrior <- tuesdata$ninja_warrior


# inspect data 
unique(ninja_warrior$season) # 10 seasons
unique(ninja_warrior$location) # 30 locations, some are vague (e.g. Southwest), some are detailed (e.g. Miami)
unique(ninja_warrior$round_stage) # 2x qualifying, 2x semi-finals/finals(regional), 4 stages for national finals --> simplify
unique(ninja_warrior$obstacle_name)
length(unique(ninja_warrior$obstacle_name)) # 225 in total, mostly 2 or 3 words
unique(ninja_warrior$obstacle_order) # maximum 10 rounds 


# add new var with simplified round_stage: "National Finals" "Regional Finals" "Qualifying" 
ninja_warrior <- ninja_warrior %>% 
  mutate(round_stage_cat = case_when(
    round_stage=="Qualifying (Regional/City)" | round_stage=="Qualifying" ~ "Qualifying",
    round_stage=="Semi-Finals" |  round_stage=="Finals (Regional/City)" ~ "Regional Finals",
    TRUE ~ "National Finals")) %>%
  mutate(round_stage_cat = fct_relevel(round_stage_cat, c("Qualifying", "Regional Finals", "National Finals")))


# most frequent obstacle names per category
name_counts <- ninja_warrior %>% 
  group_by(round_stage_cat) %>%
  count(obstacle_name) %>%
  arrange(desc(n)) %>% 
  slice(1:10)


# create plot
set.seed(100)
p <- ggplot(name_counts, aes(x=round_stage_cat, label=obstacle_name, size=n, color=round_stage_cat)) +
  geom_text_wordcloud(eccentricity = 2, family = "Segoe UI Black") + 
  scale_color_manual(values = c("#330099", "#333333", "#CC3333")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Segoe UI Black", size=20, color="#CC3333"),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size=16),
        plot.caption = element_text(family = "Segoe UI Semibold", size=12),
        axis.text = element_text(family = "Segoe UI Semibold", size=15)) + 
  labs(title="American Ninja Warrior", 
       subtitle="the 10 most frequent 'obstacle names' per round stage",
       caption = "\nby @PatriziaMaier for #TidyTuesday",
       x=NULL)

ggsave("D:/TidyTuesday/american_warrior_full_names_2020.png", width=8, height=4)


# most frequent single keywords per category (with tidytext)
word_counts <- ninja_warrior %>%
  select(season, round_stage_cat, obstacle_name) %>%
  unnest_tokens(word, obstacle_name) %>% 
  group_by(round_stage_cat) %>%
  count(word) %>%
  arrange(desc(n)) %>% 
  slice(1:10)


# create plot
set.seed(100)
p <- ggplot(word_counts, aes(x=round_stage_cat, label=word, size=n, color=round_stage_cat)) +
  geom_text_wordcloud(eccentricity = 2, family = "Segoe UI Black") + 
  scale_color_manual(values = c("#330099", "#333333", "#CC3333")) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Segoe UI Black", size=20, color="#CC3333"),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size=16),
        plot.caption = element_text(family = "Segoe UI Semibold", size=12),
        axis.text = element_text(family = "Segoe UI Semibold", size=15)) + 
  labs(title="American Ninja Warrior", 
       subtitle="the 10 most frequent 'obstacle keywords' per round stage",
       caption = "\nby @PatriziaMaier for #TidyTuesday",
       x=NULL)

ggsave("D:/TidyTuesday/american_warrior_words_2020.png", width=8, height=4)