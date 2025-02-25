##### TidyTuesday 2020-12-15 #####
###       Tate Museum          ###


# get packages 
library(tidyverse)
library(tidytext)
library(stopwords)
library(ggwordcloud)
library(viridis)
library(extrafont)
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 


# get data 
tuesdata <- tidytuesdayR::tt_load('2021-01-12')
artwork <- tuesdata$artwork
artists <- tuesdata$artists


# inspect data 
length(unique(artwork$artistId)) # 3532 different artists and over 69.000 different artworks 
count(artists, gender) # 521 women, 2895 men, 116 NA
count(artwork, medium, sort=TRUE) # already > 3000 unique technique but overlapping in syntax --> tidytext
count(artwork, year) # from 1545 till 2012, many more in recent years 


# join data sets
data <- left_join(artwork, 
                  artists,
                  by = c("artistId" = "id"))

# add century 
data <- data %>%
  mutate(century=plyr::round_any(data$year, 100, f = floor))


# most frequent keywords per category (with tidytext)
top=5
word_counts <- data %>%
  select(id, medium, century, gender) %>%
  drop_na() %>%
  unnest_tokens(word, medium) %>% 
  anti_join(get_stopwords()) %>% 
  group_by(century, gender) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  slice(1:top) %>%
  mutate(rank=order(n, decreasing=TRUE))


# create plot
set.seed(100)
ggplot(word_counts, aes(x=century, label=word, size=rank, color=century)) +
  geom_text_wordcloud(family="Segoe UI Black", shape="circle", eccentricity=0.4) + 
  scale_size(range=c(4,6), trans='reverse') +
  coord_flip() + 
  scale_x_reverse() + 
  scale_color_viridis() + 
  facet_wrap(~gender, ncol=2, labeller=as_labeller(c(`Female` = "female artists", `Male` = "male artists"))) +
  labs(title="Top 5 mediums of art in the Tate",
       subtitle="... by century of origin and artist gender\n",
       caption="\nby @PatriziaMaier for #TidyTuesday\ndata from The Tate Museum",
       x=NULL, y=NULL) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Segoe UI Black", size=20),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size=16, color="#666666"),
        plot.caption = element_text(family = "Segoe UI Semibold", size=12, color="#666666"),
        axis.text = element_text(family = "Segoe UI Semibold", size=14),
        strip.text.x = element_text(family = "Segoe UI Semibold", size=14)) 

ggsave("C:/Users/Patrizia/Data/Data Science/TidyTuesday/tate_museum_2021.png", width=6, height=10)


# most frequent keywords per category (with tidytext)
top=5
word_counts <- data %>%
  select(id, title, century, gender) %>%
  drop_na() %>%
  unnest_tokens(word, title) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!(word %in% c("n.y.c", "el", "15", "1", "2", "3", "4", "5", "4th", "st", "ii", "iii", "vol", "title", "untitled", "unknown", "verso", "sketch"))) %>%
  group_by(century, gender) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  slice(1:top) %>%
  mutate(rank=order(n, decreasing=TRUE))


# create plot
set.seed(100)
ggplot(word_counts, aes(x=century, label=word, size=rank, color=century)) +
  geom_text_wordcloud(family="Segoe UI Black", shape="circle", eccentricity=0.4) + 
  scale_size(range=c(4,6), trans='reverse') +
  coord_flip() + 
  scale_x_reverse() + 
  scale_color_viridis() + 
  facet_wrap(~gender, ncol=2, labeller=as_labeller(c(`Female` = "Female Artists", `Male` = "Male Artists"))) +
  labs(title="Top 5 title keywords in the Tate",
       subtitle="... by century of origin and artist gender\n",
       caption="by @PatriziaMaier for #TidyTuesday\ndata from The Tate Museum",
       x=NULL, y=NULL) + 
  theme_minimal() + 
  theme(plot.title = element_text(family = "Segoe UI Black", size=20),
        plot.subtitle = element_text(family = "Segoe UI Semibold", size=16, color="#666666"),
        plot.caption = element_text(family = "Segoe UI Semibold", size=12, color="#666666"),
        axis.text = element_text(family = "Segoe UI Semibold", size=14),
        strip.text.x = element_text(family = "Segoe UI Semibold", size=14)) 
