##### TidyTuesday 2021-04-08 #####
###     Deforestation          ###
### Author: Patrizia Maier     ###

# get packages 
library(tidyverse)
library(rnaturalearth)
library(sf)
library(classInt)
library(extrafont)
library(ggtext)
library(cowplot)
library(patchwork)
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 
# windowsFonts() # to check available options 


# get tidy tuesday data 
tuesdata <- tidytuesdayR::tt_load('2021-04-06')

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil


# get map data 
world_map <- ne_countries(returnclass='sf') %>% 
  select("iso_a3", "geometry")

# combine map and forest data 
data <- world_map %>% left_join(forest_area %>% 
                                filter(year==2001 | year==2013) %>% 
                                  pivot_wider(names_from = year, names_glue = "{.value}_{year}", values_from=forest_area) %>%
                                  group_by(code) %>% 
                                  mutate(change_2013_2001=forest_area_2013-forest_area_2001),
                                by=c("iso_a3"="code")) %>% 
  drop_na(change_2013_2001)

# plot map 
map <- ggplot(data) + 
  geom_sf(aes(fill=change_2013_2001)) + 
  scale_fill_distiller(palette="RdYlGn", direction = 1, guide=guide_legend()) + 
  theme_minimal() + 
  theme(plot.title = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(),
        plot.caption.position = "plot",
        legend.position = c(0.1, 0.25),
        legend.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        text=element_text(family="Times New Roman")) + 
  guides(fill=guide_colorbar(title.position = "top", 
                             title.hjust = 0.5,
                             barheight = unit(5, 'lines'),
                             barwidth = unit(1, 'lines'),
                             ticks=F)) + 
  labs(title="**Worldwide largest forest loss happens in Brazil**",
       subtitle="*between 2001 and 2013*",
       #caption = "DataViz: @PatriziaMaier | Data: Our World in Data",
       fill="Change \nin %")
  

# stacked bar chart on causes 
brazil_loss_long <- brazil_loss %>% 
  select(-entity, -code) %>% 
  group_by(year) %>% 
  pivot_longer(cols = !year)

causes <- ggplot(brazil_loss_long, aes(x=value, y=factor(year), fill=name)) + 
  geom_bar(stat="identity", color="black") + 
  scale_fill_brewer(palette="Spectral", direction = -1, 
                    labels=c("crops", "fire", "flooding due to dams", "mining", "natural disturbance", 
                             "other infrastructure", "pasture", "roads", "selective logging", 
                             "small-scale clearing", "plantations")) + 
  scale_x_continuous(labels=scales::number) + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(), 
        axis.text.x = element_text(size=10, angle=0),
        axis.text.y = element_text(size=8, angle=45),
        text=element_text(family="Times New Roman")) + 
  labs(title="**Causes of forest loss in Brazil**",
       subtitle="*between 2001 and 2013*",
       #caption = "DataViz: @PatriziaMaier | Data: Our World in Data",
       x="**Loss in hectares** (ha)")

# stacked bar chart on soybean production
brazil_soybean <- soybean %>%
  filter(code=="BRA" & year>=2001 & year <=2013) %>% 
  select(-entity, -code) %>% 
  group_by(year) %>% 
  pivot_longer(cols = !year)

chart_soybean <- ggplot(brazil_soybean, aes(x=value, y=factor(year), fill=name)) + 
  geom_bar(stat="identity", color="black") + 
  scale_fill_brewer(palette="Spectral", direction = -1, 
                    labels=c("animal feed", "human food", "processed")) + 
  scale_x_continuous(labels=scales::number) + 
  coord_flip() + 
  theme_minimal() + 
  theme(plot.title = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(), 
        axis.text.x = element_text(size=10, angle=0),
        axis.text.y = element_text(size=8, angle=45),
        text=element_text(family="Times New Roman")) +
  labs(title="**Soybean production in Brazil**",
       subtitle = "*between 2001 and 2013*",
       #caption = "DataViz: @PatriziaMaier | Data: Our World in Data",
       x="**Production in tons** (t)") 


# pie chart on vegetable oil production 
brazil_oil <- vegetable_oil %>%
  filter(code=="BRA" & year>=2001 & year <=2013) %>% 
  group_by(crop_oil) %>% 
  summarise(production=mean(production))

pie_oil <- ggplot(brazil_oil, aes(x="", y=production, fill=crop_oil)) + 
  geom_bar(stat="identity", color="black") + 
  coord_polar("y") + 
  scale_fill_brewer(palette="Spectral", direction = -1, 
                    labels=c("Coconut", "Cottonseed", "Groundnut", "Linseed", "Maize", 
                             "Palm", "Palm kernel", "Rapeseed", "**Soybean**", "Sunflower")) + 
  theme_void() + 
  theme(plot.title = element_markdown(),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(),
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.text = element_markdown(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        text=element_text(family="Times New Roman")) +
  labs(title="**Role of soybean for vegetable oil production in Brazil**",
       subtitle = "*between 2001 and 2013*",
       caption = "DataViz: @PatriziaMaier | Data: Our World in Data")  

(map) / (causes + pie_oil) + plot_layout(guides="collect")

ggsave("C:/Users/Patrizia/Data/Data Science/TidyTuesday/deforestation_2021.png", width=10, height=15)

