##### TidyTuesday 2020-12-08 #####
###       Woman of 2020        ###


# get packages 
library(rnaturalearth)
library(countrycode)
library(tmap)
library(tidyverse)


# get data 
tuesdata <- tidytuesdayR::tt_load('2020-12-08')
women <- tuesdata$women


# clean data 
women <- women[!women$name == "Unsung hero",] # remove Unsung hero, sorry
women <- women %>% 
  mutate(country = case_when(
      country=="India " ~ "India",
      country=="Somaliland" ~ "Somalia",
      country=="UK " | country=="Iraq/UK" | country=="Wales, UK" | country=="Northern Ireland" ~ "UK",
      country=="Exiled Uighur from Ghulja (in Chinese, Yining)" ~ "China", 
      TRUE ~ country))


# save country code and continent information
women$iso_a3 <- countrycode(women$country, origin = 'country.name', destination = 'iso3c')


# get world geometry polygons 
world <- ne_countries(returnclass='sf') %>% 
  select("iso_a3", "geometry")
 

# join data 
dat <- left_join(world, 
                 women %>%
                   group_by(iso_a3, category) %>% 
                   summarise(count = n()), 
                 by="iso_a3") %>%
  mutate(anyone=if_else(count > 0, 1, 0))


# make map
tmap_style("white")
tm_shape(dat) + 
  tm_fill(col="category", legend.show=FALSE, palette="Spectral") + 
  tm_facets(by="category", free.coords=FALSE, drop.units=TRUE,  drop.NA.facets=TRUE) +
  tm_layout(main.title="Where do BBC's 'Women of 2020' Awardees live?", 
            main.title.position = "center",
            sepia.intensity=0.1,
            fontfamily="mono") + 
tm_shape(dat) + 
  tm_borders(col="grey")
