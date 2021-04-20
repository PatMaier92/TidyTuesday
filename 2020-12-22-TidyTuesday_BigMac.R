##### TidyTuesday 2020-12-08 #####
###    Big Mac Economics       ###


# get packages 
library(rnaturalearth)
library(tmap)
library(tidyverse)
library(gifski)
library(extrafont)
# font_import() # only once 
loadfonts(device = "win", quiet = TRUE) # every time 


# get data 
tuesdata <- tidytuesdayR::tt_load('2020-12-22')
big_mac <- tuesdata$`big-mac`


# inspect data
# lots of missing values in GDP_dollar, adj_price, XXX_adjusted --> correct or use other vars 
# unique(big_mac$date) # from 2000 to 2020 --> use as time factor


# get world geometry polygons 
world <- ne_countries(returnclass='sf') %>% 
  select("iso_a3", "geometry")


# join data 
dat <- left_join(world, 
                 big_mac %>% 
                   group_by(date),
                 by="iso_a3")
dat$date <- as.factor(dat$date)


# create map
tmap_style("white")
animation <- tm_shape(dat) + 
  tm_fill(col="usd_raw", legend.show=TRUE, title="Rel. index to USD (raw)", palette="-RdYlGn", midpoint=0) + 
  tm_facets(by="date", free.coords=FALSE, drop.units=TRUE,  drop.NA.facets=TRUE, nrow = 1, ncol = 1) +
  tm_layout(main.title="How expensive is a Big Mac ...", 
            main.title.position="left",
            main.title.size=3,
            main.title.color="#003366", 
            title="... compared to buying it in the US?",
            title.color="#003366",
            title.size=2,
            title.position=c("right","bottom"),
            legend.position=c("left", "bottom"),
            legend.title.size=1.75,
            legend.text.size=1.25,
            panel.label.size=1.5,
            sepia.intensity=0.1,
            fontfamily="Segoe UI Semibold") + 
  tm_shape(dat %>% filter(iso_a3=="USA")) +
  tm_fill("#003366") + 
  tm_shape(dat) + 
  tm_borders(col="grey")


# save as gif with time factor
tmap_animation(animation, filename="big_mac_anim.gif", delay=100, restart.delay=200,
               width=1200, height=600)
