##Done with WINDOWS PC

library(gapminder)
library(ggplot2)
library(dplyr)
library(maps)
library(WDI)
library(ggthemes)
library(readxl)
install.packages("hrbrthemes")
library(hrbrthemes)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(tidyverse)
library(readxl)

##Density Map
install.packages("rgdal")
library(rgdal)

setwd("~/Density Map Data")
shp=readOGR(dsn=".", layer="IDN_adm1")
plot(shp)
IndoMap = fortify(shp)
ggplot()+
  geom_polygon(data = IndoMap, aes(long, lat, group = group))+
  theme_bw()

PopDensity <- read.csv("Kepadatan Penduduk menurut Provinsi.csv")
PopDensity <- PopDensity %>%
  mutate(id = as.numeric(id),
         `density` = as.numeric(`density`))
View(PopDensity)
View(IndoMap)
IndoMap <- IndoMap %>%
  mutate(id = as.numeric(id))
mapdata <- left_join(IndoMap, PopDensity, by = "id")

densitymap <- ggplot(mapdata, aes(x = long, y = lat, group=group))+
  geom_polygon(aes(fill = `density`), color = "gray80")+
  scale_fill_gradient(name = "Share of People per Sq. km", low = "green", high = "red")+
  theme_bw()+
  labs(title = "Population Density in Indonesia",
       subtitle = "Per province on 2019", caption = "Source: Badan Pusat Statistik")+
  theme_map()+  annotate(geom = "text", x = 106.837, y=-6.22462,
                         label = "DKI Jakarta", hjust = 1, size=3)
densitymap