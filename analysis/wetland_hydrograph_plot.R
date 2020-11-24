#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Plots
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 6/23/2020
#Purpose: Plot water level data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Workspace---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#load appropriate packages
library(patchwork)
library(lubridate)
library(tidyverse)

#load data
transect_depth<-read_csv("data//DepthToWaterTable.csv")
wetland_wL<-read_csv("data//waterLevel_cleaned.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Tidy Data --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clean transect data 
transect_wL<-transect_depth %>% 
  #Convert to waterLevel in cm
  mutate(waterLevel=-100*d_n) %>% 
  #select cols of interest
  select(day, station, wetland, waterLevel) %>% 
  #Select sites of interest
  filter(!str_detect(station, 'AIK')) 
  
#clean up wetland water level data
wetland_wL<-wetland_wL %>% 
  #definen station and wetland
  mutate(station = 'wetland_well',
         wetland = substr(site, 1,2), 
         waterLevel=100*waterLevel) %>% 
  #select cols of interest
  select(day, station, wetland, waterLevel)

#Only include wetlands of interest in wetland water level calc
woi<-transect_wL %>% select(wetland) %>% unique() %>% pull()
wetland_wL<-wetland_wL %>% filter(wetland %in% woi)

#Combine df
waterLevel<-bind_rows(transect_wL, wetland_wL)

#Select stations of interest
waterLevel<- waterLevel %>% 
  filter(!str_detect(station, 'AIK')) 
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Hydrologic Regime Plots-------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Hydrograph---------------------------------------------
df<-waterLevel %>% 
  #Group by date & loc
  group_by(day, station) %>% 
  #Summarise water level data
  summarise(
    mean = mean(waterLevel, na.rm = T),
    lwr    = mean - sd(waterLevel, na.rm = T)/sqrt(n()), 
    upr    = mean + sd(waterLevel, na.rm = T)/sqrt(n()))

#Subset by Hydrologic Zone
wetland<- df %>% filter(station == 'wetland_well')
a <- df %>% filter(station == 'SC-A')
b <- df %>% filter(station == 'SC-B')
c <- df %>% filter(station == 'SC-C')
d <- df %>% filter(station == 'SC-D')
e <- df %>% filter(station == "SC-E")

#Define ribbon tranparency
ribbon_alpha<-0.90

#Define colors
cols<-tibble("Wetland" = '#045a8d',
             'A' = '#2b8cbe',
             'B' = '#74a9cf',
             'C' = '#a6bddb',
             'D' = '#d0d1e6',
             'E' = '#f1eef6')
line_col<-"grey50"


#Start ggplot
hyd<-ggplot() +
  #D
  geom_ribbon(
    aes(ymin = d$lwr, ymax = d$upr, x = d$day, fill='D'),
    alpha=ribbon_alpha) +
  geom_line(
    aes(x=d$day, y=d$mean), 
    col=line_col) +
  #E
  geom_ribbon(aes(ymin = e$lwr, ymax = e$upr, x = e$day, fill='E'), 
              col='grey90', lwd=0.25) +
  geom_line(aes(x=e$day, y=e$mean), 
            col=line_col) +
  
  #C
  geom_ribbon(aes(ymin = c$lwr, ymax = c$upr, x = c$day, fill='C'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=c$day, y=c$mean), 
            col=line_col) +
  #B
  geom_ribbon(aes(ymin = b$lwr, ymax = b$upr, x = b$day, fill="B"), 
              alpha=ribbon_alpha) +
  geom_line(aes(x=b$day, y=b$mean), 
            col=line_col) +
  #A
  geom_ribbon(aes(ymin = a$lwr, ymax = a$upr, x = a$day, fill='A'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=a$day, y=a$mean), 
            col=line_col) +
  #Wetland
  geom_ribbon(aes(ymin = wetland$lwr, ymax = wetland$upr, x = wetland$day, fill='Wetland'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=wetland$day, y=wetland$mean), 
            col=line_col) +
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  #Clip to water year
  coord_cartesian(xlim=as.Date(c("2017-10-01", "2018-09-30"))) +
  #theme options
  theme_bw() + 
    ylab("Water Level [cm]") + 
    xlab(NULL) + 
    scale_x_date(date_labels = "%b") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 10),
          legend.position = c("right"), 
          legend.title = element_text(size=10), 
          legend.text  = element_text(size=10))+
  guides(fill=guide_legend(nrow=6, byrow=T))

#2.5 Print plot---------------------------------------------
tiff("docs/hydrograph.tif", res=300, width = 5, height = 3, units = 'in')
hyd 
dev.off()

png("docs/hydrograph.png", res=300, width = 5, height = 3, units = 'in')
hyd 
dev.off()

pdf("docs/hydrograph.pdf", width = 5, height = 3)
hyd 
dev.off()

