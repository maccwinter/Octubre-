#Test 3 
library(tidyverse)
library(plyr)
library(dplyr)

load('test3_data.rdata')
#Section 1 ----
#Question 1 ----
names(d)

# [1] "cruise"      "transect.id" "haul"        "area"        "tow"         "region"     
#[7] "dateTime"    "lat"         "lon"         "depth"       "temp"        "salinity"   
#[13] "pressure"    "sw.density"  "fluoro"      "oxygen"      "irradiance"  "region_fac" 
#[19] "study"    
names <- names(d)
names
subset <- d[,c(names)]
subset
#Question 2 ---- 
d1 <- arrange(subset, transect.id, dateTime)
head(d1)

#Section 2 ---- 
#Question 3 ---- 
dir.create("Figures")
#Question 4 ---- 

u <- d1[d1$tow == 'und',]
time <- u$dateTime
typeof(time)
dateTime1 <- as.POSIXct(time, format = "%Y-%m-%d %H:%M:%OS")
head(dateTime1)


ddply(.data = u, .variables = c("transect.id"), function(x){
  
  id <- unique(x$transect.id)
  
  u.plot <- ggplot(data = x, aes(x = dateTime, y = -depth)) +
    geom_point() +
    scale_x_continuous()
    xlab('hour:minute')
    ggtitle(label = id)
  
  ggsave(filename = paste0('Figures',id,'.png'),
         plot = u.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")




