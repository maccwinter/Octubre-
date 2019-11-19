#Test 3 
library(tidyverse)
library(plyr)
library(dplyr)

load('test3_data.rdata')
#Section 1 ----
#Question 1 ----
head(d)
names(d)
subset <- d[,c(names)]
head(subset)
#aren't all fields named? so I'm not subsetting anything... 
#Question 2 ---- 

d <- arrange(d, transect.id, dateTime)
head(d)

#Section 2 ---- 
#Question 3 ---- 
dir.create("Figures")
#Question 4 a - e ---- 

u <- d1[d1$tow == 'und',]

ddply(.data = u, .variables = c("transect.id"), function(x){
  
  id <- unique(x$transect.id)
  
  u.plot <- ggplot(data = x, aes(x = dateTime, y = -depth)) +
    geom_point(shape = 1, color = "#000066", size = 4.5) +
    scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M" ) +
    geom_smooth() + 
    ggtitle(label = id) 
  
  ggsave(filename = paste0('Figures',id,'.png'),
         plot = u.plot, width = 20, height = 10, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")

#Section 3 ----- 
#Question 5 ----





