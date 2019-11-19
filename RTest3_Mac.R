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
#Question 4 ---- 

u <- d1[d1$tow == 'und',]
thyme <- u$dateTime
range(thyme)

ddply(.data = u, .variables = c("transect.id"), function(x){
  
  id <- unique(x$transect.id)
  
  p.plot <- ggplot(data = x, aes(x = dateTime, y = -depth)) +
    geom_point() +
    scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M" )
  
    ggtitle(label = id)
  
  ggsave(filename = paste0('Figures',id,'.png'),
         plot = p.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")




