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

u <- d[d$tow == 'und',]

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

#Section 3 ----- ??????? AHHHH!! 
#Question 5 ----

study.crr <- function(x){
  
  t <- str_split_fixed(string = x[['transect.id']], pattern = "-", n = 3)
  s <- t[,3]
if(str_detect(string = s, pattern = fixed("Eddy"))){
    
    
  } else if(str_detect(string = s, pattern = "W")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "E")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "C")) {
    study <- "spatial"
    
  } else {}
  
  return(study)
  
}


#Question 6 ---- 
d$study <- NA
for(i in 1:nrow(d)){d[i,]$study <- study.crr(x = d[i,])}
#question 7 ---- 
d$study <- apply(X = d, MARGIN =  1, FUN = study.crr)
#Section 4 ----
d$studEZ<- factor(x = d$study, levels = c("spatial","eddy"),
                     labels = c("Spatial","Eddy"))
#Question 8 
head(d)
spatial <- d[d$studEZ=='Spatial',]
head(spatial)

ggplot(data = spatial[spatial$region!='sof',], aes(x = pressure)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(.~region) 
  
head(spatial)

#Section 5 ---- 

 #Question 9 ----
detach(package:plyr)
tempstuff <- spatial %>% group_by(region, tow) %>% summarise(meanT = mean(temp, na.rm = T), tst.dev = sd(temp, na.rm=T), tst.dev2 = (sd(temp, na.rm=T)+mean(temp, na.rm=T)))
tempstuff

#Question 10 -----
tempstuff$Fahrenheit1 <- NA
tempstuff$Kelvin1 <- NA
tempstuff$Fahrenheit2 <- NA
tempstuff$Kelvin2 <- NA

for(i in 1:nrow(tempstuff)){
  
  tempstuff[i,]$Fahrenheit1 <- tempstuff[i,]$tst.dev * (9/5) + 32
  tempstuff[i,]$Kelvin1 <- tempstuff[i,]$tst.dev + 273.15
  tempstuff[i,]$Fahrenheit2 <- tempstuff[i,]$tst.dev2 * (9/5) + 32
  tempstuff[i,]$Kelvin2 <- tempstuff[i,]$tst.dev2 + 273.15
}
tempstuff1 <- tempstuff[tempstuff$tow !='d',]
tempstuff2 <- tempstuff1[tempstuff1$region !='sof',]
head(tempstuff2)
#Question 11 ---- 
library(reshape2)
snow <- melt(data = tempstuff2, id.vars = c("region","tow"),
             measure.vars = c("Fahrenheit1","Kelvin1",'Fahrenheit2','Kelvin2'))
snow
#Question 12 ----- 

ggplot(data = snow) + 
  geom_bar(aes(x=variable, y = value), position = 'dodge', stat = 'identity') + 
  facet_grid(tow~region) 
  





