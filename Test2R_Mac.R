#Test 2!!! 
library(nutshell)
library(stringr)
#Question 1 ---- 
load('fish_data (1).Rdata')
f
head(f)
#Question 2 ----
date <- f$parcel.start.time
head(date)
y <- str_sub(date,1,4)
y
m <- str_sub(date,6,7)
head(m)
d <- str_sub(date, 9,10)
head(d)
#Question 3 ---- 
date1 <- str_c(y,m,d, sep = "/")
head(date1)
#Question 4  ---- 
f$Date <- date1
head(f)
f$Date <- as.POSIXct(f$Date)
head(f)
#Question 5 
f$Date <- as.POSIXct(f$Date, tz = "America/New_York")
head(f)

#Section 2 ---- 
#Question 6 ---- 
load("aurelia_15minCell_statareas.Rdata")
#Question 7 ---- 
library(readxl)
read_xlsx('Aurelia_SEAMAP_2012-2018_30minCell.xlsx')
#Question8 
library(data.table)
#a ---- 
fread(input="aurelia_15minCell_statareas.txt", sep =",", stringsAsFactors = F)
#b----
read.csv(file ="aurelia_15minCell_statareas.txt", header = T, sep = ",", stringsAsFactors = F)
#c---- 
read.table(file="aurelia_15minCell_statareas.txt", sep = ",", stringsAsFactors = F)
#d ----
library("tidyverse")
read.csv("aurelia_15minCell_statareas.txt")
#9?---- 
jelly <- fread(input="aurelia_15minCell_statareas.txt", sep =",", stringsAsFactors = F)
head(jelly)
jelly <- jelly[jelly$year == "2012",]
head(jelly)
?save
jelly <- save(jelly, file ="jelly.Rdata")
jelly

#Section 3 ---- 
#Question 10 ?---- 
load("test2_deep.Rdata")
head(deep)
load("test2_mid.Rdata")
head(mid)
load("test2_shallow.Rdata")
head(shallow)
library(dplyr)
all <- rbind(c(deep,mid,shallow))


#Question 11 ---- 
load("t2-2.Rdata")
load("test2_data.Rdata")

tall <- left_join(t2,t2.1, by ="transect.id")
head(tall)

#Question 12 ---- 

tall1 <- merge(t2,t2.1, by ="transect.id")
head(tall1)

#Question 13 ---- 
library(nutshell)
b<-batting.2008
#Question 14 ---- 
sumhr <- tapply(X=b$HR, INDEX =list(b$teamID), FUN = sum)
head(sumhr)
#Question 15 ---- 

total = aggregate(x=b[,c("AB","H","BB","2B","HR")], by =list(b$teamID), FUN=sum)

#Question 16 ---- 
tmsd <- b %>% group_by(teamID) %>% summarise(sumHR =sum(HR), meanHR = mean(HR),sdevHR = sd(AB))
tmsd






