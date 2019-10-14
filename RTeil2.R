#All the good stuff 
list.files(pattern=".txt")
#This finds the file that is a text file. 
#I found that long-ass ISIIS20140529124237281080643820473821907458390625478923489210778492018930217483902.txt file`

#To get the file read in. I will name it "o" 
o <- read.table(file='ISIIS201405291242.txt', sep = "\t", skip=10, header=TRUE, fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE, quote = "\"", check.names = FALSE, encoding="UTF-8", na.strings="9999.99")
#\t meas tab delimited. Skip=10 means skip first ten lines, header means we want a header lol. Don't worry about the rest... just copy it.
o
#It worked yay lol omg lmfao #hype #Igotthis #imbored #whatyadoin 

#What we're going to do is get r to report the date that this dataset ISII483921-7439-17493-7145890472 was collected 

date <- scan(file='ISIIS201405291242.txt', what="character", skip = 1, nlines=1, quiet=TRUE)
date
#OMG I asked R on a date and it gave me a time!! yay... but that date is in the past so I guess it's a no :/ #friendzoned
#The date is 05/29/14. I asked for the 2nd line (hence skip=1, and I onle wanted 1 line (nlines=1))
# I got this [1] "Date:"    "05/29/14"

library("stringr")
#downloaded library stringr 

#we're going to work with the str_sub function. Let's ask r what the arguments are. 
?str_sub

date <- date[2]

mm <- str_sub(string=date, start =1, end=2)
mm
#it gave me the month (05)
#for the day

dd <- str_sub(date, 4,5)
dd
#it gave me "29", which is the correct date 
#now time for year 
yy <-str_sub(date,7,8)
yy
#And the year was "14" which is correct 
#make object dateNextDay 
#str_c concatenates three things together,as shown below 
dd <-as.numeric(dd)
#just sett dd as s numeric value
dateNextDay <- str_c(mm,as.character(dd+1),yy, sep ="/")
dateNextDay
#I got 05/30/14, which is the date plus 1 day!
#Ok now we are looking at more time stuff in the dataset 
head(o)
str(o$Time)
#chr [1:6843] "12:42:32.78" "12:42:33.18" "12:42:33.72" "12:42:34.27" ...

o$hour <- as.numeric(str_sub(o$Time,1,2))
o$hour
#o$hour made new column with the values for hour (done above)
#so watch this there's a new column
head(o)
#Run that and you will see what I mean

o$date <- date
#added date column 
head(o$Time)

o$min <- as.numeric(str_sub(o$Time,4,5))
#Now we can add minute column 
o$min
head(o)
#now there is a minute column 
library(stringr)
library("stringr")
date
date <-date[2]
date

dd <- (str_sub(date, 4,5))
dd
dd <- as.numeric(dd)
dd
o$Time
str(o$Time)
o$hour <- as.numeric(str_sub(o$Time,1,2))
head(o)
o$minute <- as.numeric(str_sub(o$Time,4,5))
head(o)
str(o$Time)
o$second <- as.numeric(str_sub(o$Time, 7,11))
head(o)
str(o$Time)
head(o)
date <- str_c(mm,dd,yy, sep="/")
date
unique(yy)
unique(mm)
unique(dd)

o$date <-date
head(o)


o$time <- str_c(o$hour, o$minute, o$second, sep=":")

options("digits.secs"=3)
o$DateTime <- str_c(o$date, o$time, sep=" ")
head(o$DateTime)


o$DateTime <- as.POSIXct(strptime(x=o$DateTime, format = "%m/%d/%y %H:%M:%S", tz="America/New_York"))

head(o)
str(head(o))
str(head(o$DateTime))
o$dateTime <- o$dateTime - time.zone.change * 3600 
#Look into the above thing more... I zoned out when she explained it. 

#9/23/16 

#Load r data set with load("filename")

#for .txt file 
install.packages("data.table")
library(data.table)
d <- fread(input= "aurelia_15minCell_statareas.txt", sep =",", header = T, stringsAsFactors = F )
d

d <- read.csv(file = "aurelia_15minCell_statareas.txt", header = T, sep=",", stringsAsFactors = F)
d
d <- read.table(file= "aurelia_15minCell_statareas.txt", sep =",", stringsAsFactors = F)
install.packages("tidyverse")
library(tidyverse)
d4 <- read_csv(file="aurelia_15minCell_statareas.txt")
head(d4)


d3 <- load("aurelia_15minCell_statareas.Rdata")
head(d3)
a
d3
a
save(d3, file="aurelia_15minCell_statareas.txt")
d3
d3 <- a
d3
a
a<-1
a
d3
a
#excel files 
library(readxl)
e <- read_xlsx(path="Aurelia_SEAMAP_2012-2018_30minCell.xlsx", sheet =1, col_names = T)
e
library(readxl)
#Saving data as an ".Rdata" file type 
#save object of interest and write out the file name with the .Rdata file extension type.

save(d3, file ="aurelia_data.Rdata")


#Class 09/30/19 


load("fish_data (1).Rdata")
f <- fish
head(f)

#Subsetting
fdeep <- f[f$depth_fac == "Deep",]
head(fdeep)
fdeep2 <- subset(x=f, f$depth_fac == "Deep")
head(fdeep2)
library(dplyr)
fdeep3 <- filter(.data = f, depth_fac == "Deep")
head(fdeep3)
fd4 <- subset(x=f, f$depth_fac == "Deep", select = c("transect.id","area_fac"))
head(fd4)
fd5 <- f[which(f$depth_fac == "Deep" & f$area_fac =="East" & f$yr_fac !="2014"),]
head(fd5)

fshallow <- f[f$depth_fac == "Shallow",]
head(fshallow)
f2shallow <- subset(x=f, depth_fac == "Shallow")
feast <- subset(x=f, depth_fac == "East")
head(feast)
f2east <- f[f$depth_fac == "East",]
fpatches <- f[f$depth_fac == "Patches",]
f2patches <- subset(x=f, depth_fac == "Patches")


#subset & then combine using rowbind (rbind function)
d1 <- f[which(f$depth_fac == "Deep" & f$area_fac =="East"),]
d2 <- f[which(f$depth_fac == "Shallow" & f$area_fac =="West"),]
#combine d1 and d2 into a single data frame 
nrow(d1)
nrow(d2)

d3 <- rbind(d1,d2)

#combine data frames with separate columns into a single data frame. 
c1 <- subset(x=f, f$depth_fac == "Deep", select = c("transect.id","area_fac"))
c2 <- subset(x=f, f$depth_fac == "Deep", select = c("depth_fac","parcel.length.m","group"))

c3 <- cbind(c1, c2)
head(c3)


#only use column combine funtion if you know the #rows match up exactly, or else things will be missaligned, chaotic, and confused. 
#They got to line up!


#merging two data frames, ensuring that observationsfrom one data frame are aligned correctly with observations in second data frame. 

m1 <- subset(x=f, f$depth_fac == "Deep", select = c("transect.id","area_fac"))
m2 <- subset(x=f, f$depth_fac == "Deep", select = c("transect.id", "depth_fac","parcel.length.m","group"))
#We are adding a sequence so that later R will know how many rows each column has. 
#seq(dstart value, what is this sequence assigned to, by what value are subsequent values separated (intervals of 1, 0.5 etc.))
m1$seq <- seq(1, nrow(m1),1)
m2$seq <- seq(1,nrow(m2),1)

## merge so that values of longer data frame that don't correspond to those in shorter frame are converted to NA. 
#Meaning new data frame is as short as shortest data frame
mt <- merge(x=m1, y =m2, by=c("transect.id","seq"), all.x=T, no.dups=T)
nrow(mt)

mj <- right_join(x=m1,y=m2, by=c("transect.id","seq"))
nrow(mj)

#right join aligns x to y, and left join does vice versa. 

#what does cut function do? Check out below
v <-seq(5,20,0.5)
vc <- cut(x=v, breaks =seq(5,20,1), include.lowest = T)
v
vc

#10/2/19 

library(tidyverse)
install.packages("nutshell")
library(nutshell)

#data we will be using today 
data("batting.2008")
d <- batting.2008

#Why isn't github working???? 


#tapply --- (tidyverse function). Allows you to summarize a numeric vecor 

?tapply
#X is the dataframe (must be numeric vector)

str(d)
#find sum of all home runs
hr <- tapply(X = d$HR, INDEX = list(d$teamID), FUN=sum)
head(hr)

#Find quantile values for home runs by team. 
#fivenum gives you: min, lower-hinge, median, upper-hinge, and max value. Hinge is quantile? What is quantile? 

hr.q <- tapply(X = d$HR, INDEX = list(d$teamID), FUN=fivenum)
hr.q
#one category summarize 
lg.q <- tapply(X=(d$H/d$AB), INDEX = list(d$lgID), FUN=fivenum)
head(lg.q)
summarything <- summary(d[d$ligID == "AL",]$H/d[d$lgID == "AL",]$AB)
#This above is the same as something else abive 
#two category summary 
bats <- tapply(X=d$HR, INDEX=list(d$lgID,d$bats), FUN=mean)
bats
#Three category summary
bats.team <- tapply(X=d$HR, INDEX=list(d$lgID,d$teamID, d$bats), FUN=mean)
bats.team

#aggregate function ---- 
# if you put four dashes after something, as written above, r will remember it and put it in table of contents (upper right)
#Hey Mac ---- 
#Upper right of script! ---- 

team.stats.sum <- aggregate(x=d[,c("AB","H","BB","2B","HR")], by=list(d$teamID), FUN=sum)
team.stats.sum
team.stats.mean <- aggregate(x=d[,c("AB","H","BB","2B","HR")], by=list(d$teamID), FUN=mean)

#tidyverse summarise() ---- 
#This %>% passes every output from thing on left through function on right. 
team.sum <- d %>% group_by(teamID, lgID) %>% summarise(ABsum = sum(AB), ABmean = mean(AB), ABsd = sd(AB), ABcount = n())
head(team.sum)


team.sum$ABsum
#rowsum ---- 
#When you just want to add up the values in each row 

rs <- rowsum(d[,c("AB","HR","2B","3B")], group = d$teamID)
head(d[,c("AB","HR","2B","3B")])
rs

#counting variables ---- 
#use the function "tabulate"



HR.cnts <- tabulate(d$HR)
names(HR.cnts) <- 0:(length(HR.cnts) - 1)  
#names() function 
m <- matrix(nrow=4, ncol = 3)
colnames(m) <- c("one","two","three")
rownames(m) <- c("aplle","pear","orange","berry")

m


#table function ---- 

table(d$bats)
table(d[,c("bats","throws")])


#reshaping your data ---- 
n <- matrix(1:10, nrow=5)
n
t(n)
#t transposes n by making columns into rows 
#unstack and stack ---- 

s <- d[,c("lgID","teamID","HR","throws")]
head(s)
#Ok look into unpack function I guess... 
s.un
head(s.un)
install.packages("reshape2")
library(reshape2)
#melt & cast function to change data frame from the long to wide format
s.wide <- dcast(data=s, value.var = "HR", formula = "lgID" ~ "teamID", fun.aggregate = mean)
?dcast

library(tidyverse)
install.packages("nutshell")
library(nutshell)

#data we will be using today 
data("batting.2008")
d <- batting.2008

#Why isn't github working???? 


#tapply --- (tidyverse function). Allows you to summarize a numeric vecor 

?tapply
#X is the dataframe (must be numeric vector)

str(d)
#find sum of all home runs
hr <- tapply(X = d$HR, INDEX = list(d$teamID), FUN=sum)

#Find quantile values for home runs by team. 
#fivenum gives you: min, lower-hinge, median, upper-hinge, and max value. Hinge is quantile? What is quantile? 

hr.q <- tapply(X = d$HR, INDEX = list(d$teamID), FUN=fivenum)
#one category summarize 
lg.q <- tapply(X=(d$H/d$AB), INDEX = list(d$lgID), FUN=fivenum)
head(lg.q)
summarything <- summary(d[d$ligID == "AL",]$H/d[d$lgID == "AL",]$AB)
#This above is the same as something else abive 
#two category summary 
bats <- tapply(X=d$HR, INDEX=list(d$lgID,d$bats), FUN=mean)
#Three category summary
bats.team <- tapply(X=d$HR, INDEX=list(d$lgID,d$teamID, d$bats), FUN=mean)


#aggregate function ---- 
# if you put four dashes after something, as written above, r will remember it and put it in table of contents (upper right)
#Hey Mac ---- 
#Upper right of script! ---- 

team.stats.sum <- aggregate(x=d[,c("AB","H","BB","2B","HR")], by=list(d$teamID), FUN=sum)
team.stats.sum
team.stats.mean <- aggregate(x=d[,c("AB","H","BB","2B","HR")], by=list(d$teamID), FUN=mean)

#tidyverse summarise() ---- 
#This %>% passes every output from thing on left through function on right. 
team.sum <- d %>% group_by(teamID, lgID) %>% summarise(ABsum = sum(AB), ABmean = mean(AB), ABsd = sd(AB), ABcount = n())
head(team.sum)


team.sum$ABsum
#rowsum ---- 
#When you just want to add up the values in each row 

rs <- rowsum(d[,c("AB","HR","2B","3B")], group = d$teamID)
head(d[,c("AB","HR","2B","3B")])
rs

#counting variables ---- 
#use the function "tabulate"



HR.cnts <- tabulate(d$HR)
names(HR.cnts) <- 0:(length(HR.cnts) - 1)  
#names() function 
m <- matrix(nrow=4, ncol = 3)
colnames(m) <- c("one","two","three")
rownames(m) <- c("aplle","pear","orange","berry")

m


#table function ---- 

table(d$bats)
table(d[,c("bats","throws")])


#reshaping your data ---- 
n <- matrix(1:10, nrow=5)
n
t(n)
#t transposes n by making columns into rows 
#unstack and stack ---- 

s <- d[,c("lgID","teamID","HR","throws")]
head(s)
#Ok look into unpack function I guess... 
s.un
head(s.un)
install.packages("reshape2")
library(reshape2)
#melt & cast function to change data frame from the long to wide format
s.wide <- dcast(data=s, value.var = "HR", formula = "lgID" ~ "teamID", fun.aggregate = mean)
?dcast



#clas 10/9/19 ---- 

#d <- read.tanle('ISIIS201405291242.txt', sep = "\t", skip = 10, header = TRUE, fileEncoding = "ISO-8859", stringASFactirs #do we want to see data as factor data = FALSE, quate = "\"", check.names =FALSE, na.strings = "9999.9")
#date = scan('ISIIS2014052912.txt', what = "character", skip = 1, nlines =1, quiet=TRUE)
#Know how to use str_sub and str_c. 
#as.numeric is necessary to convert strings (which are always characters) to numeric values for math. 
#Can use - values in string function to get last values of a string. 
#Make date/time data usingPOSIXct and timezone specifications 
#use format function to change timezone. 

library(tidyverse)
library(batting)
c = aggregate(x=df$H, by =list(df$TeamID), FUN = sum)
#can also use tapply and group_by to do the same thing. 
# mean, median, quantile - gives you all 
#look up ?summarise and it will show you diff commands #quantile(d$object, 0.25) gives 25% quantile of that object. 


#merge dataframes 
q = data.frame(id=c('a','b','c','d'), num = c(1,2,3,4))
v = data.frame(id=c('a','b','e'), car =c(6,7,8))
#join with rbind, left_join, merge, etc... 
qv = merge(x=q,y=v, by = 'id')
qv
#by.x means x has same info but diff name. By.y is same but for y. 
#saying all includes all parts, but if no corresponding value, get NA

pv <- merge(x=q, y=v, by = 'id', all=T)
pv
px <- merge(x=q, y=v, by = 'id', all.x=T)

py <- merge(x=q, y=v, by = 'id', all.y=T)
py

#with tidyverse, use join 

#check out wrangling cheat sheet 

#melt and cast ---- 
library(reshape2)
library(nutshell)
m <- data("batting.2008")
m <- batting.2008
df <- m
m = melt(data=df, id.vars=c('teamID'), measure.vars = c('H','AB'))
m %>% group_by(teamID) %>% summarise(n=n())
cast = dcast(m, value ~ teamID, mean)
m
g <- df %>% gather('hits','amount', 16:20)
g
w = g %>% spread(hits,amount)
w
?dcast

?melt

head(d)


























