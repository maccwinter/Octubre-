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

#Class 10/16 ---- 


#Loops! 
#Allows you to split data, perform different functions on different 
#parts of a dataset, and then put it all together

library(plyr)

#going to use ddply 
#.progress tells you if theres error

d <- fish[fish$depth_fac == "Deep",]
#ddply ---- 
?ddply
nd <- ddply(.data=fish, .variables="depth_fac", function(x){

z <- unique(x$depth_fac)



depth_condition <- function(y){ 
  if(y=="Deep") q <- 50 
  else if(y=="Mid") q <- 25 
  else q <- 15}

x$depth_z <- depth_condition(y=z)

return(x)

}, .inform=T, .progress = "text")
nd
#adply ---- 
#batch data gives us a list of files 
batch_data <- list.files("batch_data", full = TRUE, pattern = "ISIIS")
batch_data
?adply
phy <- adply(.data=batch_data,.margins= 1, function(file) {da <- read.table(batch_data[1], 
                                                             sep = "\t", skip = 10, 
                                                             header = TRUE, fileEncoding = "ISO-8859-1", 
                                                             stringsAsFactors = FALSE, quote = "\"", 
                                                             check.names=FALSE, encoding="UTF-8", 
                                                             na.strings = "9999.99")

#clean names 
head <- names(d)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")
#assign names
names(d) <- head 

date <- scan(batch_data[1], what = "character", skip = 1, nlines=1, quiet=TRUE)
d$date <- date[2]
d$dateTime <- str_c(d$date, d$time, sep = " ")
d$dateTime <- as.POSIXct(strptime(d$dateTime, format = "%m/%d/%y %H:%M:%OS", tz = "America/New_York"))
return(d)
})


#class 10/28/19 ---- 

#plotting 

library(ggplot2)
load("fish_data (1).Rdata")

#one geom: 
data('economics')
e <- economics
head(e)

unemploy <- ggplot(data=e, aes(x=date, y=unemploy)) + geom_line()
unemploy

#multiple geoms 

data("presidential")
pres <- presidential

caption <-paste(strwrap("Unemployment rates in the U.S. have varied a lot over the years",40), collapse = "\n")
yrng <- range(e$unemploy)
xrng <- range(e$date)
date <- as.Date("1960-01-01")
#This stuff above is for making and positioning the caption in the graph. 

ggplot(e) + 
  geom_line(aes(x=date, y=unemploy))+
  geom_rect(data=pres, aes(xmin=start, xmax=end, fill=party), ymin =-Inf, ymax = Inf, alpha = 0.2) +
  scale_fill_manual(values=c("dodgerblue", "firebrick3")) +
  geom_vline(data=pres, aes(xintercept=as.numeric(start)), colour="grey50", alpha =0.5) +
  #geom_text(data=pres, aes(x=start, y=2500, label=name), size=3, vjust=0, hjust=0, nudge_x=50) 
annotate("text", x=date, y=yrng[2], label=caption, hjust=0, vjust=1, size=4)

#making fancy-ass graph. 
#bar graph challenge 
#stack bar with grouped bar 

load("fish_data (1).Rdata")
library(tidyverse)
fs <- fish %>% group_by(area_fac, depth_fac, yr_fac) %>% summarise(parcel.count = length(parcel.id))
head(fs)
ggplot(fs) +
  geom_bar(aes(x=area_fac, y=parcel.count, fill = depth_fac), position="stack", stat="identity") +
  facet_grid(.~yr_fac)
#could but .~ afterwords for landscape orientation 

#another way to format the graph 


ggplot(fs) +
  geom_bar(aes(x=area_fac, y=parcel.count, fill = depth_fac), position="dodge", stat="identity") +
  facet_grid(.~yr_fac)

#or

ggplot(fs) +
  geom_bar(aes(x=yr_fac, y=parcel.count, fill = depth_fac), position="stack", stat="identity") +
  facet_wrap(~area_fac)

#10/30/19 ---- 

load('fish_data (1).Rdata')
library(tidyverse)

ggplot(fish, aes(parcel.length.m, parcel.density.m3, color = depth_fac)) +
  geom_point() +
  facet_wrap(~depth_fac)
library(plyr)

ddply(.data=fish, .variables = "depth_fac", function(x){
  name = unique(x$depth_fac)
  pl =ggplot(fish, aes(parcel.length.m, parcel.density.m3)) +
    geom_point()+
    xlab('Parcel length (m)') +
    ylab(expression(paste('Parcel Density (',m^3,')')))+
    ggtitle(name)
  ggsave(filename= paste0(name,'.tiff'),plot=pl, width =4, height=3, units='in',
         dpi=600, compression = 'lzw')
}, .progress = "text")


ddply(.data=fish, .variables = 'transect.id', function(x){
  name=unique(x$transect.id)
  pl =ggplot(fish, aes(parcel.length.m, parcel.density.m3))+
    geom_point()+
    xlab('Parcel length(m')+
    ylab(expression(paste('Parcel Density (',m^3,')')))+
    ggtitle(name)
  ggsave(filename= paste0(name,'.tiff'),plot=pl, width =4, height=3, units='in',
         dpi=600, compression = 'lzw')
  
}, .progress ='text')


# plotting 3 variables! :O 
data('mtcars')
names(mtcars)

ggplot(mtcars, aes(vs,cyl,fill=mpg))+
  geom_tile()

ggplot(mtcars, aes(wt, mpg))+
  geom_point(aes(color =hp))+
  scale_color_continuous(type='viridis')+
  theme_bw()


library(tidyverse)
install.packages('ggmap')
install.packages("osmdata")
library(ggmap)
library(osmdata)


#downloading maps 
LA = getbb('Louisiana')
LA
#Louisiana terrain map
map = get_stamenmap(bbox = LA, zoom = 8, maptype = 'terrain')
ggmap(map)
 #LA toner 
map.toner = get_stamenmap(bbox = LA, zoom = 8, maptype = 'toner-background')
ggmap(map.toner)

#Ohio watercolor map 
map.OH = get_stamenmap(bbox = getbb('ohio'), zoom = 8, map = 'toner-lite')
ggmap(map.OH)
map.NM = get_stamenmap(bbox = getbb('New Mexico'), zoom = 8, map = 'terrain')
ggmap(map.NM)

getbb('Venice Italy')
getbb('Lafayette LA')
map.LAf <- get_stamenmap(bbox = getbb('Lafayette Louisiana'), zoom = 15, map = 'terrain')
ggmap(map.LAf)
#Using manual values for bbox 
bbox = c(left = 20, bottom = 0, right = 30, top = 20)
map.r = get_stamenmap(bbox = bbox, zoom = 6, map = 'terrain')
ggmap(map.r)

#puttin points on a map 

df = read_csv('LDWF2008seine.csv')
bb <- c(left=min(df$lon), bottom = min(df$lat), right = max(df$lon), top = max(df$lat))
bb

la.map = get_stamenmap(bbox = bb, zoom = 8, map = 'terrain-background')
ggmap(la.map) + 
  geom_point(data = df, aes(x=lon, y = lat))

bb1 <- c(left=min(df$lon-0.2), bottom = min(df$lat-0.2), right = max(df$lon+0.2), top = max(df$lat+0.2))
bb
#adding color to points 
la.map1 = get_stamenmap(bbox = bb1, zoom = 8, map = 'terrain-background')
ggmap(la.map1) + 
  geom_point(data = df, aes(x=lon, y = lat, color = basin)) + 
  scale_color_manual(values=c('purple', 'blue', 'orange', 'green', 'yellow', '#d9381e'))

# large mouth bass with size by abundance 
ggmap(la.map) + 
  geom_point(data = df, aes(x=lon, y = lat)) +
  geom_point(data = df[df$species == "Largemouth Bass",], 
             aes(x=lon, y=lat), color = 'red')
# plot menhaden by abundance 
n = df %>% group_by(species) %>% summarise(n=n())
d = n[order(-n$n),]

ggmap(la.map) + 
  geom_point(data = df[df$species == 'Gulf Menhaden',],
             aes(x=lon, y = lat, size = num))

#black drum 

ggmap(la.map) + 
  geom_point(data = df[df$species == 'Black Drum',],
             aes(x=lon, y = lat, size = num, color = num)) +
  scale_color_gradientn(colors = terrain.colors(5))









