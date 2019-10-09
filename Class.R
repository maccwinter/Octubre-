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
g <- df %>% group_by(teamID)%>%gather(key = 'hits', 'amount', 'H':'HR')
g















