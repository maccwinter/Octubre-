library(tidyverse)
library(plyr)
library(dplyr)

load("data_sets/ost2014_phy_t.Robj")

#subset data frame to include only the following fields:
# [1] "cruise"      "transect.id" "haul"        "area"        "tow"         "region"      "dateTime"    "depth"
#[9] "temp"        "salinity"    "pressure"    "sw.density"  "fluoro"      "chl.ug.l"    "irradiance"  "lat"
#17] "lon"

fields <- names(phy_t[,c(1:15,20:21)])

d <- phy_t[,c(fields)]

#sort the data by transect.id and then for each transect so that the first observation is the most westward (data oriented west to east)
d <- arrange(d, transect.id, lon)

#for only the undulation ("und") transects, plot the geographic position (i.e. lat and lon) of each transect using ggplot + geom_point
## Label each transect's plot with the name of the transect as the title
## save these plots to a new directory named "und_coords"
dir.create("und_coords")

u <- d[d$tow == "und",]

ddply(.data = u, .variables = c("transect.id"), function(x){
  
  tr <- unique(x$transect.id)
  
  u.plot <- ggplot(data = x, aes(x = lat, y = lon)) +
    geom_point() +
    ggtitle(label = tr)
  
  ggsave(filename = paste0('und_coords/',tr,'.png'),
         plot = u.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")

#section 2 ----

#YOu can see from the transect.id values that there were three different studies in the "phy_t" data set: DVM, Eddy, and Spatial.
#For each of these studies, create a histogram of depth values. Use the 'facet' function in ggplot to plot the individual studies.

#identify the separate studies using a custom "if else" function and then loop over the rows of the data frame
#create an "if else" statement to each row to identify the study type using information
#contained in the 'transect.id' field

study.fxn <- function(x){
  
  t <- str_split_fixed(string = x[['transect.id']], pattern = "-", n = 3)
  s <- t[2]
  
  if(str_detect(string = s, pattern = "L")){
    study <- "lagrangian"
    
    #NOTE: the "fixed' function allows you to match the EXACT character string rather than only part of the string
  } else if(str_detect(string = s, pattern = fixed("Eddy"))){
    
    
  } else if(str_detect(string = s, pattern = "W")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "E")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "C")) {
    study <- "spatial"
    
  } else if(str_detect(string = s, pattern = "DVM")) {
    study <- "dvm"
    
  } else {}
  
  return(study)
  
}

#note: When I first wrote the function, I used "x$transect.id" and it worked when I ran it using
#the 'for loop'. However, when I ran it with the "apply" function, R gave me this error:

#"Error: $ operator is invalid for atomic vectors"

#To fix: I referred to the 'transect.id" field using the double bracket [["element"]] notation.
# i.e., 'x[[transect.id]]' in the function

#Option 1: using a 'for loop'; not this is not very efficient ----

d$study <- NA #create an empty field to pupulate the study type into; this step allows
#R to run more efficiently

# 'for loops' are slow for many processes. Here we can compare elasped time for the 'for loop' and 'apply' functions to do the same work

# Start the clock!
start <- proc.time()

# Run the loop
for(i in 1:nrow(d)){
  #get the 'ith" row, from 1 to the end of data frame "d" (i.e., 659008)
  d[i,]$study <- study.fxn(x = d[i,]) #apply the function to the input row of data
}

# Stop the clock!
time <- proc.time() - start; print(time)

#The values presented (user, system, and elapsed) will be defined by your operating system, but generally, the user time relates to the execution of the code,
#the system time relates to system processes such as opening and closing files, and the elapsed time is the difference in times since you started the stopwatch
# (and will be equal to the sum of user and system times if the chunk of code was run altogether and single-threaded).


#Option 2: using an "apply" function from the tidyverse package to assign the study type to each observation----

# Start the clock!
start <- proc.time()

d$study <- apply(X = d, MARGIN =  1, FUN = study.fxn)

# Stop the clock!
time <- proc.time() - start; print(time)

# ----

#check to make sure you have 4 unique values in the "study" field
unique(d$study)

#create a factor 'study field, order and label the the levels the way you want them to appear on the plot.
d$study_fac<- factor(x = d$study, levels = c("spatial","eddy","dvm","lagrangian"),
                     labels = c("Spatial","Eddy", "DVM","Lagrangian"))


#Make the plot using the field with the different study types
p <- ggplot(data = d, aes(x = depth)) + geom_histogram() + facet_wrap(.~study_fac) #this code answers the question
p #plot the plot.


p <- ggplot(data = d, aes(x = depth)) + geom_histogram(binwidth = 5, color="black", fill="white") + facet_wrap(.~study_fac) #if you wanted to be fancy
p #plot the plot.


#section 3 ----

#Calculate the average water temperaturefor the shallow, mid-depth, and deep transects in each of the western, central, and eastern regions.

#more verbose code
r <- group_by(.data = d, region)
w <- summarise(.data = r, avg.temp = mean(temp, na.rm = T)) #we need to add the argument 'na.rm = T' because there are NAs in the data. Use

#if you get an overall summary when you print 'w' instead of grouped summary, then you need to detach the plyr package
detach(package:plyr)

#more verbose code
r <- group_by(.data = d, region)
w <- summarise(.data = r, avg.temp = mean(temp, na.rm = T)) #we need to add the argument 'na.rm = T' because there are NAs in the data. Use

#less verbose code using piping syntax
w <- d %>% group_by(region) %>% summarise(avg.tempC = mean(temp, na.rm = T)) #we need to add the argument 'na.rm = T' because there are NAs in the data. Use
# the summary function on data frame 'd' to check for yourself

#Using a 'for loop', convert these average water temperature (currently in Celcius) to degrees Farenheit and Kelvin.

w$tempF <- NA
w$tempK <- NA

for(i in 1:nrow(w)){
  
  w[i,]$tempF <- w[i,]$avg.tempC * (9/5) + 32
  w[i,]$tempK <- w[i,]$avg.tempC + 273.15
  
}

#Generate grouped barplots using ggplot showing the average temperatures in Farenheit (plot 1) and Kelvin (plot 2). Using facetting to separate the plots by region
#(i.e. west, central, east)
library(reshape2)

#note: we are excluding the "sof' region from the data we are melting because we do not need to include it in the plot
wm <- melt(data = w[w$region != "sof",], id.vars = c("region"),measure.vars = c("tempF","tempK"))

p1 <- ggplot(data = wm, aes(x = variable, y=value)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(.~region)

#anthor way to display the data without facetting
p2 <- ggplot(data = wm, aes(fill = region, x = variable, y=value)) + geom_bar(stat = "identity", position = "dodge")