# Homework 5 

#Import and 'clean up' 5 data sets to R. 

library(tidyverse)
library(plyr)

st1 <- read_csv('sampling_stations.csv')



clean1 <- adply(.data = st1,.margins= 1, function(file) {st1
                                                                            


head <- names(st1)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")

names(st1) <- head 


})

head(st1)


