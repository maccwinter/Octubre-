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

library(readxl)

st2 <- read_xlsx('SFBVEG2013.xlsx')


clean2 <- adply(.data = st2,.margins= 1, function(file) {st2
  
  
  
  head <- names(st2)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  
  names(st1) <- head 
  
  
})

st3 <- read_xlsx('pdwq.12.17.xlsx',1)
st3
names(st3)
head(st3)

clean3 <- adply(.data = st3,.margins= 1, function(file) {st3
  
  
  
  head <- names(st3)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  
  names(st3) <- head 
  
  
})
head(clean3)

st4 <-  read_xlsx('pdwq.12.16.xlsx', skip = 1)
names(st4)
clean4 <- adply(.data = st4,.margins= 1, function(file) {st4
  
  
  
  head <- names(st4)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  
  names(st4) <- head 
  
  
})
head(clean4)

st5 = read_xlsx('pdwq.12.10.xlsx', skip =1)
head(st5)

clean5 <- adply(.data = st5,.margins= 1, function(file) {st5
  
  
  
  head <- names(st5)
  head <- str_replace(head, "\\(.*\\)", "")
  head <- str_trim(head)
  head <- make.names(head)
  head <- tolower(head)
  head <- str_replace(head, fixed(".."), ".")
  
  names(st5) <- head 
  
  
})
head(clean5)


