#RotoGuru Scraper----------------------
library(XML)
library(RCurl)
library(plyr)
library(stringr)
library(tidyverse)
library(dplyr)

# create the days
x <- seq(as.Date("2020-07-30"), as.Date("2020-08-15"), by = "day")
twentyfourteen <- x[-which(x %in% as.Date(c("2019-11-28","2019-12-24","2020-02-14","2020-02-15","2020-02-16","2020-02-17","2020-02-18","2020-02-19")))]

x <- twentyfourteen
# create a url template for sprintf()
utmp <- "http://rotoguru1.com/cgi-bin/hyday.pl?game=dk&mon=%d&day=%d&year=%d"

# convert to numeric matrix after splitting for year, month, day
m <- do.call(rbind, lapply(strsplit(as.character(x), "-"), type.convert))
# create the list to hold the tables
tables <- vector("list", length(m))
# get the tables
for(i in seq_len(nrow(m))) {
  # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- if(url.exists(u <- sprintf(utmp, m[i, 2], m[i, 3], m[i, 1])))
    readHTMLTable(u, stringsAsFactors = FALSE,which=8,as.data.frame=TRUE)
  else NULL
  Sys.sleep(3)
}

# create the list to hold the dates
dates <- vector("list", length(tables))

# get the dates
for(i in seq_len(nrow(m))) {
  dates[[i]] <- if(url.exists(u <- sprintf(utmp, m[i,2],m[i,3],m[i,1])))
    x[i]
  else NULL
}

#put the data together
data <- do.call(rbind,Map(data.frame,A=tables, B=dates))
#data <- rbind(data,data2,data3,data4)
scores <- data %>% 
  group_by(Position) %>% 
  summarize(count = n())

#data2 <- data %>% 
#  filter(Position == "NA")
#Clean data
data <- data[!data$A.V1 %in% c("Unlisted","Guards","Centers","Forwards"),]
colnames(data) <- c("Position", "Player", "Points","Salary","Team","Opp","Score","Minutes","Stats","Date")
data <- data[complete.cases(data),]
data$Started <- ifelse(grepl("\\^",data$Player),"Yes","No")
data$Player <- gsub("\\^","",data$Player)
data$Score <- gsub("Â","",data$Score)
data$Score <- as.character(data$Score)
data$Stats <- gsub("Â","",data$Stats)
data$Points <- round(as.numeric(data$Points),1)
data$Home.Away <- ifelse(grepl("v ",data$Opp),"Home","Away")
data$Opp <- gsub("v ","",data$Opp)
data$Opp <- gsub("@ ","",data$Opp)
data <- data %>% 
  filter(Position != "NA") %>% 
  filter(Opp != "H") %>% 
  filter(Opp != "A")

data$TeamScore <- as.numeric(sapply(strsplit(data$Score,"\\-"),'[[',1))
data$OppScore <- as.numeric(sapply(strsplit(data$Score,"\\-"),'[[',2))
data$Win.Loss <- ifelse(data$TeamScore > data$OppScore,"Win","Loss")
data <- data[,-which(names(data) %in% c("Score"))]
data$Position[data$Position=="NA"] <- NA
data$Salary[data$Salary=="N/A"] <- NA
data <- data[complete.cases(data),]
data$Salary <- gsub("\\$","",data$Salary)
data$Salary <- gsub("\\,","",data$Salary)
data$Salary <- as.numeric(data$Salary)
data$Minutes <- as.numeric(sapply(strsplit(data$Minutes,"\\:"),'[[',1))
data$Minutes <- as.numeric(data$Minutes)
data$Team <- as.factor(data$Team)
data$Opp <- as.factor(data$Opp)
data$Started <- as.factor(data$Started)
data$Home.Away <- as.factor(data$Home.Away)
data$Win.Loss <- as.factor(data$Win.Loss)
data$Position <- as.factor(data$Position)
data <- data[complete.cases(data),]

#str(data)
#split Name into two columns
splits <- str_split_fixed(data$Player, ", ", 2)
#now merge these two columns the other way round
data$Player <- paste(splits[,2], splits[,1], sep = ' ')

write_csv(data, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/NBA DK Scoring 2020.csv")



## FANDUEL 
x <- seq(as.Date("2014-10-28"), as.Date("2015-04-15"), by = "day")
twentyfourteen <- x[-which(x %in% as.Date(c("2014-11-27","2014-12-24","2015-02-17","2015-02-16","2015-02-15","2015-02-14", "2015-02-13", "2015-02-18")))]

x <- twentyfourteen
# create a url template for sprintf()
utmp <- "http://rotoguru1.com/cgi-bin/hyday.pl?game=fd&mon=%d&day=%d&year=%d"

# convert to numeric matrix after splitting for year, month, day
m <- do.call(rbind, lapply(strsplit(as.character(x), "-"), type.convert))
# create the list to hold the tables
tables <- vector("list", length(m))
# get the tables
for(i in seq_len(nrow(m))) {
  # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- if(url.exists(u <- sprintf(utmp, m[i, 2], m[i, 3], m[i, 1])))
    readHTMLTable(u, stringsAsFactors = FALSE,which=8,as.data.frame=TRUE)
  else NULL
  Sys.sleep(3)
}

# create the list to hold the dates
dates <- vector("list", length(tables))

# get the dates
for(i in seq_len(nrow(m))) {
  dates[[i]] <- if(url.exists(u <- sprintf(utmp, m[i,2],m[i,3],m[i,1])))
    x[i]
  else NULL
}

#put the data together
data <- do.call(rbind,Map(data.frame,A=tables, B=dates))
#data <- rbind(data,data2,data3,data4)
scores <- data %>% 
  group_by(Score) %>% 
  dplyr::summarize(count = n())


data <- data[!data$A.V1 %in% c("Unlisted","Guards","Centers","Forwards"),]
colnames(data) <- c("Position", "Player", "Points","Salary","Team","Opp","Score","Minutes","Stats","Date")
data <- data[complete.cases(data),]
data$Started <- ifelse(grepl("\\^",data$Player),"Yes","No")
data$Player <- gsub("\\^","",data$Player)
data$Score <- gsub("Â","",data$Score)
data$Score <- as.character(data$Score)
data$Stats <- gsub("Â","",data$Stats)
data$Points <- round(as.numeric(data$Points),1)
data$Home.Away <- ifelse(grepl("v ",data$Opp),"Home","Away")
data$Opp <- gsub("v ","",data$Opp)
data$Opp <- gsub("@ ","",data$Opp)
data <- data %>% 
  filter(Position != "NA") %>% 
  filter(Opp != "H") %>% 
  filter(Opp != "A")

data$TeamScore <- as.numeric(sapply(strsplit(data$Score,"\\-"),'[[',1))
data$OppScore <- as.numeric(sapply(strsplit(data$Score,"\\-"),'[[',2))
data$Win.Loss <- ifelse(data$TeamScore > data$OppScore,"Win","Loss")
data <- data[,-which(names(data) %in% c("Score"))]
data$Position[data$Position=="NA"] <- NA
data$Salary[data$Salary=="N/A"] <- NA
data <- data[complete.cases(data),]
data$Salary <- gsub("\\$","",data$Salary)
data$Salary <- gsub("\\,","",data$Salary)
data$Salary <- as.numeric(data$Salary)
data$Minutes <- as.numeric(sapply(strsplit(data$Minutes,"\\:"),'[[',1))
data$Minutes <- as.numeric(data$Minutes)
data$Team <- as.factor(data$Team)
data$Opp <- as.factor(data$Opp)
data$Started <- as.factor(data$Started)
data$Home.Away <- as.factor(data$Home.Away)
data$Win.Loss <- as.factor(data$Win.Loss)
data$Position <- as.factor(data$Position)
data <- data[complete.cases(data),]

#str(data)
#split Name into two columns
splits <- str_split_fixed(data$Player, ", ", 2)
#now merge these two columns the other way round
data$Player <- paste(splits[,2], splits[,1], sep = ' ')

write_csv(data, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/NBA FD Scoring 2014.csv")
