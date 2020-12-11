#RotoGuru Scraper----------------------
library(XML)
library(RCurl)
library(plyr)
library(stringr)

#Scraper (if RotoGuru doesn't have data, tables will be NULL, date will Work)
# create the days
x <- seq(as.Date("2015-10-27"), as.Date("2016-04-13"), by = "day")
#twentyfifteen <- x[ -which(x %in% as.Date(c("2015-11-26","2015-12-24")))] 
twentyfourteen <- x[-which(x %in% as.Date(c("2015-11-26","2015-12-24","2016-02-12","2016-02-13","2016-02-14","2016-02-15","2016-02-16","2016-02-17")))]

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

#Clean data
data <- data[!data$A.V1 %in% c("Unlisted","Guards","Centers","Forwards"),]
colnames(data) <- c("Position", "Player", "Points","Salary","Team","Opp","Score","Minutes","Stats","Date")
data <- data[complete.cases(data),]
data$Started <- ifelse(grepl("\\^",data$Player),"Yes","No")
data$Player <- gsub("\\^","",data$Player)
data$Score <- gsub("Â","",data$Score)
data$Stats <- gsub("Â","",data$Stats)
data$Points <- round(as.numeric(data$Points),1)
data$Home.Away <- ifelse(grepl("v ",data$Opp),"Home","Away")
data$Opp <- gsub("v ","",data$Opp)
data$Opp <- gsub("@ ","",data$Opp)
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

write_csv(data, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/NBA DK Scoring 2014.csv")
