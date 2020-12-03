library(tidyverse)
library(dplyr)
library(rvest)
library(reshape)


## DraftKings Scrape
i <- 1
while (i <= 16){
  url1 <- "http://rotoguru1.com/cgi-bin/fyday.pl?week="
  url2 <- "&game=dk&scsv=1"
  url <- paste(url1,i,url2,sep="")
  webpage <- read_html(url)
  roto_guru_html <- webpage %>%
    html_nodes("pre") %>%
    html_text() %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    .[. != ""]
  roto_guru_html <- as.data.frame(roto_guru_html)
  roto_guru_html = transform(roto_guru_html, roto_guru_html = colsplit(roto_guru_html, split = "\\;",
                                                                       names = c('week', 'year','GID','player','Pos',
                                                                                 'Team','H/A','Oppt','DKP','DKSalary')))
  roto_guru_html <- roto_guru_html[-1,] 
  paste0("roto_guru_html",i)
  assign(paste("week",i,"DFS",sep=""), roto_guru_html)
  i <- i + 1}
roto_guru_html <- NULL

dk_weekly_scores <- rbind(week1DFS,week2DFS,week3DFS,week4DFS,week5DFS,week6DFS,week7DFS,week8DFS,week9DFS,week10DFS,week11DFS,
                          week12DFS,week13DFS,week14DFS,week15DFS,week16DFS)
dsts <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/RotoGuru%20DSTs.csv"))

dk_weekly_scores <- dk_weekly_scores %>% 
  separate(player, c("Last", "First"), ", ") %>% 
  left_join(dsts,
            by = c("Team" = "team")) %>% 
  mutate(player = if_else(Pos == "Def",
                          def,
                          paste0(First, " ", Last))) %>% 
  select(-c("def", "First", "Last", "GID"))

dk_weekly_scores$player <- sub("^(\\S*\\s+\\S+).*", "\\1", dk_weekly_scores$player)
dk_weekly_scores$player <- gsub("[.]", "", dk_weekly_scores$player)

write_csv(dk_weekly_scores, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/2020 DraftKings Weekly Scores.csv")



## FanDuel Scrape
i <- 1
while (i <= 16){
  url1 <- "http://rotoguru1.com/cgi-bin/fyday.pl?week="
  url2 <- "&game=fd&scsv=1"
  url <- paste(url1,i,url2,sep="")
  webpage <- read_html(url)
  roto_guru_html <- webpage %>%
    html_nodes("pre") %>%
    html_text() %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    .[. != ""]
  roto_guru_html <- as.data.frame(roto_guru_html)
  roto_guru_html = transform(roto_guru_html, roto_guru_html = colsplit(roto_guru_html, split = "\\;",
                                                                       names = c('week', 'year','GID','player','Pos',
                                                                                 'Team','H/A','Oppt','DKP','DKSalary')))
  roto_guru_html <- roto_guru_html[-1,] 
  paste0("roto_guru_html",i)
  assign(paste("week",i,"DFS",sep=""), roto_guru_html)
  i <- i + 1}
roto_guru_html <- NULL

fd_weekly_scores <- rbind(week1DFS,week2DFS,week3DFS,week4DFS,week5DFS,week6DFS,week7DFS,week8DFS,week9DFS,week10DFS,week11DFS,
                          week12DFS,week13DFS,week14DFS,week15DFS,week16DFS)
dsts <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/RotoGuru%20DSTs.csv"))

fd_weekly_scores <- fd_weekly_scores %>% 
  separate(player, c("Last", "First"), ", ") %>% 
  left_join(dsts,
            by = c("Team" = "team")) %>% 
  mutate(player = if_else(Pos == "Def",
                          def,
                          paste0(First, " ", Last))) %>% 
  select(-c("def", "First", "Last", "GID"))

fd_weekly_scores$player <- sub("^(\\S*\\s+\\S+).*", "\\1", fd_weekly_scores$player)
fd_weekly_scores$player <- gsub("[.]", "", fd_weekly_scores$player)

write_csv(fd_weekly_scores, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/2020 FanDuel Weekly Scores.csv")
