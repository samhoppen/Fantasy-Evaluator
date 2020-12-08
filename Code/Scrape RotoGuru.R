library(tidyverse)
library(dplyr)
library(rvest)
library(reshape)

#import game data
games_2020 <- read_csv(url("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv"))
games_20202 <- read_csv(url("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")) %>% 
  mutate(home_team = away_team)
games <- rbind(games_2020, games_20202) %>% 
  select(-c("away_team", "game_id", "game_type", "gameday", "weekday",
            "gametime", "location", "result", "overtime", "old_game_id",
            "away_rest", "home_rest", "div_game", "roof", "surface",
            "temp", "wind", "away_coach", "home_coach", "referee",
            "stadium_id", "stadium"))

games$home_team <- sub("OAK", "LV", games$home_team)
games$home_team <- sub("STL", "LA", games$home_team)
games$home_team <- sub("SD", "LAC", games$home_team)


## DraftKings Scrape
i <- 1
while (i <= 17){
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
                                                                                 'Team','H/A','Oppt','FPTS','DKSalary')))
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

dk_weekly_scores$Team <- toupper(dk_weekly_scores$Team)
dk_weekly_scores$Oppt <- toupper(dk_weekly_scores$Oppt)

dk_weekly_scores$Team <- sub("NWE", "NE", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("GNB", "GB", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("NOR", "NO", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("OAK", "LV", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("KAN", "KC", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("LVR", "LV", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("SFO", "SF", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("TAM", "TB", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("LAR", "LA", dk_weekly_scores$Team)
dk_weekly_scores$Team <- sub("JAC", "JAX", dk_weekly_scores$Team)

dk_weekly_scores$Oppt <- sub("NWE", "NE", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("GNB", "GB", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("NOR", "NO", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("OAK", "LV", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("KAN", "KC", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("LVR", "LV", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("SFO", "SF", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("TAM", "TB", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("LAR", "LA", dk_weekly_scores$Oppt)
dk_weekly_scores$Oppt <- sub("JAC", "JAX", dk_weekly_scores$Oppt)

dk_weekly_scores$year <- as.numeric(dk_weekly_scores$year)
dk_weekly_scores$week <- as.numeric(dk_weekly_scores$week)

dk_weekly_scores2 <- dk_weekly_scores %>% 
  mutate(year = 2020) %>% 
  left_join(games,
            by = c("year" = "season",
                   "week" = "week",
                   "Team" = "home_team")) %>% 
  mutate(result = if_else(home_score == away_score, "tie",
                          if_else(H.A == "h" & home_score >= away_score, "win",
                                  if_else(H.A == "a" & away_score >= home_score, "win", "loss"))))

write_csv(dk_weekly_scores2, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/2020 DraftKings Weekly Scores.csv")



## FanDuel Scrape
i <- 1
while (i <= 17){
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
                                                                                 'Team','H/A','Oppt','FPTS','FDSalary')))
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

fd_weekly_scores$Team <- toupper(fd_weekly_scores$Team)
fd_weekly_scores$Oppt <- toupper(fd_weekly_scores$Oppt)

fd_weekly_scores$Team <- sub("NWE", "NE", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("GNB", "GB", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("NOR", "NO", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("OAK", "LV", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("KAN", "KC", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("LVR", "LV", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("SFO", "SF", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("TAM", "TB", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("LAR", "LA", fd_weekly_scores$Team)
fd_weekly_scores$Team <- sub("JAC", "JAX", fd_weekly_scores$Team)

fd_weekly_scores$Oppt <- sub("NWE", "NE", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("GNB", "GB", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("NOR", "NO", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("OAK", "LV", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("KAN", "KC", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("LVR", "LV", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("SFO", "SF", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("TAM", "TB", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("LAR", "LA", fd_weekly_scores$Oppt)
fd_weekly_scores$Oppt <- sub("JAC", "JAX", fd_weekly_scores$Oppt)

fd_weekly_scores$year <- as.numeric(fd_weekly_scores$year)
fd_weekly_scores$week <- as.numeric(fd_weekly_scores$week)

fd_weekly_scores2 <- fd_weekly_scores %>% 
  mutate(year = 2020) %>% 
  left_join(games,
            by = c("year" = "season",
                   "week" = "week",
                   "Team" = "home_team")) %>% 
  mutate(result = if_else(home_score == away_score, "tie",
                          if_else(H.A == "h" & home_score >= away_score, "win",
                                  if_else(H.A == "a" & away_score >= home_score, "win", "loss"))))

write_csv(fd_weekly_scores2, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/2020 FanDuel Weekly Scores.csv")



## add 2020 to total data
dk_weekly_scores_2014 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2014%20DraftKings%20Weekly%20Scores.csv"))
dk_weekly_scores_2015 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2015%20DraftKings%20Weekly%20Scores.csv"))
dk_weekly_scores_2016 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2016%20DraftKings%20Weekly%20Scores.csv"))
dk_weekly_scores_2017 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2017%20DraftKings%20Weekly%20Scores.csv"))
dk_weekly_scores_2018 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2018%20DraftKings%20Weekly%20Scores.csv"))
dk_weekly_scores_2019 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2019%20DraftKings%20Weekly%20Scores.csv"))
dk_weekly_scores_2020 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2020%20DraftKings%20Weekly%20Scores.csv"))

dk_weekly_scores_tot <- rbind(dk_weekly_scores_2014,
                              dk_weekly_scores_2015,
                              dk_weekly_scores_2016,
                              dk_weekly_scores_2017,
                              dk_weekly_scores_2018,
                              dk_weekly_scores_2019,
                              dk_weekly_scores_2020)

write_csv(dk_weekly_scores_tot, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/DraftKings Weekly Scores Total.csv")


fd_weekly_scores_2014 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2014%20FanDuel%20Weekly%20Scores.csv"))
fd_weekly_scores_2015 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2015%20FanDuel%20Weekly%20Scores.csv"))
fd_weekly_scores_2016 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2016%20FanDuel%20Weekly%20Scores.csv"))
fd_weekly_scores_2017 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2017%20FanDuel%20Weekly%20Scores.csv"))
fd_weekly_scores_2018 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2018%20FanDuel%20Weekly%20Scores.csv"))
fd_weekly_scores_2019 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2019%20FanDuel%20Weekly%20Scores.csv"))
fd_weekly_scores_2020 <- read_csv(url("https://raw.githubusercontent.com/samhoppen/NFL-Analysis/main/Data/2020%20FanDuel%20Weekly%20Scores.csv"))

fd_weekly_scores_tot <- rbind(fd_weekly_scores_2014,
                              fd_weekly_scores_2015,
                              fd_weekly_scores_2016,
                              fd_weekly_scores_2017,
                              fd_weekly_scores_2018,
                              fd_weekly_scores_2019,
                              fd_weekly_scores2)

write_csv(fd_weekly_scores_tot, "C:/Users/Hoppy/OneDrive/NFL Analysis/NFL-Analysis/Data/FanDuel Weekly Scores Total.csv")
