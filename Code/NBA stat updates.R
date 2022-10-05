library(dplyr)
library(tidyverse)
library(nbastatR)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

data <- nbastatR::game_logs(seasons = 2022,
                            season_types = "Regular Season")

data$namePlayer <- sub("^(\\S*\\s+\\S+).*", "\\1", data$namePlayer)
data$namePlayer <- gsub("[.]", "", data$namePlayer)

final_data <- data %>% 
  dplyr::select(-c(yearSeason, nameTeam, idPlayer, slugSeason, slugLeague, typeSeason, numberGameTeamSeason, idTeam, fgm, fga,fg2m, fg2a,minutes, ftm, fta,
                   plusminus, fpts, urlPlayerStats, urlPlayerThumbnail, urlPlayerActionPhoto, urlPlayerPhoto, urlTeamSeasonLogo,
                   isB2B, isB2BFirst, isB2BSecond, slugMatchup, countDaysRestTeam, countDaysNextGameTeam,
                   slugTeamWinner, slugTeamLoser, outcomeGame, numberGamePlayerSeason, countDaysRestPlayer,
                   countDaysNextGamePlayer, isWin, pctFG, pctFG3, pctFT, hasVideo, pctFG2,
                   oreb, dreb, pf, idGame)) %>% 
  dplyr::rename(points = pts,
                rebounds = treb,
                assists = ast,
                `3-pointers` = fg3m,
                turnovers = tov,
                blocks = blk,
                steals = stl,
                Opp = slugOpponent)


final_data$slugTeam <- sub("SAS", "SA", final_data$slugTeam)
final_data$Opp <- sub("SAS", "SA", final_data$Opp)

write_csv(final_data, "C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/Data/NBA Player Stats 2021.csv")

# team_logos <- final_data %>% 
#   select(slugTeam, urlTeamSeasonLogo) %>% 
#   unique()
# 
# write_csv(team_logos, "C:/Users/Hoppy/OneDrive/NFL Analysis/Fantasy-Evaluator/Data/NBA Team Logos.csv")
# info <- nbastatR::teams_seasons_info(seasons = 2021)
# 
# bradley <- projections %>% 
#   filter(Players == "Tony Bradley")
