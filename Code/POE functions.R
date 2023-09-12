library(stringi)
library(xgboost)

poe_model <- xgb.load("C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/Code/POE.model")


poe_model_mutations <- function(pbp){
  pbp$start_seconds <- sapply(pbp$time, function(x) {
    parts <- as.integer(unlist(strsplit(x, ":")))
    return(parts[1] * 60 + parts[2])
  })
  pace_pbp <- pbp %>% 
    filter(pass == 1 | rush == 1) %>% 
    mutate(change_of_poss = if_else(lag(posteam) == posteam, 0, 1),
           oob_stoppage = case_when(out_of_bounds == 1 & (half_seconds_remaining <= 120 | game_seconds_remaining <= 300) ~ 1,
                                    TRUE ~ 0),
           prev_start = lag(start_seconds),
           time_left = if_else(time == "15:00" | half_seconds_remaining == 120, 1, 0),
           two_min_warning = if_else(120 <= prev_start & 120 >= start_seconds & (qtr == 2 | qtr == 4) & time_left == 0 & timeout == 0, 1, 0),
           prev_kickoff = if_else(lag(play_type) == "kickoff", 1, 0),
           prev_inc_pass = lag(incomplete_pass),
           prev_oob = lag(oob_stoppage),
           prev_to = if_else(lag(timeout) == 1, 1, 0),
           play_type = case_when(penalty == 1 & grepl("enforced", desc, ignore.case = T) ~ "no_play",
                                 TRUE ~ play_type),
           prev_td = if_else(lag(touchdown) == 1, 1, 0),
           prev_no_play = if_else(lag(play_type) == "no_play", 1, 0),
           prev_yds_gained = lag(yards_gained),
           clock_stopped = if_else(play_type %in% c("kickoff", "extra_point") | prev_td == 1 | extra_point_attempt ==1 | two_point_attempt == 1 | two_min_warning == 1 | prev_no_play == 1 | time_left == 1 | prev_kickoff == 1 | change_of_poss == 1 | prev_oob == 1 | prev_inc_pass == 1 | prev_to == 1, 1, 0),
           diff_time_ratio = score_differential*exp(4*((3600-game_seconds_remaining)/3600))
    )  #%>% 
#    mutate(change_of_poss = if_else(lag(drive_play_id_started) == drive_play_id_started, 0, 1),
#           oob_stoppage = case_when(out_of_bounds == 1 & (half_seconds_remaining <= 120 | game_seconds_remaining <= 300) ~ 1,
#                                    TRUE ~ 0),
#           prev_inc_pass = lag(incomplete_pass),
#           prev_oob = lag(oob_stoppage),
#           prev_to = if_else(lag(home_timeouts_remaining) != home_timeouts_remaining | lag(away_timeouts_remaining) != away_timeouts_remaining, 1, 0),
#           prev_yds_gained = lag(yards_gained),
#           clock_stopped = if_else(change_of_poss == 1 | prev_oob == 1 | prev_inc_pass == 1 | prev_to == 1, 1, 0),
#           diff_time_ratio = score_differential*exp(4*((3600-game_seconds_remaining)/3600)))
  
  
  pace_pbp$play_clock <- as.numeric(pace_pbp$play_clock)
  
  model_cols <- c('season', 'posteam', 'game_id', 'play_id', 'play_clock', 'pace', 'wp',
                  'down', 'half_seconds_remaining',
                  'game_seconds_remaining', 'score_differential', 'diff_time_ratio',
                  'prev_yds_gained') #'no_huddle',
  
  model_pbp <- pace_pbp %>% 
    mutate(pace = 40-play_clock) %>% 
    filter(clock_stopped == 0, !is.na(down), play_clock <= 40) %>% 
    select(all_of(model_cols))
  
  model_pbp[is.na(model_pbp)] <- 0
  
  
  return(model_pbp)
}


add_poe <- function(fastr_pbp) {
  
  
  fin_pbp <- poe_model_mutations(fastr_pbp)

  
  plays <- fin_pbp %>%
    dplyr::mutate(index = 1:dplyr::n())
  
  # user_message("Computing xyac...", "todo")
  
  join_data <- plays %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%
    dplyr::mutate(
      original_pace = pace
    ) %>%
    dplyr::select("index":"pace", "original_pace", dplyr::everything())
  
  
  preds <- stats::predict(
    poe_model,
    # get rid of the things not needed for prediction here
    as.matrix(plays %>% dplyr::select(-c(season, posteam, game_id, play_id, wp,
                                  play_clock, pace, index)))
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(exp_pace = value) %>%
    dplyr::mutate(exp_pace = round(exp_pace, 4))%>%
    dplyr::bind_cols(
      tibble::tibble(
        "sec_per_play" = rep_len(0:40, length.out = nrow(plays) * 41),
        "index" = rep(plays$index, times = rep_len(41, length.out = nrow(plays)))
      ) %>%
        dplyr::left_join(join_data, by = "index")
    )
  
  return(preds)
}

