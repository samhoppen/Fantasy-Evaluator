library(xgboost)

edp_model <- xgb.load("C:/Users/sphop/OneDrive/Betsperts/R/Fantasy-Evaluator/Code/EDP.model")

edp_model_mutations <- function(pbp){
  
  model_pbp <- pbp %>% 
    filter(!is.na(down), play_type != 'kickoff', !is.na(posteam)) %>% 
    group_by(game_id, posteam, drive) %>% 
    filter(sum(qb_kneel) == 0) %>% 
    ungroup() %>% 
    mutate(
           label = case_when(fixed_drive_result == "Touchdown" ~ 0,
                             fixed_drive_result == "Opp touchdown" ~ 1,
                             fixed_drive_result == "Field goal" ~ 2,
                             fixed_drive_result == "Safety" ~ 3,
                             fixed_drive_result %in% c("Punt", "Turnover", "Missed field goal", "End of half", "Turnover on downs") ~ 4),
           era0 = dplyr::if_else(.data$season <= 2001, 1, 0),
           era1 = dplyr::if_else(.data$season > 2001 & .data$season <= 2005, 1, 0),
           era2 = dplyr::if_else(.data$season > 2005 & .data$season <= 2013, 1, 0),
           era3 = dplyr::if_else(.data$season > 2013 & .data$season <= 2017, 1, 0),
           era4 = dplyr::if_else(.data$season > 2017, 1, 0),
           #for fg model, an era factor
           era = dplyr::case_when(
             .data$era0 == 1 ~ 0,
             .data$era1 == 1 ~ 1,
             .data$era2 == 1 ~ 2,
             .data$era3 == 1 | era4 == 1 ~ 3
           ),
           era = as.factor(.data$era),
           home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
           model_roof = dplyr::if_else(is.na(.data$roof) | .data$roof == 'open' | .data$roof == 'closed', as.character('retractable'), as.character(.data$roof)),
           model_roof = as.factor(.data$model_roof),
           retractable = dplyr::if_else(.data$model_roof == 'retractable', 1, 0),
           dome = dplyr::if_else(.data$model_roof == 'dome', 1, 0),
           outdoors = dplyr::if_else(.data$model_roof == 'outdoors', 1, 0)) %>% 
    dplyr::select(
      label,
      game_id,
      play_id,
      posteam,
      season,
      drive,
      half_seconds_remaining,
      down,
      yardline_100,
      home,
      retractable,
      dome,
      outdoors,
      ydstogo,
      era0, era1, era2, era3, era4,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining
    ) %>% 
    filter(!is.na(label))

  
  model_pbp[is.na(model_pbp)] <- 0
  
  
  return(model_pbp)
}


add_edp <- function(fastr_pbp) {
  
  
  fin_pbp <- edp_model_mutations(fastr_pbp)
  
  
  plays <- fin_pbp %>%
    dplyr::mutate(index = 1:dplyr::n())
  
  # user_message("Computing xyac...", "todo")
  
  join_data <- plays %>%
    dplyr::mutate(index = 1:dplyr::n()) #%>%
    # dplyr::mutate(
    #   original_pace = pace
    # ) %>%
    # dplyr::select("index":"pace", "original_pace", dplyr::everything())
  
  
  preds <- stats::predict(
    edp_model,
    # get rid of the things not needed for prediction here
    as.matrix(plays %>% dplyr::select(-c(season, posteam, game_id, play_id, drive, label, index)))
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename(edp = value) %>%
    dplyr::mutate(edp = round(edp, 4))%>%
    dplyr::bind_cols(
      tibble::tibble(
        "edp_label" = rep_len(0:4, length.out = nrow(plays) * 5),
        "index" = rep(plays$index, times = rep_len(5, length.out = nrow(plays)))
      ) %>%
        dplyr::left_join(join_data, by = "index")
    ) %>% 
    mutate(edp_value = case_when(edp_label == 0 ~ 6,
                                 edp_label == 1 ~ -6,
                                 edp_label == 2 ~ 3,
                                 edp_label == 3 ~ -2,
                                 edp_label == 4 ~ 0,
                                 TRUE ~ 0),
           act_points = case_when(label == 0 ~ 6,
                                  label == 1 ~ -6,
                                  label == 2 ~ 3,
                                  label == 3 ~ -2,
                                  label == 4 ~ 0,
                                  TRUE ~ 0))
  
  return(preds)
}

