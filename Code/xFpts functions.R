made_model_mutations <- function(pbp) {
  
  pbp <- pbp %>%
    dplyr::mutate(
      #for EP, CP, and WP model, xgb needs 0/1 for eras
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
      down1 = dplyr::if_else(.data$down == 1, 1, 0),
      down2 = dplyr::if_else(.data$down == 2, 1, 0),
      down3 = dplyr::if_else(.data$down == 3, 1, 0),
      down4 = dplyr::if_else(.data$down == 4, 1, 0),
      home = dplyr::if_else(.data$posteam == .data$home_team, 1, 0),
      model_roof = dplyr::if_else(is.na(.data$roof) | .data$roof == 'open' | .data$roof == 'closed', as.character('retractable'), as.character(.data$roof)),
      model_roof = as.factor(.data$model_roof),
      retractable = dplyr::if_else(.data$model_roof == 'retractable', 1, 0),
      dome = dplyr::if_else(.data$model_roof == 'dome', 1, 0),
      outdoors = dplyr::if_else(.data$model_roof == 'outdoors', 1, 0)
    )
  
  return(pbp)
}


prepare_xyac_data <- function(pbp) {
  
  # valid pass play: at least -15 air yards, less than 70 air yards, has intended receiver, has pass location
  passes <- pbp %>%
    make_model_mutations() %>%
    dplyr::mutate(
      # receiver_player_name =
      #   stringr::str_extract(desc, glue::glue('{receiver_finder}{big_parser}')),
      pass_middle = dplyr::if_else(pass_location == "middle", 1, 0),
      air_is_zero = dplyr::if_else(air_yards == 0, 1, 0),
      distance_to_sticks = air_yards - ydstogo,
      distance_to_goal = yardline_100 - air_yards,
      valid_pass = dplyr::if_else(
        (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
          !is.na(air_yards) & air_yards >= -15 & air_yards < 70 &
          !is.na(receiver_player_name) & !is.na(pass_location),
        1, 0
      )
    )
  return(passes)
}




add_xyac <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    # user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    # testing only
    # pbp <- g
    
    # pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))
    
    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      dplyr::filter(valid_pass == 1, distance_to_goal != 0)
    
    if (!nrow(passes) == 0) {
      # user_message("Computing xyac...", "todo")
      join_data <- passes %>%
        dplyr::select(
          "index", "distance_to_goal", "season", "week", "home_team", "posteam", "roof",
          "half_seconds_remaining", "down", "ydstogo",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining",
          "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
        ) %>%
        dplyr::mutate(
          down = as.integer(down),
          ydstogo = as.integer(ydstogo),
          original_ydstogo = ydstogo
        ) %>%
        dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())
      
      xyac_vars <-
        stats::predict(
          fastrmodels::xyac_model,
          as.matrix(passes %>% xyac_model_select())
        ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(prob = "value") %>%
        dplyr::bind_cols(
          tibble::tibble(
            "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
            "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
          ) %>%
            dplyr::left_join(join_data, by = "index") %>%
            dplyr::mutate(
              half_seconds_remaining = dplyr::if_else(
                half_seconds_remaining <= 6,
                0,
                half_seconds_remaining - 6
              )
            )
        ) %>%
        dplyr::group_by(index) %>%
        dplyr::mutate(
          max_loss = dplyr::if_else(distance_to_goal < 95, -5, distance_to_goal - 99),
          max_gain = dplyr::if_else(distance_to_goal > 70, 70, distance_to_goal),
          cum_prob = cumsum(prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            yac == max_loss ~ cum_prob,
            # same for gains bigger than possible
            yac == max_gain ~ 1 - dplyr::lag(cum_prob, 1),
            TRUE ~ prob
          ),
          # get updated end result for each possibility
          yardline_100 = distance_to_goal - yac
        ) %>%
        # dplyr::filter(yac >= max_loss, yac <= max_gain) %>%
        # dplyr::select(-cum_prob) %>%
        dplyr::mutate(
          posteam_timeouts_pre = posteam_timeouts_remaining,
          defeam_timeouts_pre = defteam_timeouts_remaining,
          gain = original_spot - yardline_100,
          turnover = dplyr::if_else(down == 4 & gain < ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(gain >= ydstogo, 1, down + 1),
          ydstogo = dplyr::if_else(gain >= ydstogo, 10, ydstogo - gain),
          # possession change if 4th down failed
          down = dplyr::if_else(turnover == 1, as.integer(1), as.integer(down)),
          ydstogo = dplyr::if_else(turnover == 1, as.integer(10), as.integer(ydstogo)),
          # save yardline_100 for yards gained calculation
          yardline_100_noflip = yardline_100,
          # flip yardline_100 and timeouts for turnovers for EP calculation
          yardline_100 = dplyr::if_else(turnover == 1, as.integer(100 - yardline_100), as.integer(yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(
            turnover == 1,
            defeam_timeouts_pre,
            posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            turnover == 1,
            posteam_timeouts_pre,
            defeam_timeouts_pre
          ),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(ydstogo >= yardline_100, as.integer(yardline_100), as.integer(ydstogo))
        )
      
      
      pbp <- pbp %>%
        dplyr::left_join(xyac_vars, by = "index") %>%
        dplyr::select(-index)
      
      # message_completed("added xyac variables", ...)
    } else { # means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-index)
      # user_message("No non-NA values for xyac calculation detected. xyac variables set to NA", "info")
    }
  }
  
  return(pbp)
}
