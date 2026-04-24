library(hoopR)
library(dplyr)

LAST_FT = c("Free Throw 1 of 1", "Free Throw 2 of 2", "Free Throw 3 of 3")

raw_pbp <- nba_pbp(game_id = "0022501097")

raw_boxscore <- nba_boxscoretraditionalv3(game_id = "0022501097")

game_boxscore <- raw_boxscore$home_team_player_traditional |>
  as.data.frame() |>
  bind_rows(
    raw_boxscore$away_team_player_traditional |>
      as.data.frame()
  )

game_pbp <- raw_pbp[-1,] |>
  select(
    game_id,
    description,
    action_type, sub_type,
    minute_game,
    away_score, home_score,
    player1_id, player1_name, player1_team_id,
    player2_id, player2_name, player2_team_id,
    player3_id, player3_name, player3_team_id,
    is_field_goal, shot_value, shot_result,
    team_id, team_tricode
  ) |>
  filter(
    action_type != "Substitution",
    action_type != "Instant Replay",
    action_type != "Timeout"
  ) |>
  mutate(
    new_team_id = ifelse(team_id == 0, player1_id, team_id),
    home_team_id = first(game_boxscore$home_team_id),
    away_team_id = first(game_boxscore$away_team_id),
    
    is_defensive_rebound = (action_type == "Rebound") & (new_team_id != lag(player1_team_id)),
    is_turnover = (action_type == "Turnover"),
    is_made_fg = is_field_goal & (shot_result == "Made"),
    is_and1 = is_made_fg & (lead(sub_type == "Shooting") & lead(player1_team_id) != player1_team_id),
    is_made_fg_no_and1 = is_made_fg & !is_and1,
    is_final_ft = sub_type %in% LAST_FT,
    is_possession_end = is_defensive_rebound | is_turnover | is_made_fg_no_and1 | is_final_ft
  )

rebounds <- game_pbp |>
  select(
    description, is_defensive_rebound, is_turnover, is_made_fg_no_and1, is_final_ft
  )
