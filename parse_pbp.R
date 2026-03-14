library(hoopR)
library(dplyr)
library(stringr)
library(tidyverse)

raw <- load_nba_pbp()
boxscore <- load_nba_player_box()

pbp <- raw |>
  filter(
    !(type_id %in% c(
      0,                    # unknown
      214, 215, 216, 277,   # coach challenge
      278, 279, 280         # referee challenge
    ))
  ) |>
  mutate(
    game_id = substring(game_id, 0, 9)
  ) |>
  select(
    game_id,                                                                        # game id to match with boxscore
    type_id, type_text, text, short_description,                                    # play description
    athlete_id_1, athlete_id_2, athlete_id_3,                                       # players involved
    team_id,                                                                        # team involved in play
    shooting_play, scoring_play, score_value, points_attempted,                     # play scoring info
    coordinate_x_raw, coordinate_y_raw,                                             # play coordinates if appropriate
    home_team_id, home_team_abbrev, away_team_id, away_team_abbrev,                 # team info
    home_score, away_score,                                                         # score info
    period_number, start_quarter_seconds_remaining, end_quarter_seconds_remaining   # clock info
  ) |>
  rename(
    play_team = team_id
  )
  
boxscore <- boxscore |>
  filter(
    !did_not_play
  )

game_pbp <- pbp |>
  filter(
    game_id == "401810429"
  )

game_boxscore <- boxscore |>
  filter(
    game_id == "401810429"
  )

athlete_team <- game_boxscore %>%
  select(athlete_id, team_id)

neo_game_pbp <- game_pbp |>
  left_join(athlete_team, by = c("athlete_id_1"="athlete_id")) |>
  rename(athlete_1_team = team_id) |>
  left_join(athlete_team, by = c("athlete_id_2"="athlete_id")) |>
  rename(athlete_2_team = team_id) |>
  left_join(athlete_team, by = c("athlete_id_3"="athlete_id")) |>
  rename(athlete_3_team = team_id) |>
  mutate(
    prev_foul = ifelse(str_detect(type_text, "Foul"), text, NA)
  ) |> 
  fill(prev_foul) |>
  mutate(
    is_defensive_rebound = type_text == "Defensive Rebound",
    is_turnover = str_detect(short_description, "Turnover"),
    is_and1_fg = scoring_play & (lead(type_text) == "Shooting Foul") & (athlete_1_team != lead(athlete_1_team)),
    is_made_fg_no_and1 = (score_value >= 2) & !is_and1_fg,
    is_free_throw = str_detect(type_text, "Free Throw"),
    is_final_free_throw = (type_id == 97) | (type_id == 99) | (type_id == 102), # 97 = 1/1, 99 = 2/2, 102 = 3/3
    is_made_final_free_throw = is_final_free_throw & scoring_play,
    keep_ball_after_ft = is_free_throw & scoring_play &
      ((type_id %in% c(
        103, # Technical
        105, # Flagrant 2/2
        106, # Flagrant 1/1
        108  # Clear path
      )) | (prev_foul == "Transition Take Foul")),
    possession_change = is_defensive_rebound | is_turnover | is_made_fg_no_and1 | (is_made_final_free_throw & !keep_ball_after_ft)
  )

home_starters <- game_boxscore |>
  filter(starter, home_away == "home") |>
  select(athlete_id) |>
  paste(sep = ",")

away_starters <- game_boxscore |>
  filter(starter, home_away == "away") |>
  select(athlete_id) |>
  paste(sep = ",")

neo_game_pbp_lineups <- neo_game_pbp |>
  mutate(
    home_lineup = accumulate(
      row_number(),
      ~ if ((type_text[.y] == "Substitution") & (play_team[.y] == home_team_id[.y])) {
        gsub(athlete_id_2[.y], athlete_id_1[.y], .x)
      } else {
        .x
      },
      .init = home_starters
    )[-1]
  ) |>
  mutate(
    away_lineup = accumulate(
      row_number(),
      ~ if ((type_text[.y] == "Substitution") & (play_team[.y] == away_team_id[.y])) {
        gsub(athlete_id_2[.y], athlete_id_1[.y], .x)
      } else {
        .x
      },
      .init = away_starters
    )[-1]
  )

neo_game_pbp_possession <- neo_game_pbp |>
  mutate(
    possession_change = lag(possession_change, default = first(possession_change))
  ) |>
  mutate(
    home_possession = accumulate(
      row_number(),
      ~ if (type_text[.y] == "End Period" & (period_number[.y] == 1 | period_number[.y] == 2)) { # end of second or third quarter
            athlete_3_team[1] != home_team_id[1]
        } else if (type_text[.y] == "Jumpball") {
            athlete_3_team[.y] == home_team_id[.y]
        } else if (possession_change[.y]) !.x else .x,
      .init = athlete_3_team[1] == home_team_id[1]
    )[-1]
  ) |>
  mutate(
    possession = ifelse(home_possession, home_team_abbrev, away_team_abbrev)
  ) |>
  select(
    text, possession
  )
