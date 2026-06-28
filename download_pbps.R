library(hoopR)
library(dplyr)
library(DBI)
library(progressr)

handlers(global = TRUE)
handlers("cli")

con <- dbConnect(RSQLite::SQLite(), "nba_pbp.sqlite")

total_games <- 0

for (i in 1996:2025) {
  print(paste("Now doing:", year_to_season(i)))
  
  schedule <- nba_schedule(season = i)

  
  regular_season_schedule <- schedule |>
    filter(
      season_type_description == "Regular Season"
    ) |>
    pull(game_id)
  
  
  existing <- dbGetQuery(con, "SELECT DISTINCT game_id FROM pbp")$game_id
  
  new_games <- setdiff(regular_season_schedule, existing)
  
  total_games <- total_games + length(regular_season_schedule)
  
  if (length(new_games) == 0){
    print("All games downloaded")
    next; 
  }
  
  print(paste(length(existing), "downloaded,", length(regular_season_schedule), "to do"))
  
  pbp <- nba_pbps(
    game_ids = new_games,
    nest_data = FALSE,
    on_court = FALSE
  )
  
  dbWriteTable(con, "pbp", as.data.frame(pbp), append = TRUE)
}



total_pbp <- dbGetQuery(con, "SELECT * FROM pbp")

games <- total_pbp |>
  group_by(game_id) |>
  count()

events <- total_pbp |>
  group_by(action_type, sub_type, player1_id, player1_name) |>
  count()
