library(hoopR)
library(dplyr)
library(DBI)


schedule <- nba_schedule(season = most_recent_nba_season()-1)
  
regular_season_schedule <- schedule |>
  filter(
    season_type_description == "Regular Season"
  ) |>
  pull(game_id)

con <- dbConnect(RSQLite::SQLite(), "nba_pbp.sqlite")

while (length(new_games) > 0) {
  existing <- dbGetQuery(con, "SELECT DISTINCT game_id FROM pbp")$game_id
  
  new_games <- setdiff(regular_season_schedule, existing)
  
  pbp <- nba_pbps(
    game_ids = new_games[1:10],
    nest_data = FALSE
  )
  
  dbWriteTable(con, "pbp", as.data.frame(pbp), append = TRUE)
  
  print(new_games[1:10])
}

total_pbp <- dbGetQuery(con, "SELECT * FROM pbp")