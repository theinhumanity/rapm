library(hoopR)
library(dplyr)
library(DBI)
library(progressr)

handlers(global = TRUE)
handlers("cli")

con <- dbConnect(RSQLite::SQLite(), "nba_boxscore.sqlite")

for (i in rev(1946:2025)) {
  
  print(paste("Now doing:", year_to_season(i)))
  
  schedule <- nba_schedule(season = i)
  
  
  regular_season_schedule <- schedule |>
    filter(
      season_type_description == "Regular Season"
    ) |>
    pull(game_id)
  
  
  
  existing <- dbGetQuery(con, "SELECT DISTINCT game_id FROM boxscores")$game_id
  
  new_games <- setdiff(regular_season_schedule, existing)
  
  if (length(new_games) == 0){
    print("All games downloaded")
    next; 
  }
  
  print(paste(length(existing), "downloaded,", length(regular_season_schedule), "to do"))
  
  dbBegin(con)
  
  with_progress({
    p <- progressor(along = new_games)
    
    for (game_id in new_games) {
      
      boxscores <- nba_boxscoretraditionalv3(game_id = game_id)
      
      home_boxscore <- boxscores$home_team_player_traditional |>
        filter(player_slug != "")
      away_boxscore <- boxscores$away_team_player_traditional |>
        filter(player_slug != "")
      
      dbWriteTable(con, "boxscores", as.data.frame(home_boxscore), append = TRUE)
      dbWriteTable(con, "boxscores", as.data.frame(away_boxscore), append = TRUE)
      
      p(sprintf("Processed %s", game_id))
    }
  })
  
  dbCommit(con)
  
}

all_boxscores <- dbGetQuery(con, "SELECT * FROM boxscores")
games_processed <- all_boxscores |>
  group_by(comment) |>
  count()
