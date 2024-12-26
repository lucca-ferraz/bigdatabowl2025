library(tidyverse)
library(gganimate)
library(janitor)
library(cowplot)

# read in data
# tracking <- read_csv("tracking_week_1.csv") |>
#   bind_rows(read_csv("tracking_week_2.csv")) |>
#   bind_rows(read_csv("tracking_week_3.csv")) |>
#   bind_rows(read_csv("tracking_week_4.csv")) |>
#   bind_rows(read_csv("tracking_week_5.csv")) |>
#   bind_rows(read_csv("tracking_week_6.csv")) |>
#   bind_rows(read_csv("tracking_week_7.csv")) |>
#   bind_rows(read_csv("tracking_week_8.csv")) |>
#   bind_rows(read_csv("tracking_week_9.csv"))
# arrow::write_parquet(tracking, "tracking.parquet")

tracking <- arrow::read_parquet("tracking.parquet")
plays <- read_csv("plays.csv") |> clean_names()
players <- read_csv("players.csv") |> clean_names()
games <- read_csv("games.csv") |> clean_names()
player_play <- read_csv("player_play.csv") |> clean_names()

# data prep
player_play$game_play_id <- paste(player_play$game_id, player_play$play_id)
plays$game_play_id <- paste(plays$game_id, plays$play_id)
player_play <- player_play |> 
  left_join(plays |> select(game_play_id, possession_team)) |> 
  mutate(is_offense = ifelse(possession_team == team_abbr, 1, 0)) |> 
  select(-possession_team)
player_play <- player_play |> 
  left_join(players |> select(nfl_id, position))
# https://www.kaggle.com/competitions/nfl-big-data-bowl-2025/discussion/548627
player_play_offense <- player_play |> 
  filter(is_offense == 1) |> 
  mutate(
    in_motion_at_ball_snap = ifelse(is.na(in_motion_at_ball_snap), FALSE, in_motion_at_ball_snap),
    shift_since_lineset = ifelse(is.na(shift_since_lineset), FALSE, shift_since_lineset),
    motion_since_lineset = ifelse(is.na(motion_since_lineset), FALSE, motion_since_lineset)
  )
offense_personnel <- player_play |> 
  group_by(game_play_id) |> 
  summarise(rbs = sum(position == "RB" | position == "FB"),
            tes = sum(position == "TE")) |> 
  mutate(personnel = paste0(rbs, tes))
plays <- plays |> 
  left_join(offense_personnel |> select(game_play_id, personnel))
defense_personnel <- player_play |> 
  group_by(game_play_id) |> 
  summarise(dl = sum(position == "DT" | position == "NT" | position == "DE"),
            lbs = sum(position == "ILB" | position == "OLB" | position == "MLB" |
                        position == "LB"),
            dbs = sum(position == "CB" | position == "SS" | position == "FS" |
                        position == "DB")) |> 
  mutate(def_personnel = paste0(dl, "-", lbs, "-", dbs))
plays <- plays |> 
  left_join(defense_personnel |> select(game_play_id, def_personnel))


# standardize tracking data coordinates
tracking <- tracking |>
  mutate(
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

tracking <- tracking |> clean_names()
tracking <- tracking |> 
  mutate(all_ids = paste(game_id, play_id, nfl_id),
         game_play_id = paste(game_id, play_id))

# plotting function
play_visualization <- function(gameid, playid){
  example_play <- tracking |> 
    filter(game_id == gameid, 
           play_id == playid) |> 
    mutate(
      color = case_when(
        #display_name == "Taysom Hill" ~ "red",
        #club == "NO" ~ "gold",
        nfl_id == 47879 ~ "red",
        club == "football" ~ "brown",
        club == "BUF" ~ "blue",
        TRUE ~ "white"
      )
    )
  example_play |> 
    ggplot(aes(x, y, color = color)) +
    geom_point(size = 5) +
    scale_color_identity() +
    transition_time(frame_id)
}
play_visualization(2022090800, 122)