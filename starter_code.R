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
  library(sportyR)
  field_params <- list(field_apron = "springgreen3",
                       field_border = "springgreen3",
                       offensive_endzone = "springgreen3",
                       defensive_endzone = "springgreen3",
                       offensive_half = "springgreen3",
                       defensive_half = "springgreen3")
  nfl_field <- geom_football(league = "nfl",
                             display_range = "in_bounds_only",
                             x_trans = 60,
                             y_trans = 26.6667,
                             #xlims = c(40, 100),
                             xlims = c(20, 90),
                             color_updates = field_params)
  example_play <- tracking |> 
    filter(game_id == gameid, 
           play_id == playid) |> 
           # frame_type == "BEFORE_SNAP") |> 
    mutate(
      color = case_when(
        #display_name == "Taysom Hill" ~ "red",
        #club == "NO" ~ "gold",
        nfl_id == 54577 ~ "gold",
        club == "football" ~ "brown",
        # club == "BUF" ~ "blue",
        club == "CLE" ~ "#FF3C00",
        club == "CAR" ~ "#0085CA",
        club == "GB" ~ "#203731",
        club == "DET" ~ "#0076B6",
        club == "MIN" ~ "#4F2683",
        club == "NYG" ~ "#0B2265",
        TRUE ~ "white"
      )
    )
  caption_text <- plays |>
    filter(game_play_id == paste(gameid, playid)) |> 
    pull(play_description) |> str_replace("\\)\\.", "\\)")
  nfl_field +
    geom_point(data = example_play,
               aes(x, y, fill = color),
               size = 5,
               shape = 21,
               color = "black") +
    transition_time(example_play$frame_id) + 
    # labs(title = "<span style = 'color:#203731;'>**Green Bay Packers**</span> @ <span style = 'color:#4F2683;'>**Minnesota Vikings**</span>, 2022 NFL Week 1",
    #       subtitle = str_c("Q2: ", caption_text, "\n"),
    #      fill = "") +
    # labs(title = "<span style = 'color:#203731;'>**Green Bay Packers**</span> @ <span style = 'color:#0076B6;'>**Detroit Lions**</span>, 2022 NFL Week 9",
    #      subtitle = str_c("Q2: ", caption_text, "\n"),
    #      fill = "") +
    labs(title = "<span style = 'color:#0B2265;'>**New York Giants**</span> @ <span style = 'color:#203731;'>**Green Bay Packers**</span>, 2022 NFL Week 5",
         # subtitle = str_c("Q2: ", caption_text, "\n"),
         subtitle = str_c("Q4: ", caption_text, "\n"),
         fill = "") +
    # scale_fill_identity(guide = "legend", labels = c(
    #   "#203731" = "Packers",
    #   "#4F2683" = "Vikings",
    #   "gold" = "Randall Cobb",
    #   "brown" = "Football"
    # )) +
    # scale_fill_identity(guide = "legend", labels = c(
    #   "#203731" = "Packers",
    #   "#0076B6" = "Lions",
    #   "gold" = "Marcedes Lewis",
    #   "brown" = "Football"
    # )) +
    scale_fill_identity(guide = "legend", labels = c(
      "#203731" = "Packers",
      "#0B2265" = "Giants",
      "gold" = "Daniel Bellinger",
      "brown" = "Football"
    )) +
    theme(
      plot.title = ggtext::element_markdown(size = 15, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 10, face = "italic"),
      legend.position = "top"
    )
  # example_play |>
  #   ggplot(aes(x, y, fill = color)) +
  #   geom_point(size = 5, shape = 21, color = "black") +
  #   scale_fill_identity() +
  #   transition_time(frame_id)
}
play_visualization(2022100900, 3109)
anim_save("over_animation.gif", play_visualization(2022091101, 2501))

play_context |> filter(game_play_id == "2022110603 915") |> pull(desc)
