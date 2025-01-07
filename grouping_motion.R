new_motion_labels |> 
  left_join(players |> 
              select(player_in_motion = nfl_id, position) |> 
              mutate(player_in_motion = as.character(player_in_motion))) |> 
  group_by(cluster, position) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(cluster) |> 
  mutate(percent = count / sum(count) * 100) |>  # Calculate percentage for each position in the cluster
  select(cluster, position, percent) |> 
  tidyr::pivot_wider(
    names_from = position, 
    values_from = percent, 
    values_fill = 0  # Fill missing combinations with 0
  )

library(gt)
library(gtExtras)
library(nflplotR)
motion_labels |> 
  inner_join(play_context) |> 
  group_by(posteam, cluster) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(cluster) |> 
  mutate(percent = count / sum(count) * 100) |> 
  select(cluster, posteam, percent) |> 
  filter(!is.na(cluster)) |> 
  arrange(-percent) |> 
  ungroup() |> 
  slice_head(n = 5) |> 
  mutate(cluster = c("5 - Over Shift", "6 - Jet Motion", "1 - Inward Shift",
                     "3 - Glide Motion", "3 - Glide Motion")) |> 
  mutate(Team = posteam,
         percent = round(percent, 2)/100) |> 
  select(Team, posteam, cluster, percent) |> 
  gt() |> 
  tab_header(title = md("**Most Commonly Used Presnap Movement Clusters by Team**"),
             subtitle = md("*Data from Weeks 1-9 of the 2022 NFL Season*")) |> 
  gt_nfl_logos("posteam", height = 40) |> 
  gtExtras::gt_theme_538() |> 
  data_color(columns = percent, palette = "RdYlGn", domain = c(0, .1)) |> 
  fmt_percent(columns = percent, decimals = 2) |> 
  cols_label(
    posteam = "",
    cluster = "Motion Cluster",
    percent = "Percent of Snaps Used",
  ) |> 
  cols_align(align = "center") |> 
  gt_add_divider(posteam, color = "black") |> 
  tab_style(
    style = cell_text(weight = "bold"),    # Apply bold text style
    locations = cells_body(columns = Team) # Specify the column to make bold
  ) |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = cluster)
  ) 

motion_labels |> 
  group_by(player_in_motion, cluster) |> 
  summarise(count = n()) |> 
  arrange(-count) |> 
  ungroup() |> 
  left_join(players |> 
              select(player_in_motion = nfl_id, display_name) |> 
              mutate(player_in_motion = as.character(player_in_motion)))

motion_labels |> 
  group_by(player_in_motion) |> 
  summarise(count = n()) |> 
  arrange(-count)|> 
  ungroup() |> 
  left_join(players |> 
              select(player_in_motion = nfl_id, display_name) |> 
              mutate(player_in_motion = as.character(player_in_motion)))
