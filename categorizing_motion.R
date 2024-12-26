# get ids for all plays with motion
motion_ids <- player_play |> 
  filter(motion_since_lineset == TRUE) |> 
  mutate(all_ids = paste(game_id, play_id, nfl_id),
         game_play_id = paste(game_id, play_id))

# count number of motions on each play
num_motions_per_play <- motion_ids |> 
  group_by(game_play_id) |> 
  summarise(n_motions = sum(motion_since_lineset, na.rm = TRUE))

# get tracking data for all motions
tracking_motions <- tracking |> 
  filter(all_ids %in% motion_ids$all_ids) |> 
  group_by(game_play_id) |> 
  mutate(
    frame_line_set = frame_id[which(event == "line_set")][1],
    frame_man_in_motion = frame_id[which(event == "man_in_motion")][1],
    frame_snap = frame_id[which(frame_type == "SNAP")][1]
  ) |> 
  ungroup() |> 
  left_join(num_motions_per_play)

# use man in motion event to track frame where motion starts
# this only works for plays with only one motion
plays_motion_tag <- tracking_motions |> 
  filter(n_motions == 1 & event == "man_in_motion") |> 
  group_by(game_id, play_id) |> 
  slice_min(frame_id) |> # 2022100205 3671: 2 man_in_motion events
  ungroup() |> 
  select(all_ids, game_play_id, frame_line_set, frame_motion = frame_id, frame_snap) |> 
  filter(frame_motion <= frame_snap)

# histogram to see at what percentage of their top speed players are 
# when man in motion event is captured
plays_motion_tag |>
  distinct(all_ids, frame_motion) |> 
  inner_join(tracking_motions) |> 
  group_by(all_ids) |> 
  mutate(max_s = max(s, na.rm = TRUE)) |>
  ungroup() |> 
  filter(frame_id == frame_motion) |> 
  mutate(frac_s = s / max_s) |> 
  # pull(frac_s) |> summary()
  ggplot(aes(frac_s)) +
  geom_histogram(bins = 30, fill = "gray", color = "gray40") +
  geom_vline(xintercept = 0.453, linetype = "dashed")

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2614  0.4693  0.4683  0.6721  1.0000 

# manually tag start and end frames for all motions on plays with more than one motion
tagging_motion <- tracking_motions |> 
  anti_join(select(plays_motion_tag, all_ids)) |> 
  filter(!is.na(frame_line_set)) |> 
  filter(frame_id > frame_line_set & frame_id < frame_snap) |> 
  group_by(all_ids) |>
  mutate(max_s = max(s, na.rm = TRUE)) |>
  ungroup() |>
  mutate(frac_s = s / max_s) |> 
  filter(frac_s > 0.4693) |>
  group_by(all_ids) |>
  slice_min(frame_id) |> 
  ungroup() |>
  select(all_ids, game_play_id, frame_line_set, frame_motion = frame_id, frame_snap)

# combine all tracking data for only the motion aspects of the plays
tagged_motion <- plays_motion_tag |> 
  bind_rows(tagging_motion) |>
  mutate(frames_between = frame_snap - frame_motion)

# get the quarterback for each play
qb_per_play <- player_play |> 
  filter(position == "QB") |> 
  group_by(game_play_id) |> 
  filter(n() == 1) |> #remove plays with multiple QBs (Taysom Hill)
  summarise(qb_id = first(nfl_id)) |> 
  ungroup() |> 
  mutate(all_ids = paste(game_play_id, qb_id))

qb_tracking <- tracking |> 
  inner_join(qb_per_play) |> 
  left_join(plays |> select(game_play_id, absolute_yardline_number)) |> 
  mutate(x_los = 120 - absolute_yardline_number,
         y_qb = y) |> 
  select(game_play_id, frame_id, x_los, y_qb)

motion_features <- tagged_motion |> 
  select(game_play_id, all_ids, frame_line_set, frame_motion, frame_snap) |> 
  pivot_longer(starts_with("frame_"), 
               values_to = "frame_id", 
               names_to = "frame_event",
               names_prefix = "frame_") |> 
  inner_join(tracking_motions) |> 
  select(game_play_id, frame_id, frame_event, x, y) |> 
  left_join(qb_tracking) |> 
  pivot_wider(id_cols = game_play_id,
              names_from = frame_event,
              values_from = c(frame_id, x, y, x_los, y_qb),
              values_fn = list) |> 
  unnest(cols = contains("_")) |> 
  mutate(x_center_before_snap = x_los_motion, 
         y_center_before_snap = y_qb_line_set, 
         y_change_start = abs(y_motion - y_center_before_snap),
         x_change_snap = abs(x_snap - x_center_before_snap),
         y_change_snap = abs(y_snap - y_center_before_snap)) |> 
  filter(!is.na(y_qb_line_set))

library(mclust)
set.seed(123)
motion_mclust <- motion_features |> 
  select(contains("_change_")) |> 
  Mclust(G = 2:6)

summary(motion_mclust)
motion_mclust |> 
  plot(what = "BIC")

# motion_mclust |> broom::tidy()
# motion_mclust |> broom::augment(motion_features)

motion_clusters <- motion_features |> 
  mutate(cluster = motion_mclust$classification)

# plot player trajectories for each cluster
motion_clusters |> 
  mutate(curve_id = row_number()) |> 
  select(game_play_id, frame_id_motion, frame_id_snap, cluster, curve_id) |> 
  left_join(tracking_motions) |> 
  mutate(frame_color = ifelse(frame_id == frame_id_motion, "red", NA),
         frame_color = ifelse(frame_id == frame_snap, "blue", frame_color)) |> 
  filter(frame_id >= frame_id_motion & frame_id <= frame_snap) |> 
  ggplot(aes(y, x, group = curve_id)) +
  geom_point(aes(color = I(frame_color)), size = 0.8, alpha = 0.5) +
  geom_path(alpha = 0.3) + 
  facet_wrap(~ cluster) +
  labs(x = "along the sideline", y = "yardline")
