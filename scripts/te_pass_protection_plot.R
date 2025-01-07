play_context <- nflreadr::load_pbp(2022) |> 
  mutate(game_id = as.numeric(old_game_id),
         play_id = play_id,
         game_play_id = paste(game_id, play_id))

pass_plays <- play_context |> filter(play_type == "pass")

pass_player_play_tes <- player_play |> 
  filter(game_play_id %in% pass_plays$game_play_id) |> 
  filter(position == "TE")

sum(!is.na(pass_player_play_tes$pressure_allowed_as_blocker)) / length(pass_player_play_tes$blocked_player_nfl_id1)

block_tendencies <- pass_player_play_tes |> 
  group_by(team_abbr) |> 
  summarise(plays = n(), blocks = sum(!is.na(pressure_allowed_as_blocker)),
            te_block_pct = blocks / plays)

pass_player_play_def <- player_play |> 
  filter(game_play_id %in% pass_plays$game_play_id) |> 
  filter(is_offense != 1)

pressure_per_play <- pass_player_play_def |> 
  group_by(game_play_id) |> 
  summarise(num_pressures = sum(caused_pressure),
            pressure = ifelse(num_pressures > 0, TRUE, FALSE)) |> 
  select(-num_pressures) |> 
  ungroup()

pressure_stats <- plays |> 
  left_join(pressure_per_play) |> 
  filter(!is.na(pressure)) |> 
  group_by(possession_team) |> 
  summarise(plays = n(), pressures = sum(pressure), pressure_rate_allowed = pressures / plays) |> 
  rename(team_abbr = possession_team) |> 
  ungroup()

plot_data <- block_tendencies |> 
  left_join(pressure_stats, by = "team_abbr") |> 
  select(team_abbr, te_block_pct, pressure_rate_allowed)

library(nflplotR)
plot_data |> 
  ggplot(aes(pressure_rate_allowed, te_block_pct)) +
  geom_point(alpha = 0) +
  ggthemes::theme_clean() +
  geom_nfl_logos(aes(team_abbr = team_abbr, width = 0.075, height = 0.1)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Does Having the Tight End Block Aid in Pass Protection?",
       subtitle = "TE Block Rate on Pass Plays vs Pressure Rate Allowed",
       y = "% of Plays Where the Tight End Blocked",
       x = "Pressure Rate Allowed")
