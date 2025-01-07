motion_graph <- tracking |> 
  filter(game_play_id == "2022091112 917") |> 
  filter(nfl_id == 37139) |> 
  #filter(frame_type != "AFTER_SNAP") |> 
  mutate(h_distance = x - 92.75,
         v_distance = 24.03 - y) |> 
  select(v_distance, h_distance, frame_id) |> 
  pivot_longer(cols = c(v_distance, h_distance), 
               names_to = "type", 
               values_to = "distance") |> 
  ggplot(aes(frame_id, distance, color = type)) +
  geom_line() +
  ggthemes::theme_clean() +
  transition_reveal(frame_id) +
  labs(y = "Distance", x = "Frame", color = "Distance Type",
       title = "Computing Motion Features for Clustering",
       subtitle = "Week 1, Packers @ Vikings, 2nd and 7 from the 46") +
  scale_color_manual(values = c("v_distance" = "red", 
                                "h_distance" = "blue"),
                     labels = c("Vertical Distance", 
                                "Horizontal Distance")) +
  geom_vline(xintercept = 151, linetype = "dotted", color = "grey", size = 1) +
  annotate(
    "text", x = 151, y = -20, 
    label = "Frame of Snap", color = "grey", angle = 90, vjust = -0.5, hjust = 0
  ) +
  geom_vline(xintercept = 104, linetype = "dotted", color = "grey", size = 1) +
  annotate(
    "text", x = 104, y = -20, 
    label = "Start of Motion", color = "grey", angle = 90, vjust = -0.5, hjust = 0
  ) +
  geom_vline(xintercept = 53, linetype = "dotted", color = "grey", size = 1) +
  annotate(
    "text", x = 53, y = -20, 
    label = "Lineset", color = "grey", angle = 90, vjust = -0.5, hjust = 0
  )

motion_play <- play_visualization(2022091112, 917)

motion_anim <- animate(
  motion_play,
  width = 480, 
  height = 480
)
graph_anim <- animate(
  motion_graph,
  width = 480,
  height = 480
)
library(magick)
motion_gif <- image_read(motion_anim)
graph_gif <- image_read(graph_anim)

distance_gif <- image_append(c(motion_gif[1], graph_gif[1]), stack = FALSE)
for(i in 2:100){
  together <- image_append(c(motion_gif[i], graph_gif[i]), stack = FALSE)
  distance_gif <- c(distance_gif, together)
}
distance_gif
anim_save("Distance.gif", image_animate(distance_gif, fps = 10))

### prediction animation 1
animation_data <- read_csv("data_2022110603_915_te.csv") |> clean_names()
colnames(animation_data)
str(animation_data)

preds <- animation_data |> 
  ggplot(aes(frame_id, prediction)) +
  geom_line(color = "#203731") + 
  geom_vline(xintercept = 255, linetype = "dotted", color = "red", size = 1) + # Add vertical line
  annotate(
    "text", x = 255, y = max(animation_data$prediction) - 0.5, 
    label = "Time of Lineset", color = "red", angle = 90, vjust = -0.5, hjust = 0
  ) +
  ggthemes::theme_clean()  +
  labs(x = "Frame Number", y = "Estimated Probability of Blocking",
       title = "TEndencIQ Estimate for Marcedes Lewis",
       subtitle = "Week 9, Packers @ Lions, 4th & Goal from the 1") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic"),
        text = element_text(family = "sans")) +
  transition_reveal(frame_id)

play_viz <- play_visualization(2022110603, 915)
play_viz

play_anim <- animate(
  play_viz,
  width = 480, 
  height = 480,
  fps = 5
)
preds_anim <- animate(
  preds,
  width = 480,
  height = 480,
  fps = 5
)
library(magick)
play_gif <- image_read(play_anim)
preds_gif <- image_read(preds_anim)

comb_gif <- image_append(c(play_gif[1], preds_gif[1]), stack = FALSE)
for(i in 2:100){
  combined <- image_append(c(play_gif[i], preds_gif[i]), stack = FALSE)
  comb_gif <- c(comb_gif, combined)
}
comb_gif
anim_save("Estimate.gif", image_animate(comb_gif, fps = 10))

### prediction animation 2
second_anim_data <- read_csv("data_2022100900_3109.csv") |> clean_names()

second_preds <- second_anim_data |> 
  ggplot(aes(frame_id, prediction)) +
  geom_line(color = "#203731") + 
  geom_vline(xintercept = 54, linetype = "dotted", color = "red", size = 1) + 
  annotate(
    "text", x = 54, y = max(second_anim_data$prediction) - 0.5, 
    label = "Time of Lineset", color = "red", angle = 90, vjust = -0.5, hjust = 0
  ) +
  geom_vline(xintercept = 73, linetype = "dotted", color = "red", size = 1) + 
  annotate(
    "text", x = 73, y = max(second_anim_data$prediction) - 0.5, 
    label = "Start of Motion", color = "red", angle = 90, vjust = -0.5, hjust = 0
  ) +
  ggthemes::theme_clean()  +
  labs(x = "Frame Number", y = "Estimated Probability of Blocking",
       title = "TEndencIQ Estimate for Daniel Bellinger",
       subtitle = "Week 5, Giants @ Packers, 1st & 10 from the 40") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic"),
        text = element_text(family = "sans")) +
  transition_reveal(frame_id)

second_viz <- play_visualization(2022100900, 3109)

play_anim_two <- animate(
  second_viz,
  width = 480, 
  height = 480
)
preds_anim_two <- animate(
  second_preds,
  width = 480,
  height = 480
)
second_play_gif <- image_read(play_anim_two)
second_preds_gif <- image_read(preds_anim_two)

comb_gif_two <- image_append(c(second_play_gif[1], second_preds_gif[1]), stack = FALSE)
for(i in 2:100){
  combined_two <- image_append(c(second_play_gif[i], second_preds_gif[i]), stack = FALSE)
  comb_gif_two <- c(comb_gif_two, combined_two)
}
comb_gif_two
anim_save("Estimate_two.gif", image_animate(comb_gif_two, fps = 10))
