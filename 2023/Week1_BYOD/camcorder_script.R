library(camcorder)

gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8.3, # width of saved image
  height = 11.7, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

ranked_by_year %>% 
  filter(sex == "M") %>% 
  filter(year >= 1998) %>% 
  ggplot() +
  aes(xmin = 0,
      xmax = var) +
  aes(ymin = rank - 0.5,
      ymax = rank + 0.5,
      y = rank) +
  theme_minimal() +
  facet_wrap(~ year) +
  geom_rect(alpha = 0.3, color = "black") +
  aes(fill = name) +
  scale_x_continuous(
    limits = c(-2, 1.4)
  ) +
  geom_label(col = "gray12",
            hjust = "right",
            aes(label = name, fill = name),
            x = -0.2) +
  scale_y_reverse() +
  labs(fill = NULL) +
  labs(title = "Most popular baby names between 1998-2017",
       y = "",
       x = "Percentage of names") +
  facet_null() +
  scale_x_continuous(
    limits =c(-0.7, 2)
  ) +
  geom_text(x = 1.6 , y = -10,
            family = "Times",
            aes(label = as.character(year)),
            size = 20, col = "grey18") +
  aes(group = name) +
  theme(legend.position = "none") 


gg_playback(
  name = file.path("20230103.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
