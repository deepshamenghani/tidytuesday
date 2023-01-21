library(camcorder)
library(showtext)
library(ggthemes)
library(cowplot)
library(magick)
library(ggimage)

gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 9, # width of saved image
  height = 12, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

artist_space_plot <-  artist_spaces %>%
  ggplot(aes(year, maxspace ,color = artist_gender_race)) +
  geom_line(size = 2) +
  geom_text(aes(label = label),
            na.rm = TRUE,
            size = 15,
            hjust = -0.15,
            vjust=0.5, family = "MedievalSharp-Regular.ttf") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_color_manual(values = c("#8c684e", "gray18", "#337b70", "yellow4")) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, by = 0.5)) +
  scale_x_continuous(limits = c(1926, 2045), breaks = seq(1926, 2045, by = 30))  +
  labs(
    title = "Maximum page space by artist gender and race",
    x= "",
    y = "",
    color = "",
    subtitle = "Gardner’s Art Through the Ages"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(hjust = 2, size = 50),
    text = element_text(size = 2, family = "MedievalSharp-Regular.ttf", color="brown4"),
    title = element_text(family = "MedievalSharp-Regular.ttf", color="brown4",size = 50, hjust= 1, vjust=10),
    legend.position = "none"
  )  +
  geom_text(aes(label = label_year, y=0.01), 
            na.rm = TRUE,
            size = 18,
            # hjust = -0.15,
            vjust=0.7, family = "MedievalSharp-Regular.ttf", color="gray20")

ggdraw()  +
  draw_image(Gardnerimage_transparent, x = 0.04, height=1.05) +
  draw_plot(artist_space_plot) +
  draw_image(Gardnerimage_transparent2, x = 0.97, y = 1.04, hjust = 1, vjust = 1, width = 0.25, height = 0.4) 
  draw_image(Gardnerimage_transparent, x = 0, y = 0, height = 1.01) +
  draw_plot(artist_space_plot)


gg_playback(
  name = file.path("20230117.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)

ggsave("image.png",device = "png", # device to use to save images
       width = 9, # width of saved image
       height = 12, # height of saved image
       units = "in", # units for width and height
       dpi = 300)
