library(camcorder)
library(showtext)
library(ggthemes)
library(cowplot)
library(magick)
library(ggimage)

Gardnerimage <- image_read("GardnersATTP2.png")
Gardnerimage_transparent <- image_colorize(Gardnerimage, opacity = 60, color = "white")
Gardnerimage_transparent2 <- image_colorize(Gardnerimage, opacity = 20, color = "white")


ggdraw() +
  draw_plot(wednesday_chart) +
  draw_image(wednesdayadams2, x = 0.9, y = 0.9, hjust = 1, vjust = 1, width = 0.13, height = 0.4) 

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 9, # width of saved image
  height = 12, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)


artist_space_plot <-  artist_spaces %>%
  mutate(label = if_else(year == max(year), as.character(artist_gender_race), NA_character_))%>% 
  mutate(label = label %>% str_replace_all("White","W"))  %>% 
  mutate(label_year = if_else(year %in% c(1936, 1959, 1986, 2016), as.character(year), NA_character_)) %>% 
  filter((book %in% c("Gardner"))) %>% 
  ggplot(aes(year, maxspace ,color = artist_gender_race)) +
  geom_line(size = 2) +
  geom_text(aes(label = label),
            na.rm = TRUE,
            size = 17,
            hjust = -0.15,
            vjust=0.5) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  ) +
  scale_color_manual(values = c("#8c684e", "gray18", "#337b70", "darkgreen")) +
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
