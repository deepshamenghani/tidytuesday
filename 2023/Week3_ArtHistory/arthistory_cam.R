library(camcorder)

gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 8.3, # width of saved image
  height = 11.7, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)


artists %>%
  # filter(year >= 2000) %>%
  filter((book %in% c("Gardner"))) %>%
  group_by(year, artist_race_nwi) %>%
  summarise(maxspace = max(space_ratio_per_page_total, na.rm=TRUE),
            avgspace = mean(space_ratio_per_page_total, na.rm=TRUE),
            minspace = min(space_ratio_per_page_total, na.rm=TRUE)) %>%
  ungroup() %>%
  ggplot(aes(year,maxspace , color = artist_race_nwi)) +
  geom_line() +
  coord_flip()



gg_playback(
  name = file.path("20230117.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "white"
)
