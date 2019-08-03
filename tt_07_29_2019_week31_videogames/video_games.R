library(tidyverse)
library(lubridate)
library(plotly)
library(ggrepel)
library(cowplot)
library(ggforce)

vg_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

# Clean dates
vg_edited <- vg_df %>% 
    mutate(release_date = as.Date(release_date, format = '%b %d, %Y')) %>%
    mutate(release_year  = as_factor(year(release_date))) %>% 
    drop_na(release_year) %>% 
    mutate(label_text = str_glue("Game: {game}
                                 Year: {release_year}
                                 Price: {price}"))

# Create text for annotations by playtime
annotations_playtime <- vg_edited %>% 
    arrange(desc(median_playtime)) %>% 
    filter(row_number() < 5) %>% 
    mutate(log_price = log(price)) %>% 
    mutate(game_label = str_glue("{game}\n{release_year}\n{scales::dollar(price)}")) %>% # Create label text for plot
    select(game_label, median_playtime, log_price) %>% 
    mutate(y = c(median_playtime[1] + 400, median_playtime[2] - 200, median_playtime[3] + 150, median_playtime[4] - 400),  # Define annotations location
           x = c(log_price[1], log_price[2] + 0.85, log_price[3] - 0.85, log_price[4])) %>% 
    mutate(y_arrow = c(median_playtime[1] + 200, median_playtime[2] + 50, median_playtime[3] - 50, median_playtime[4] - 200)) # Define arrow location

# Create text for annotations by price
annotations_price <- vg_edited %>% 
    arrange(desc((price))) %>% 
    filter(row_number() < 3) %>% 
    mutate(log_price = log(price)) %>% 
    mutate(game_label = str_glue("{game}\n{release_year}\n{scales::dollar(price)}")) %>%
    select(game_label, median_playtime, log_price) %>% 
    mutate(y = c(median_playtime[1] + 550, median_playtime[2] + 450), # Define annotations location
           x = c(log_price[1] - 0.2, log_price[2] - 0.75)) %>% 
    mutate(y_arrow = c(median_playtime[1] + 300, median_playtime[2] + 200))  # Define arrow location
    

# Plot with annotations and arrows
pc_games_plot <- ggplot(data = vg_edited, 
                        aes(x = log(price), 
                            y = median_playtime)) +
    geom_point(aes(color = median_playtime, 
                   size  = median_playtime), 
                   alpha = 0.8) +
    scale_color_gradient(low  = "blue2", 
                         high = "darkgreen") +
    annotate("text", 
             x        = annotations_playtime$x, 
             y        = annotations_playtime$y, 
             fontface = "bold", 
             label    = annotations_playtime$game_label, 
             size     = 5, 
             color    = "darkgreen") + 
    geom_curve(data      = annotations_playtime, 
               aes(x     = log_price, 
                   y     = median_playtime, 
                   xend  = x, 
                   yend  = y_arrow),
               arrow     = arrow(length = unit(0.07, "inch")), 
               size      = 1,
               color     = "gray20", 
               curvature = -0.25)+
    annotate("text", 
             x        = annotations_price$x, 
             y        = annotations_price$y, 
             fontface = "bold", 
             label    = annotations_price$game_label, 
             size     = 5, color = "blue2") + 
    geom_curve(data      = annotations_price, 
               aes(x     = log_price, 
                   y     = median_playtime, 
                   xend  = x, 
                   yend  = y_arrow),
               arrow     = arrow(length = unit(0.07, "inch")), 
               size      = 1,
               color     = "gray20", 
               curvature = -0.25) + 
    geom_mark_circle(data       = annotations_price, 
                     aes(x      =log_price[1] ,
                         y      =median_playtime[1]), 
                     color      ='blue2', 
                     label.fill = NA, 
                     expand     = unit(3, "mm")) + 
    geom_mark_circle(data       = annotations_price, 
                     aes(x      =log_price[2] ,
                         y      =median_playtime[2]), 
                     color      ='blue2', 
                     label.fill = NA, 
                     expand     = unit(3, "mm")) +
    labs(
        x     = "price (logarithmic scale)",
        y     = "Median playtime",
        title = "Playtime vs Price"
    ) +
    theme_minimal() +
    scale_y_continuous(breaks = seq(from = 0, to = 4000, by = 500))  +
    scale_x_continuous(breaks = seq(from = -1, to = 7, by = 1)) +
    theme(
        axis.title.x     = element_text(size = 20),
        axis.title.y     = element_text(size = 20, vjust = 1.5),
        axis.text.x      = element_text(size = 15),
        axis.text.y      = element_text(size = 15),
        plot.title       = element_text(size = 25),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    ) 

ggsave("PC_Games.png", plot = pc_games_plot, width = 60, height =25, units = "cm")
