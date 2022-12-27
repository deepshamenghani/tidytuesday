# Libraries

library(tidyverse)
library(tidytuesdayR)
library(ggdark)
library(showtext)

# Get the Data

tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')

# Get Star trek font
font_add(family = "AmericanCaptainPatrius02Fre-G06G.otf", regular = "AmericanCaptainPatrius02Fre-G06G.otf")
showtext_auto()

# Function to calculate min and max year by grouping
function_get_minmax <- function(data, colname) {
  data %>% 
    select({{colname}}, year) %>% 
    group_by({{colname}}) %>% 
    summarise(minyear = min(year),
              maxyear = max(year)) %>% 
    ungroup()  %>% 
    mutate(lengthcol = maxyear - minyear) %>% 
    arrange(desc(lengthcol))
}

# Filter the data and get min and max year
data <- tlBooks %>% filter(format == "story")

plot_data <- function_get_minmax(data, title) %>% 
  head(10) %>% 
  mutate(title = fct_reorder(as_factor(title), maxyear)) %>% 
  mutate(endarrow = ifelse(minyear < 0 , -40000, minyear-15000)) 

# Dark mode for ggplot
dark_mode(.theme = theme_get(), verbose = TRUE,
          force_geom_invert = FALSE)
  
# Plot the data

plot_data %>%
  ggplot(aes(x = maxyear, y = title)) +
  geom_point(shape = 3, size = 8, color = "#E7B800") +
  xlim(c(-50000,80000)) +
  theme_minimal() +
  geom_text(aes(x = maxyear, y = title, label = maxyear), nudge_y = -0, nudge_x = 7000, color = "lightblue", size =5) +
  geom_segment(aes(x = maxyear, y = title, xend = endarrow, yend = title), arrow = arrow(length = unit(0.3, "cm")), color = "#E7B800", size = 1, alpha = 0.5) +
  geom_text(aes(x = endarrow, y = title, label = minyear), nudge_y = -0, nudge_x = -5100, color = "lightblue", size =5) +
  labs(
    title = "Top 10 Star Trek stories with the longest timelines" 
  ) +
  geom_label(x = -40000, y = 10.3, label = "START YEAR", fill = "#E7B800", size = 5, color = "black") +
  geom_label(x = 75000, y = 10.3, label = "END YEAR", fill = "#E7B800", size = 5, color = "black") +
  geom_vline(xintercept = -25000, linetype="dotted")  +
  geom_label(x = -30000, y = 0.7, label = "BC", fill = "#E7B800", size = 5, color = "black") +
  geom_label(x = -21000, y = 0.7, label = "AD", fill = "#E7B800", size = 5, color = "black") +
  dark_mode(theme_minimal()) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    title = element_text(size = 15, family = "AmericanCaptainPatrius02Fre-G06G.otf"),
    axis.text = element_text(size = 10, family = "AmericanCaptainPatrius02Fre-G06G.otf")
  )  
  
# Save the ggplot
ggsave(filename = "startrek_font.png", width=6, height=6)
