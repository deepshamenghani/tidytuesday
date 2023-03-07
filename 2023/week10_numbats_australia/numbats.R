# For loading Tidy Tuesday data
library(tidytuesdayR)

# EDA
library(tidyverse)

# Plotting
library(highcharter)
library(purrr)

# Read data

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

# Edit the data to get counts for plotting
numbats_edited <- numbats %>% 
  filter(year >= 2000) %>% 
  count(dataResourceName, year) %>% 
  complete(dataResourceName, year = seq(2000,2020,by=1)) %>% 
  replace_na(list(n=0)) 

# Create the initial column plot data
numbats_column <- numbats_edited %>% 
  group_by(name = dataResourceName) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(drilldown = tolower(name)) %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  tibble()

# Create the yearly drilldown data
numbats_year <- numbats_edited %>% 
  group_by(year) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() 

# Create ids to connect layers of plot
idlist <- numbats_column$name

yearlyview <- idlist %>% map(~ numbats_edited %>% 
                               filter(dataResourceName == .x) %>% select(year, n))

dflist <- yearlyview %>% map(~ list_parse2(.x))

listall <- c(1:5) %>% map(~ list(
  id = tolower(idlist[[.x]]),
  data = dflist[[.x]]
))

numbat_observations <-  highchart() %>%
  hc_title(text = "Numbat observations by Data Resources",
           style = list(fontWeight = "bold", fontSize = "20px"),
           align = "center") %>% 
  hc_subtitle(text = "Top 5 by count from year 2000 onward") %>% 
  hc_caption(text = "Click on bar to drilldown for yearly view") %>% 
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(
        enabled = TRUE,
        style = list(fontSize = 15, textOutline = "none")
      )
    )
  ) %>%
  hc_add_series(
    data = numbats_column,
    type = "column",
    hcaes(name = name, y = n),
    name = "Things",
    color = "#008080"
  ) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = listall
  ) 

htmlwidgets::saveWidget(numbat_observations,"numbat_drilldown.html", selfcontained = TRUE)
