library(tidyverse)
library(highcharter)
library(tidytuesdayR)

#import data
afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

language_count = afrisenti |> 
  count(language_iso_code, label) |> 
  group_by(language_iso_code) |> 
  mutate(perc = (n/sum(n))*100) |> 
  ungroup() |> 
  select(-n) 
  pivot_wider(names_from = "label", values_from = "perc")

language_countries_comb <- language_countries |> 
  group_by(language_iso_code) |> 
  summarise(countrylist = paste(country, collapse = ", ")) |> 
  ungroup()

color_df <- data.frame(label = c("positive", "neutral", "negative"), colorvalue = c("#007A4D", "#FFB612", "#DE3831")) 

language_perc <- language_count |> 
  left_join(language_countries_comb) |> 
  left_join(languages) |> 
  left_join(color_df)

language_perc |> 
  hchart(type = "column", hcaes(x = language, y = perc, color = colorvalue))  |> 
  hc_plotOptions(column = list(stacking = "normal")) |> 
  hc_yAxis(max = 100, min = 0, labels = list(format = "{value}%"),
              showFirstLabel = FALSE, title = "") |> 
  hc_tooltip(
    useHTML = TRUE,                              
    formatter = JS(
      "
      function(){
        outHTML = '<b>' + this.point.language + '<br>' + this.point.countrylist + '<br>' + this.point.label + '<br>' + Math.round(this.y) + '%'
        return(outHTML)
      }

      "
    ),
    shape = "callout", 
    borderWidth = 0 
  ) |> 
  hc_xAxis(
    title = ""
  )


