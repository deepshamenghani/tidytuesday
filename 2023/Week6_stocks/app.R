library(tidytuesdayR)
library(tidyverse)

library(shiny)
library(shinythemes)

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


ui <- bootstrapPage(
  
  theme  = bslib::bs_theme(version = 5, primary = "#008080"),
  title = "Stocks analysis for TidyTuesday"
  
)

server <- function(input, output) {
  
}


shinyApp(ui = ui, server = server)
