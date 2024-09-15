# Using this to evaluate the frequency and scale of new pathways

library(tidyverse)
library(rvest)

argus_theme <- function() {
  theme_light(base_family = "Segoe UI Semibold") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size = 0.25, linetype = "solid", 
                                            color = "gray"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_rect(color = "#005DAA", fill = "#6CADDF"),
          strip.text.x = element_text(color = "White"))
}

url <- "https://ww2.arb.ca.gov/2021-lcfs-pathways-requiring-public-comments"

first_look <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath = '//*[@id="main-content"]/main/div[2]/article/div/div/div[2]/section[1]/div/table[3]/tbody') %>%
  html_nodes('tr')

second_look <- url %>%
  xml2::read_html() %>%
  html_nodes('.container') %>%
  html_table()

third_look <- url %>%
  xml2::read_html() %>%
  html_nodes(xpath = '//*[@id="main-content"]/main/div[2]/article/div/div/div[2]/section[1]/div/table[3]') %>%
  html_table()

paths <- third_look[[1]]
paths$`Posted Date` <- as.Date(paths$`Posted Date`, '%m/%d/%Y')

month_filings <- paths %>%
  mutate(months = lubridate::month(`Posted Date`)) %>%
  count(months) %>%
  mutate(Date = as.Date(paste(2021, months, 01, sep="-"), '%Y-%m-%d'))

ggplot(month_filings, aes(x = Date, y = n)) +
  geom_col() +
  ylim(0,15) +
  scale_x_date(date_breaks = "month",
               date_labels = "%B") +
  labs(title = "Pathway filings by month in 2021", 
       x = element_blank(),
       y = "Pathway Applications Filed\n") +
  argus_theme()

week_filings <- paths %>%
  mutate(weeks = lubridate::week(`Posted Date`)) %>%
  count(`weeks`) %>%
  mutate(Date = as.Date(paste(2021, weeks, 1, sep="-"), "%Y-%U-%u"))

ggplot(week_filings, aes(x = Date, y = n)) +
  geom_col() +
  scale_x_date(date_breaks = "month",
               date_labels = "%B") +
  labs(title = "Pathway filings by week in 2021", 
       x = element_blank(),
       y = "Pathway Applications Filed\n") +
  argus_theme()
  
url2022 <- "https://ww2.arb.ca.gov/resources/documents/lcfs-pathways-requiring-public-comments"

latest <- url2022 %>%
  xml2::read_html() %>%
  html_nodes(xpath = '//*[@id="main-content"]/main/div[2]/article/div/div/div[2]/section[1]/div/table[3]') %>%
  html_table()
latest <- latest[[1]]
latest$`Posted Date` <- as.Date(latest$`Posted Date`, '%m/%d/%Y')

latest_weeks <- latest %>%
  mutate(weeks = lubridate::week(`Posted Date`)) %>%
  count(`weeks`) %>%
  mutate(Date = as.Date(paste(2022, weeks, 1, sep="-"), "%Y-%U-%u"))

ggplot(latest_weeks, aes(x = Date, y = n)) +
  geom_col() +
  scale_x_date(date_breaks = "month",
               date_labels = "%B") +
  labs(title = "Pathway filings by week in 2022", 
       x = element_blank(),
       y = "Pathway Applications Filed\n") +
  argus_theme()
