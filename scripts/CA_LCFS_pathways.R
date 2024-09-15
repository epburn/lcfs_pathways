library(tidyverse)
library(openxlsx)

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

pathways_url <- 'https://ww2.arb.ca.gov/sites/default/files/classic/fuels/lcfs/fuelpathways/current-pathways_all.xlsx'

LCFS_path <- read.xlsx(pathways_url,
                       sheet = "All Pathways",
                       startRow = 4,
                       detectDates = TRUE)

CFP_url <- "https://www.oregon.gov/deq/FilterDocs/cfp-All-CIs.xlsx"

CFP_path <- read.xlsx(CFP_url,
                      sheet = "Updated FPC list",
                      startRow = 5,
                      detectDates = TRUE)

# LCFS_path <- mutate_at(LCFS_path, 
#                        vars(Feedstock, Fuel.Type, Fuel.Category), 
#                        as.factor)

LCFS_path %>%
  mutate(`Fuel Source` = ifelse(Feedstock %in% c("Dairy Manure (026)", "Swine Manure (044)"), "Manure", "Not Manure")) %>%
  group_by(lubridate::year(Certification.Date), `Fuel Source`) %>%
  summarize(`New Certifications` = n()) %>%
  ggplot(., aes(x = `lubridate::year(Certification.Date)`, 
                y = `New Certifications`, 
                fill = `Fuel Source`)) +
  geom_col() +
  argus_theme() +
  labs(x = element_blank(),
       title = "Manure vs Non-Manure pathways by year",
       caption = "CARB")

LCFS_path %>%
  group_by(lubridate::year(Certification.Date), Fuel.Category) %>%
  summarize(`New Certifications` = n()) %>%
  ggplot(., aes(x = `lubridate::year(Certification.Date)`, 
                y = `New Certifications`, 
                fill = Fuel.Category)) +
  geom_col(position = "dodge") +
  argus_theme() +
  labs(x = element_blank(),
       title = "New LCFS pathway certifications by fuel category",
       caption = "CARB")

LCFS_path %>%
  group_by(Certification.Date, Fuel.Category) %>%
  summarize(`New Certifications` = n()) %>%
  ggplot(., aes(x = Certification.Date, 
                y = `New Certifications`, 
                fill = Fuel.Category)) +
  geom_col() +
  argus_theme() +
  labs(x = element_blank(),
       title = "New LCFS pathway certifications by fuel category")

LCFS_path %>%
  group_by(Fuel.Category) %>%
  summarize(`New Certifications` = n()) %>%
  ggplot(., aes(x = Fuel.Category, 
                y = `New Certifications`, 
                fill = `New Certifications`)) +
  geom_col() +
  argus_theme() +
  labs(x = element_blank(),
       title = "New LCFS pathway certifications by fuel category")

LCFS_path %>%
  filter(Fuel.Category == "Renewable Diesel") %>%
  mutate(Year = lubridate::year(Certification.Date)) %>%
  ggplot(., aes(x = Year, y = Current.Certified.CI, group = Year)) +
  geom_boxplot()

LCFS_path %>%
  filter(Fuel.Category == "Renewable Diesel") %>%
  mutate(Year = lubridate::year(Certification.Date)) %>%
  ggplot(., aes(x = Year, y = Current.Certified.CI, group = Year)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="grey", alpha=0.2)
