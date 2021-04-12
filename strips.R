library(tidyverse)
library(extrafont)
library(gtrendsR)
library(ggtext)
library(lubridate)
library(scico)
library(here)

search_terms <- c("Palaeolithic", "Paleolithic")

trends <- gtrends(keyword = search_terms,
        time = "all")

trends_clean <- trends %>%
  .$interest_over_time %>%
  mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% 
  mutate_at("hits", ~as.numeric(.)) %>%
  select(date, hits, keyword)
  
trend.labels <- trends_clean %>%
  group_by(keyword) %>%
  slice(1)
  
ggplot(trends_clean, aes(x = date, xend = date, y = 0, yend = 1)) +
  geom_segment(aes(colour = hits), size = 3) +
  labs(title = "Which spelling is used<span style = 'color:#4A7AC2;'> more</span>?",
       caption = "@CSHoggard | #30DayChartChallenge") +
  geom_text(data = trend.labels, aes(label = keyword, x = as.POSIXct("2004-01-01"), y = 0.18), size = 9, hjust = 0, colour = "white", family = "Roboto Black") +
  scale_colour_scico(palette = 'davos') +
  facet_wrap(~keyword) +
  theme_void() +
  theme(plot.margin = margin(20,20,20,20),
        plot.title = element_markdown(size = 32, family = "Roboto Black", margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 12, family = "Roboto Black", margin = margin(20,0,0,0)),
        strip.text = element_blank(),
        legend.position = "none")

ggsave("strips.png", plot = last_plot(), height = 2.85, width = 10, units = "in", dpi = 500) 
