library(spotifyr)
library(tidyverse)
library(ggbump)
library(wesanderson)
library(extrafont)
library(ggtext)
library(here)

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')

access_token <- get_spotify_access_token()

fleetwood <- get_artist_audio_features('fleetwood mac')

fleetwood_clean <- fleetwood %>%
  filter(!album_name == "Before the Beginning - 1968-1970 Rare Live & Demo Sessions (Remastered)") %>%
  select(album_release_year, danceability, energy, acousticness, valence) %>%
  group_by(album_release_year) %>%
  summarise(across(everything(), list(mean))) %>%
  pivot_longer(!album_release_year, names_to = "factor", values_to = "count")

ggplot(fleetwood_clean, aes(album_release_year, count, colour = factor)) +
  geom_point(size = 5) +
  geom_bump(size = 2,
            smooth = 7) +
  lims(y = c(0,1)) +
  labs(title = "Don't Stop (Thinking About Data Visualisation)",
       subtitle = "Fleetwood Mac album <span style = 'color:#E1C844;'> acousticness</span>, <span style = 'color:#003008;'>danceability</span>, <span style = 'color:#A2A475;'>energy</span> and <span style = 'color:#7BA487;'>musical positivity</span>",
       caption = "Data: Spotify Web API (courtesy of spotifyr) | #30DayChartChallenge | @CSHoggard",
       y = "Index") +
  scale_color_manual(values = wes_palette(n = 4, name = "Cavalcanti1")) + 
  theme_minimal(base_family = "Asap Condensed") +
  theme(plot.margin = margin(20,20,20,20),
        plot.title = element_text(size = 22),
        plot.subtitle = element_textbox_simple(colour = "grey10", size = 14),
        plot.caption = element_text(hjust = 0, colour = "grey40", margin = margin(20,0,5,0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(0,20,0,0)),
        legend.position = "none")

ggsave("30DayChartChallenge/Historic.png", plot = last_plot(), width = 300, height = 140, units = "mm", dpi = 400)  
