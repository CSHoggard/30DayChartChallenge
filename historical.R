library(spotifyr)
library(tidyverse)
library(ggbump)
library(wesanderson)
library(extrafont)
library(ggtext)
library(here)

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXX')

access_token <- get_spotify_access_token()

fleetwood <- get_artist_audio_features('fleetwood mac')

fleetwood_clean <- fleetwood %>%
  filter(!album_name == "Before the Beginning - 1968-1970 Rare Live & Demo Sessions (Remastered)") %>%
  select(album_release_year, danceability, energy, acousticness, valence) %>%
  group_by(album_release_year) %>%
  summarise(across(everything(), list(mean))) %>%
  pivot_longer(!album_release_year, names_to = "factor", values_to = "count")

ggplot(fleetwood_clean, aes(album_release_year, count, colour = factor)) +
  geom_point(size = 4) +
  geom_bump(size = 1.5,
            smooth = 7) +
  lims(y = c(0,1)) +
  labs(title = "Deconstructing Fleetwood Mac Albums",
       subtitle = "Investigating <span style = 'color:#E1C844;'> acousticness</span>, <span style = 'color:#003008;'>danceability</span>, <span style = 'color:#003008;'>energy</span> and <span style = 'color:#7BA487;'>musical positivity</span>",
       caption = "Data: Spotify Web API (courtesy of spotifyr) | @CSHoggard",
       y = "Index") +
  scale_color_manual(values = wes_palette(n = 4, name = "Cavalcanti1")) + 
  theme_minimal(base_family = "Asap Condensed") +
  theme(plot.margin = margin(20,20,20,20),
        plot.subtitle = element_textbox_simple(colour = "grey10", family = "IBM Plex Sans", size = 11),
        axis.title.x = element_blank(),
        legend.position = "none")
  
