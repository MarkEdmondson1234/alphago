# Raw data: https://github.com/yenw/computer-go-dataset/tree/master/AI/AlphaGo

# Load packages
library(gganimate)
library(ggart) # devtools::install_github("marcusvolz/ggart")
library(ggforce)
library(readxl)
library(tidyverse)

# Create lookup tables
lookup_x <- data.frame(numbers = 1:26, letters = letters[1:26], stringsAsFactors = FALSE)
lookup_y <- data.frame(numbers = 1:26, letters = letters[26:1], stringsAsFactors = FALSE)

# Read in and manipulate data
data <- read_xlsx("data/data.xlsx") %>%
  left_join(lookup_x, by = c("row" = "letters")) %>% rename(row1 = numbers) %>%
  left_join(lookup_y, by = c("col" = "letters")) %>% rename(col1 = numbers) %>%
  select(colour, row1, col1, game) %>%
  rename(x = row1, y = col1) %>%
  group_by(game) %>%
  mutate(frame = 1:n())

# Create plot
p <- ggplot() +
  geom_circle(aes(x0 = x, y0 = y, r = 0.4, fill = colour, frame = frame,
                  cumulative = TRUE), data %>% filter(game < 5), colour = "transparent") +
  scale_fill_manual(values = c("#707070", "white")) +
  scale_y_reverse() +
  facet_wrap(~game, nrow = 2) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0, bg_col = "#000000")

# Save plot
ggsave("plots/plot001.png", p, width = 21, height = 21, units = "cm")

# Create animation
animation::ani.options(interval = 1/15)
gganimate(p, "gifs/gif001.gif", title_frame = FALSE, ani.width = 600, ani.height = 600)
