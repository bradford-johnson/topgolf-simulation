# load packages
library(ggplot2)
library(dplyr)
library(ggforce)

# set seed
set.seed(as.numeric(Sys.time()))

#---- select club ----
clubs <-c("Driver", "3 Wood", "Hybrid",
          "4 Iron", "6 Iron", "8 Iron",
          "Pitching Wedge")

selected_club <- sample(clubs, 1)

club_lab <- paste0("Club: ", selected_club)

#---- simulate hits ----

if (selected_club == "Driver") {
  y <- runif(20, min = 8.0, max = 14)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

if (selected_club == "3 Wood") {
  y <- runif(20, min = 8.0, max = 14)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

if (selected_club == "Hybrid") {
  y <- runif(20, min = 6.0, max = 12.5)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

if (selected_club == "4 Iron") {
  y <- runif(20, min = 8.0, max = 12.5)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

if (selected_club == "6 Iron") {
  y <- runif(20, min = 4.9, max = 11.0)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

if (selected_club == "8 Iron") {
  y <- runif(20, min = 4.8, max = 10.0)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

if (selected_club == "Pitching Wedge") {
  y <- runif(20, min = 0.5, max = 6.0)
  x <- runif(20, min = 0.0, max = 6)
  
  df <- data.frame(x,y)
}

# load data
dat <- readr::read_csv("data/targets.csv")

#---- target colors ----
t_red <- "#F00016"

red_target <- dat %>%
  filter(color == "red")

t_yellow <- "#FFBB00"

yellow_target <- dat %>%
  filter(color == "yellow")

t_green <- "#27B028"

green_target <- dat %>%
  filter(color == "green")

t_brown <- "#BF5107"

brown_target <- dat %>%
  filter(color == "brown")

t_blue <- "#006EB4"

blue_target <- dat %>%
  filter(color == "blue")

t_white <- "#BDC1C0"

white_target <- dat %>%
  filter(color == "white")

t_trench <- "#141615"

#---- create base visual ----
tg <- ggplot() + 
  geom_circle(data = red_target, aes(x0=x, y0=y, r=size), color = t_red,
              fill = t_red, alpha = .5) +
  
  geom_circle(data = yellow_target, aes(x0=x, y0=y, r=size), color = t_yellow,
              fill = t_yellow, alpha = .5) +
  
  geom_circle(data = green_target, aes(x0=x, y0=y, r=size), color = t_green,
              fill = t_green, alpha = .5) +
  
  geom_circle(data = brown_target, aes(x0=x, y0=y, r=size), color = t_brown,
              fill = t_brown, alpha = .5) +
  
  geom_circle(data = blue_target, aes(x0=x, y0=y, r=size), color = t_blue,
              fill = t_blue, alpha = .5) +
  
  geom_circle(data = white_target, aes(x0=x, y0=y, r=size), color = t_white,
              fill = t_white, alpha = .5) +
  
  geom_rect(aes(xmin = 1.2, xmax = 4.8, ymin = 13.5, ymax = 14), fill = t_trench,
            color = t_trench, linewidth = .3, alpha = .5) +
  coord_equal() +
  theme_classic()

#---- point plot ----
# create title
title_lab <- paste0(Sys.time())

# plot
tg +
  geom_point(data = df, aes(x, y)) +
  geom_circle(data = red_target, aes(x0=x, y0=y, r=size), color = t_red) +
  
  geom_circle(data = yellow_target, aes(x0=x, y0=y, r=size), color = t_yellow) +
  
  geom_circle(data = green_target, aes(x0=x, y0=y, r=size), color = t_green) +
  
  geom_circle(data = brown_target, aes(x0=x, y0=y, r=size), color = t_brown) +
  
  geom_circle(data = blue_target, aes(x0=x, y0=y, r=size), color = t_blue) +
  
  geom_circle(data = white_target, aes(x0=x, y0=y, r=size), color = t_white) +
  
  geom_rect(aes(xmin = 1.2, xmax = 4.8, ymin = 13.5, ymax = 14),
            color = t_trench, linewidth = .3, fill = NA) +
  theme_void() +
  labs(title = title_lab,
       subtitle = club_lab)

#---- save plot ----
ggsave("plots/daily-game.png")    
unlink("Rplots.pdf")

#---- mutate {in_target} ----
df <- df |>
  mutate(in_red1 = sqrt((1 - x)^2 + (1 - y)^2) < .5,
         in_red2 = sqrt((3 - x)^2 + (1 - y)^2) < .5,
         in_red3 = sqrt((5 - x)^2 + (1 - y)^2) < .5,
         in_yellow1 = sqrt((2 - x)^2 + (3 - y)^2) < .75,
         in_yellow2 = sqrt((4 - x)^2 + (3 - y)^2) < .75,
         in_green = sqrt((3 - x)^2 + (5.25 - y)^2) < 1,
         in_brown = sqrt((4 - x)^2 + (7.5 - y)^2) < 1,
         in_blue = sqrt((2 - x)^2 + (9.25 - y)^2) < 1,
         in_white = sqrt((4 - x)^2 + (11.5 - y)^2) < .5,
         in_trench = x > 1.2 & x < 13.5 & y > 13.5 & y < 14)

#---- mutate {in_overall} ----
df |>
  mutate(anyTRUE = if_any(.cols = contains('in'), I))

