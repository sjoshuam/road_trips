##########==========##########==========##########==========##########==========

## SET-UP ======================================================================

## meta-information
## author: Josh Mendelsohn
## creation: 2021-01-24
## version: R 4.0.3
## description: renders maps of road trips and road trip goals

## environment set up
remove(list = objects())
options(scipen = 2, digits = 6)
library(lintr) #lint("2_render_map.R")
library(tidyverse)

## LAYOUT PLAN =================================================================

## poster dimensions (layout plan)

## poster size: 36.0in x 24.0in, all measurements in inches
## margins: 0.5 on all sides
## map x/y ratio: 1.623

## panel dimensions (start-end coordinates)
## standard routes: 8.115 (0.5-8.615) x 5 (0.5-5.5) 
## routes traveled: 28.403 (0.5 - 28.903) x 17.5 (6-23.5)
## progress bar: 26.385 (9.115-35.5) x 5 (0.5-5.5)
## map legend: 6.098 (29.403-35.5) x 17.5 (6 x 23.5)

## READ IN DATA ================================================================

travels  <- readRDS("B_Intermediates/travels.RData")
progress <- readRDS("B_Intermediates/progress.RData")

## BUG PATCH ===================================================================
warning("TODO: hunt down source of bug in script #1")

travels <- travels %>%
  mutate(group = as.numeric(as.factor(paste(level1, group))))

## PREPARE TO RENDER POSTER ====================================================

## rescale coordinates to unity
travels$x <- travels$x - min(travels$x)
travels$y <- travels$y - min(travels$y)
scale_factor <- 1 / max(c(travels$x, travels$y))
travels[, c("x", "y")] <- travels[, c("x", "y")] * scale_factor

## calculate coordinates for each plot panel
traveled_routes <- standard_routes <- travels[, c("x", "y")]
traveled_routes$x <- (traveled_routes$x * 28.403) + 0.5
traveled_routes$y <- (traveled_routes$y * 28.403) + 6.0
standard_routes$x <- (standard_routes$x * 8.115)  + 0.5
standard_routes$y <- (standard_routes$y * 8.115)  + 0.5

colnames(traveled_routes) <- paste0("traveled_", colnames(traveled_routes) )
colnames(standard_routes) <- paste0("standard_", colnames(standard_routes) )
travels <- bind_cols(travels, traveled_routes, standard_routes)

## categorize cities by visited / not visited
travels <- mutate(travels,
  "been_there" = travels$level1 %in% travels$level1[travels$theme == "Travels"]
  ) %>%
#  mutate("been_there" = if_else(been_there, "Visited", "Not Visited")) %>%
  mutate(
    "been_there" = if_else(polygon == "Point", been_there, as.logical(NA)))

## categorize cities by closest standard route
cities <- travels %>%
  filter(polygon == "Point", theme == "Wishlist") %>%
  select(level1, x, y)
routes <- filter(travels, polygon == "LineString", theme == "Wishlist") %>%
  select(level1, x, y)

city_route_dist <- outer(cities$x, routes$x, FUN = "-")^2
city_route_dist <- city_route_dist + outer(cities$y, routes$y, FUN = "-")^2
city_route_dist <- apply(city_route_dist, 1, which.min)
city_route_dist <- routes$level1[city_route_dist]
city_route_dist <- tibble(
  "City" = cities$level1,
  "route_of_city" = city_route_dist)
travels <- left_join(travels, city_route_dist, by = c("level1" = "City"))

## generate color palette
color_palette <- tribble(
  ~name, ~hue,
  "land", 0.10,
  "sea",  0.60,
  )
color_palette$light  <- hsv(color_palette$hue, 0.1, v= 1.0)
color_palette$medium <- hsv(color_palette$hue, 0.8, v= 0.7)
color_palette$dark   <- hsv(color_palette$hue, 1.0, v= 0.4)

grab_color <- function(color_name, color_type, dat = color_palette) {
  pull(dat[dat$name == color_name,], color_type)
}

## define master size scalar
master_size <- 25.4 * 0.05

## initialize ggplot object
travel_poster <- ggplot(size = 25.4*0.1) +
  coord_fixed(xlim= c(0, 36), ylim = c(0, 24), expand = FALSE, ratio = 1)

## PANEL 4: ROUTES TRAVELED ====================================================

## generate blank state map
states <- travels %>%
  filter(polygon == "Polygon", theme == "map") %>%
  select(level1, group, traveled_x, traveled_y)
travel_poster <- travel_poster + geom_polygon(
  data = states,
  mapping = aes(x = traveled_x, y = traveled_y, group = group),
  color = grab_color("land", "dark"),
  fill = grab_color("land", "light"),
  size = master_size
  )
remove(states)

## generate routes
routes <- travels %>%
  filter(theme == "Travels", polygon == "LineString") %>%
  select(group, traveled_x, traveled_y)
travel_poster <- travel_poster + geom_path(data = routes,
  mapping = aes(x = traveled_x, y = traveled_y, group = group),
  color = grab_color("land", "light"),
  size = master_size * 2
  )
travel_poster <- travel_poster + geom_path(data = routes,
  mapping = aes(x = traveled_x, y = traveled_y, group = group),
  color = grab_color("sea", "medium"),
  size = master_size
  )
remove(routes)

## render cities
cities <- travels %>%
  filter(theme == "Wishlist", polygon == "Point") %>%
  select(been_there, traveled_x, traveled_y)
travel_poster <- travel_poster + geom_point(data = cities,
  mapping = aes(x = traveled_x, y = traveled_y),
  size = master_size * ifelse(cities$been_there, 6, 4),
  stroke = master_size * 2,
  color = ifelse(
   cities$been_there, grab_color("land", "light"), grab_color("land", "dark")),
  fill = ifelse(
   cities$been_there, grab_color("sea", "dark"), grab_color("land", "light")),
  shape = 21
  )
remove(cities)

## PANEL 3: STANDARD ROUTES ====================================================

## render states
states <- travels %>%
  filter(polygon == "Polygon", theme == "map") %>%
  select(group, standard_x, standard_y)
travel_poster <- travel_poster + geom_polygon(
  data = states,
  mapping = aes(x = standard_x, y = standard_y, group = group),
  color = grab_color("sea", "light"),
  fill = grab_color("land", "light"),
  size = master_size
  )
remove(states)

## render routes
routes <- travels %>%
  filter(theme == "Wishlist", polygon == "LineString") %>%
  select(level1, group, standard_x, standard_y)
travel_poster <- travel_poster + geom_path(data = routes,
  mapping = aes(x = standard_x, y = standard_y, group = group, color = level1),
  size = master_size
  ) + scale_color_hue(l = 40)
remove(routes)

## render cities
cities <- travels %>%
  filter(theme == "Wishlist", polygon == "Point") %>%
  select(route_of_city, standard_x, standard_y)
travel_poster <- travel_poster + geom_point(data = cities,
  mapping = aes(x = standard_x, y = standard_y, color = route_of_city),
  size = master_size * 4
  )
remove(cities)

## PANEL 2: PROGRESS BAR =======================================================

## PANEL 1: LEGEND =============================================================

## MAP STYLING =================================================================

## render panel dividers and remove automatic legend
dividing_lines <- tibble(
  x = c(8.615 + 0.25, 29.403 - 0.25, 0.5),
  y = c(5.5 + 0.25, 23.5, 5.5 + 0.25),
  xend = c(8.615 + 0.25, 29.403 - 0.25, 35.5),
  yend = c(0.5, 6 - 0.25, 5.5 + 0.25)
  )
travel_poster <- travel_poster + geom_segment(data = dividing_lines,
  mapping = aes(x = x, y = y, xend = xend, yend = yend),
  color = grab_color("sea", "dark")
  ) + theme(
    legend.position = "none", plot.margin = unit(rep(0, 4), units = "in"),
    axis.text = element_blank(), axis.line = element_blank(),
    axis.ticks = element_blank(), axis.title = element_blank(),
    panel.background = element_rect(fill = grab_color("sea", "light")),
    panel.grid = element_blank(), panel.border = element_blank()
    )

## EXPORT TO PDF ===============================================================

pdf("C_Outputs/road_trip_poster.pdf", width = 36, height = 24)
travel_poster
graphics.off()

##########==========##########==========##########==========##########==========
