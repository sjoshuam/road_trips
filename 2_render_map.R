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

## PREPARE TO RENDER NON-MAP PANELS ============================================

## convert to non-tibble format 
progress <- add_row(
  progress,
  tibble("theme" = c("Percent", "Bars"), "state" = rep(NA, 2),
    "city" = rep(NA, 2))
  ) %>%
  as.data.frame()

rownames(progress) <- progress$theme
progress$theme <- NULL

## calculate percentage and 25-bar equivalent
progress["Percent", ] <- progress["Travels", ] / progress["Wishlist", ]
progress["Bars", ] <- floor(progress["Percent", ] * 25)

## generate progress bars
progress_bars <- tibble(
  "xmin" = seq(from = 9.115 + 4.25, to = 35.5 - 2, length.out = 26)[-26],
  "xmax" = seq(from = 9.115 + 4.25, to = 35.5 - 2, length.out = 26)[-1] - 0.2,
  "ymin_state" = 0.7,
  "ymax_state" = 1.7,
  "ymin_city" = 2.7,
  "ymax_city" = 3.7,
  "progress_state" = FALSE,
  "progress_city" = FALSE
  )

progress_bars$progress_state[seq(progress["Bars", "state"])] <- TRUE
progress_bars$progress_city[seq(progress["Bars", "city"])] <- TRUE

## PREPARE TO RENDER MAP PANELS ================================================

## rescale coordinates to unity
travels$x <- travels$x - min(travels$x)
travels$y <- travels$y - min(travels$y)
scale_factor <- 1 / max(c(travels$x, travels$y))
travels[, c("x", "y")] <- travels[, c("x", "y")] * scale_factor
remove(scale_factor)

## calculate coordinates for each plot panel
traveled_routes <- standard_routes <- travels[, c("x", "y")]
traveled_routes$x <- (traveled_routes$x * 28.403) + 0.5
traveled_routes$y <- (traveled_routes$y * 28.403) + 6.0
standard_routes$x <- (standard_routes$x * 8.115)  + 0.5
standard_routes$y <- (standard_routes$y * 8.115)  + 0.5

colnames(traveled_routes) <- paste0("traveled_", colnames(traveled_routes) )
colnames(standard_routes) <- paste0("standard_", colnames(standard_routes) )
travels <- bind_cols(travels, traveled_routes, standard_routes)
remove(traveled_routes, standard_routes)

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
  "route_of_city" = city_route_dist) %>%
  mutate(
    "route_of_city" = if_else(
      City %in% c("San Juan PR", "Honolulu HI", "Vancouver CAN", "Bismarck ND"),
      "Not Currently On A Route", route_of_city)
    )
travels <- left_join(travels, city_route_dist, by = c("level1" = "City"))
remove(city_route_dist)

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

## generate a legends dataset
legend_data <- travels %>%
  filter(theme == "Wishlist", polygon == "LineString") %>%
  distinct(level1) %>%
  mutate(legend = "Preplanned Routes") %>%
  add_row(
    "level1" = c("Have Visited", "Intend To Visit", "Not Currently On A Route"),
    "legend" = c(rep("Routes Traveled So Far", 2), "Preplanned Routes")
    ) %>%
  arrange(desc(legend), level1) %>%
  select(legend, level1) %>%
  mutate(y = seq(from = 23.5 - 1.0, length.out = length(level1), by = -0.5)) %>%
  mutate(y = if_else(legend == "Preplanned Routes", y - 1.5, y)) %>%
  mutate(x = 29.403 + 0.2 + 0.5) %>%
  mutate(color = grab_color("sea", "dark")) %>%
  mutate(fill = grab_color("land", "light"))

## specify legend colors
i <- legend_data$legend == "Preplanned Routes"
legend_color <- seq(from = 15, to = 375, length.out = sum(i) + 1)[-(sum(i) + 1)]
legend_color <- legend_color - (legend_color %/% 360)
legend_color <- hcl(h = legend_color, c = 100, l = 40)

legend_data$color <- c(grab_color("land", "dark"), grab_color("sea", "dark"),
  legend_color)
legend_data$fill <- c(grab_color("land", "light"), grab_color("sea", "dark"),
  legend_color)

i <- which(legend_data$level1 == "Not Currently On A Route")
legend_data[ 7, c("color", "fill")] <- legend_data[i, c("color", "fill")]
legend_data[i, c("color", "fill")] <- hcl(c = 0, l = 60)

remove(legend_color)

##

## define master size scalar
master_size <- 25.4 * 0.05

## initialize ggplot object
travel_poster <- ggplot(size = 25.4*0.1) +
  coord_fixed(xlim= c(0, 36), ylim = c(0, 24), expand = FALSE, ratio = 1)

## PANEL 4: ROUTES TRAVELED ====================================================

## routes traveled: 28.403 (0.5 - 28.903) x 17.5 (6-23.5)

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
  size = master_size * 3.0
  )
travel_poster <- travel_poster + geom_path(data = routes,
  mapping = aes(x = traveled_x, y = traveled_y, group = group),
  color = grab_color("sea", "medium"),
  size = master_size *1.5
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

## render traveled routes panel title
travel_poster <- travel_poster + geom_text(
  data = tibble(NA),
  x = (0.5 + 28.903) / 2,
  y = 23.5,
  label = "Roadtrips So Far",
  color = grab_color("sea", "dark"),
  size = 24, fontface = "bold", hjust = 0.45, vjust = 1
  )

## PANEL 3: STANDARD ROUTES ====================================================

## standard routes: 8.115 (0.5-8.615) x 5 (0.5-5.5) 

## render states
states <- travels %>%
  filter(polygon == "Polygon", theme == "map") %>%
  select(group, standard_x, standard_y)
travel_poster <- travel_poster + geom_polygon(
    data = states,
    mapping = aes(x = standard_x, y = standard_y, group = group),
    color = grab_color("land", "dark"),
    fill = grab_color("land", "light"),
    size = master_size
  )
remove(states)

## render routes
routes <- travels %>%
  filter(theme == "Wishlist", polygon == "LineString") %>%
  select(level1, group, standard_x, standard_y)
travel_poster <- travel_poster +
  geom_path(data = routes,
    mapping = aes(x = standard_x, y = standard_y, group = group),
    size = master_size * 3,
    color = grab_color("land", "light")) +
  geom_path(data = routes,
    mapping = aes(x = standard_x, y = standard_y, group = group, color = level1),
    size = master_size) +
  scale_color_manual(values = set_names(legend_data$color, legend_data$level1))
remove(routes)

## render cities
cities <- travels %>%
  filter(theme == "Wishlist", polygon == "Point") %>%
  select(route_of_city, standard_x, standard_y)
travel_poster <- travel_poster + geom_point(data = cities,
  mapping = aes(x = standard_x, y = standard_y),
  size = master_size * 4, color = grab_color("land", "light")
  ) + geom_point(data = cities,
  mapping = aes(x = standard_x, y = standard_y, color = route_of_city),
  size = master_size * 3
  )
remove(cities)

## render inset panel title
travel_poster <- travel_poster + geom_text(
  data = tibble(NA),
  x = (8.615 + 0.5) / 2,
  y = 5.5,
  label = "Ten Preplanned Roadtrip Routes",
  color = grab_color("sea", "dark"),
  size = 8, fontface = "bold", hjust = 0.45
  )


## PANEL 2: PROGRESS BAR =======================================================

## progress bar: 26.385 (9.115-35.5) x 5 (0.5-5.5)

## render bars
travel_poster <- travel_poster + geom_rect(
  data = progress_bars,
  mapping = aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin_state,
    ymax = ymax_state
    ),
    fill = if_else(
      progress_bars$progress_state,
      grab_color("sea", "dark"), grab_color("sea", "medium")),
    color = grab_color("sea", "dark"),
  size = 2
  ) + geom_rect(
  data = progress_bars,
  mapping = aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin_city,
    ymax = ymax_city
    ),
    fill = if_else(
      progress_bars$progress_city,
      grab_color("sea", "dark"), grab_color("sea", "medium")),
    color = grab_color("sea", "dark"),
    size = 2
  )

## render bar labels
travel_poster <- travel_poster + geom_text(
    data = tibble(NA),
    x = min(progress_bars$xmin) - 0.4,
    y = unique(progress_bars$ymin_state) + 0.5, 
    label = "States (And\nEquiv.) Visited:",
    color = grab_color("sea", "dark"),
    hjust = 1, fontface = "bold", size = 12
  ) + geom_text(
    data = tibble(NA),
    x = min(progress_bars$xmin) - 0.4,
    y = unique(progress_bars$ymin_city) + 0.5,
    label = "Metropolitan\nAreas Visited:",
    color = grab_color("sea", "dark"),
    hjust = 1, fontface = "bold", size = 12
  ) + geom_text(
    data = tibble(NA),
    x = max(progress_bars$xmax) + 0.4,
    y = unique(progress_bars$ymin_state) + 0.5,
    label = paste0(round(progress["Percent", "state"] * 100), "%"),
    color = grab_color("sea", "dark"),
    hjust = 0, fontface = "bold", size = 20
  ) + geom_text(
    data = tibble(NA),
    x = max(progress_bars$xmax) + 0.4,
    y = unique(progress_bars$ymin_city) + 0.5,
    label = paste0(round(progress["Percent", "city"] * 100), "%"),
    color = grab_color("sea", "dark"),
    hjust = 0, fontface = "bold", size = 20
  ) + geom_text(
    data = tibble(NA),
    x = (9.115 + 35.5) / 2,
    y = unique(progress_bars$ymax_city) + 1.0,
    label = "Progress Towards Visiting 104 Metropolitan Areas Across the US and Canada",
    color = grab_color("sea", "dark"),
    hjust = 0.5, fontface = "bold", size = 16
  )
remove(progress, progress_bars)

## PANEL 1: LEGEND =============================================================

## map legend: 6.098 (29.403-35.5) x 17.5 (6-23.5)

## render legend text
travel_poster <- travel_poster + geom_text(
    data = filter(legend_data, legend == "Preplanned Routes"),
    mapping = aes(x = unique(x) - 0.75, y = max(y) - mean(diff(y)) * 1.5),
    hjust = 0,
    fontface = "bold", size = 12,
    label = "Key: Preplanned Routes",
    color = grab_color("sea", "dark")
  ) + geom_text(
    data = filter(legend_data, legend == "Preplanned Routes"),
    mapping = aes(x = x, y = y, label = level1, color = color),
    hjust = 0,
    fontface = "bold", size = 7.5,
    color = grab_color("sea", "dark")
  ) + geom_text(
    data = filter(legend_data, legend != "Preplanned Routes"),
    mapping = aes(x = unique(x) - 0.75, y = max(y) - mean(diff(y)) * 1.5),
    hjust = 0,
    fontface = "bold", size = 12,
    label = "Key: Roadtrips So Far",
    color = grab_color("sea", "dark")
  ) + geom_text(
    data = filter(legend_data, legend != "Preplanned Routes"),
    mapping = aes(x = x, y = y, label = level1, color = color),
    hjust = 0,
    fontface = "bold", size = 7.5,
    color = grab_color("sea", "dark")
    )

## render legend objects
travel_poster <- travel_poster + geom_point(
  data = legend_data,
  mapping = aes(x = x - 0.5, y = y),
  color = legend_data$color, fill = legend_data$fill,
  shape = 21, size = master_size * 5, stroke = master_size * 2
  )

## render explanatory text

travel_poster <- travel_poster + geom_text(
  data = summarize(legend_data,
    x = min(x) - 0.75, y = min(y) + mean(diff(y)) * 1.5),
  mapping = aes(x = x, y = y),
  label = paste(
    "This poster depicts information about my roadtrips.",
    "",
    "The \"Roadtrips So Far\" panel maps where I have traveled",
    "so far, as well as the other metropolitan areas I strive",
    "to visit. Taken together, I aim to see 104 metropolitan",
    "areas across the US and Canada.",
    "",
    "  The \"Preplanned Routes\" panel outline ten plans for",
    "two-week roadtrips. Taken together, those ten routes pass",
    "through 100 of the 104 metropolitan areas",
    "",
    "  The \"Progress\" panel measures my progress towards",
    "visiting all 104 areas.",
    "",
    "  When visiting an area, I generally plan a walking route",
    "through the core city, striving to see downtowns, historic",
    "districts, and other noteworthy sites.  I may also plan side",
    "driving excursions to see remote sites, especially UNESCO",
    "World Heritage Sites.",
    "",
    "  The selection criteria for the metropolitan areas favored",
    "those that held state capitals, state's largest city,",
    "significant historic / cultural sites, or otherwise",
    "underrepresented geographic areas.",
    sep = "\n"
    ),
  size = 6, hjust = 0, vjust = 1, color = grab_color("sea", "dark")
  )

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
  color = grab_color("sea", "dark"), size= 2
  ) + theme(
    plot.margin = unit(rep(0, 4), units = "in"),
    axis.text = element_blank(), axis.line = element_blank(),
    axis.ticks = element_blank(), axis.title = element_blank(),
    panel.background = element_rect(fill = grab_color("sea", "light")),
    panel.grid = element_blank(), panel.border = element_blank(),
    legend.position = "none"
    )
remove(dividing_lines)

## EXPORT TO PDF ===============================================================

pdf("C_Outputs/road_trip_poster.pdf", width = 36, height = 24)
travel_poster
graphics.off()

##########==========##########==========##########==========##########==========
