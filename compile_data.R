library(tidyverse)
library(lubridate)
library(grid)

# Read in Fitbit exports
alldata <- lapply(list.files("exports/"), function(f) {
  read_csv(paste0("exports/", f), skip = 1)
}) %>% bind_rows %>%
  unique %>%
  # Convert times to POSIXct
  mutate(start_time = as.POSIXct(`Start Time`, tz = "America/Chicago",
                                 format = "%Y-%m-%d %I:%M%p"),
         end_time = as.POSIXct(`End Time`, tz = "America/Chicago",
                               format = "%Y-%m-%d %I:%M%p")) %>%
  # Set midnight to noon the day after
  mutate(start_time = start_time + 12 * 60 * 60,
         end_time = end_time + 12 * 60 * 60,
         # Another kid!
         kid = ifelse(year(start_time) > 2019, "Henry", "Nathaniel")) %>%
  select(kid, start_time, end_time, minutes_asleep = `Minutes Asleep`)

# Split naps that cross noon into two days
alldata <- alldata %>%
  filter(day(start_time) != day(end_time)) %>%
  # First half
  mutate(end_time = as.POSIXct(paste(as.Date(start_time)-1, "11:59PM"),
                               tz = "America/Chicago",
                               format = "%Y-%m-%d %I:%M%p"),
         minutes_asleep = as.integer(end_time - start_time)) %>%
  # Second half
  bind_rows(alldata %>%
              filter(day(start_time) != day(end_time)) %>%
              mutate(start_time = as.POSIXct(paste(as.Date(end_time), "12:01AM"),
                                           tz = "America/Chicago",
                                           format = "%Y-%m-%d %I:%M%p"),
                     minutes_asleep = as.integer(end_time - start_time))) %>%
  # All other naps
  bind_rows(alldata %>% filter(day(start_time) == day(end_time)))

# Adjust start day to be kid's age
alldata <- alldata %>%
  mutate(start_day = case_when(
    # Nathaniel was born 105 days before the end of 2017
    kid == "Nathaniel" ~
      ifelse(year(start_time) == 2017,
             yday(start_time) - 260,
             yday(start_time) + 105),
    # Henry was born on the 201st day of the year
    kid == "Henry" ~ 
      yday(start_time) - 201),
    start_week = start_day / 7)

# How much sleep per night?
alldata <- alldata %>%
  group_by(kid, start_week) %>%
  mutate(sleep_per_night = sum(minutes_asleep)) %>%
  ungroup %>%
  arrange(start_week)

## PLOTTING ##

# Restrict age range
toplot <- alldata %>%
  filter(start_week < 6)

# Get loess smoothed sleep per night
toplot_smoothed <- toplot %>%
  select(kid, start_week, sleep_per_night) %>% unique %>%
  group_by(kid) %>%
  nest %>%
  mutate(smoothed_sleep_per_night = map(data, function(d) {
    loess(sleep_per_night ~ start_week, data = d, span = 0.5) %>% predict
  }),
  fill_color = map(smoothed_sleep_per_night, function(s) {
    scales::rescale(s, c(0.2, 1))
  })) %>%
  unnest(-kid) %>% ungroup %>%
  mutate(fill_color = ifelse(kid == "Nathaniel", -fill_color, fill_color)) %>%
  inner_join(toplot, .)

# Intervals - faceted
ints <- toplot_smoothed %>%
  ggplot() +
  geom_rect(aes(ymax = -(hour(start_time) * 60 + minute(start_time)),
                ymin = -(hour(end_time) * 60 + minute(end_time)),
                xmin = start_week - (0.5 / 7),
                xmax = start_week + (0.5 / 7),
                fill = fill_color)) +
  scale_fill_distiller(palette = "RdYlBu", guide = F) +
  scale_y_continuous("Time",
                     breaks = -1440 + seq(0, 1440, 180),
                     labels = c("12 PM", "9 AM", "6 AM", "3 AM", "12 AM",
                                "9 PM", "6 PM", "3 PM", "12 PM")) +
  scale_x_continuous("Kid's age (weeks)") +
  theme_dark() +
  facet_wrap(~kid, ncol = 1)

# Nightly sleep
nightly <- toplot_smoothed %>%
  select(kid, start_week, smoothed_sleep_per_night, sleep_per_night, fill_color) %>%
  ggplot(aes(start_week, color = fill_color)) +
  geom_point(aes(y = sleep_per_night / 60), size = 2) +
  geom_line(aes(y = smoothed_sleep_per_night / 60, group = kid), size = 2, lineend = "round") +
  scale_color_distiller(palette = "RdYlBu", guide = F) +
  ylab("Sleep per night (hours)") +
  scale_x_continuous(limits = ggplot_build(ints)$layout$panel_scales_x[[1]]$range$range) +
  theme_dark() +
  ggtitle("Dad's sleep with a newborn in the house", "Data from Fitbit") +
  theme(axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank())

## Align two plots
grid.newpage()
grid.draw(rbind(ggplotGrob(nightly), ggplotGrob(ints), size = "last"))
