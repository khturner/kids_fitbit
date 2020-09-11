library(tidyverse)
library(lubridate)

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

toplot <- alldata %>%
  # Restrict age range
  filter(start_week < 9) %>%
  # No naps to make the block viz cleaner - start time after 5pm, end time before 8am
  filter(hour(start_time) > 5, hour(end_time) < 20)

# Get loess smoothed sleep per night
toplot_smoothed <- toplot %>%
  select(kid, start_week, sleep_per_night) %>% unique %>%
  group_by(kid) %>%
  nest %>%
  mutate(smoothed_sleep_per_night = map(data, function(d) {
    loess(sleep_per_night ~ start_week, data = d,
          span = (22-max(d$start_week)) / 22) %>% predict
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
  ggtitle("Dad's sleep with a newborn in the house", "Data from Fitbit") +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_rect(aes(ymax = -(hour(start_time) * 60 + minute(start_time)),
                ymin = -(hour(end_time) * 60 + minute(end_time)),
                xmin = start_week - (0.5 / 7),
                xmax = start_week + (0.5 / 7),
                fill = fill_color)) +
  scale_fill_distiller(palette = "RdYlBu", guide = F) +
  scale_y_continuous("Time asleep",
                     breaks = -1440 + seq(0, 1440, 180),
                     minor_breaks = -1440 + seq(0, 1440, 60),
                     labels = c("12 PM", "9 AM", "6 AM", "3 AM", "12 AM",
                                "9 PM", "6 PM", "3 PM", "12 PM")) +
  scale_x_continuous("Kid's age (weeks)", minor_breaks = -2:20) +
  theme_dark() +
  facet_wrap(~kid, ncol = 1)

ggsave("plots/intervals.png", ints, height = 6, width = 8)

# Nightly sleep
nightly <- toplot_smoothed %>%
  select(kid, start_week, smoothed_sleep_per_night, sleep_per_night, fill_color) %>%
  unique %>%
  ggplot(aes(start_week, color = fill_color)) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_point(aes(y = sleep_per_night / 60), size = 2, alpha = 0.5) +
  geom_line(aes(y = smoothed_sleep_per_night / 60, group = kid), size = 2, lineend = "round") +
  scale_color_distiller("Kid", palette = "RdYlBu",
                        breaks = c(-1, 1), labels = c("Nathaniel", "Henry")) +
  scale_y_continuous("Sleep per night (hours)",
                     limit = c(0, 9), breaks = seq(0, 10, 2)) +
  scale_x_continuous("Kid's age (weeks)",
                     limits = ggplot_build(ints)$layout$panel_scales_x[[1]]$range$range,
                     minor_breaks = -2:20) +
  theme_dark() +
  ggtitle("Dad's sleep with a newborn in the house", "Data from Fitbit")

ggsave("plots/nightly.png", nightly, height = 6, width = 8)

# Longest block of sleep
longest_block <- toplot_smoothed %>%
  group_by(kid, start_week) %>%
  arrange(desc(minutes_asleep)) %>%
  filter(row_number() == 1) %>%
  group_by(kid) %>%
  nest %>%
  mutate(smoothed_longest_block = map(data, function(d) {
    loess(minutes_asleep ~ start_week, data = d,
          span = (22-max(d$start_week)) / 22) %>% predict
  })) %>%
  unnest(-kid) %>% ungroup %>%
  ggplot(aes(start_week, color = kid)) +
  ggtitle("Dad's sleep with a newborn in the house", "Data from Fitbit") +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
  geom_point(aes(y = minutes_asleep / 60), size = 2, alpha = 0.6) +
  geom_line(aes(y = smoothed_longest_block / 60, group = kid), size = 2) +
  scale_color_manual("Kid", values = c("#fc8d59", "#91bfdb")) +
  scale_y_continuous("Longest stretch of sleep (hours)",
                     limit = c(0, 9), breaks = seq(0, 10, 2)) +
  scale_x_continuous("Kid's age (weeks)",
                     limits = ggplot_build(ints)$layout$panel_scales_x[[1]]$range$range,
                     minor_breaks = -2:20) +
  theme_dark()

ggsave("plots/longest_block.png", longest_block, height = 6, width = 8)

# Snoo data
# $ snoo days -s 2020-07-19 > snoo_daily.csv
snoo <- read_csv("snoo_data/snoo_daily.csv")

# Night sleep
snoo_night <- snoo %>%
  mutate(kid_age = yday(date) - 201) %>%
  filter(nightSleep > 0) %>%
  ggplot(aes(kid_age / 7, nightSleep / (60*60))) +
  geom_point(color = "#fc8d59") +
  geom_smooth(se = F,
              color = "#fc8d59") +
  theme_bw() +
  labs(title = "Henry's total night sleep in the Snoo",
       y = "Hours asleep between 7pm and 7am",
       x = "Henry's age (weeks)")

ggsave("plots/snoo_night.png", snoo_night, height = 4, width = 6)

snoo_longest <- snoo %>%
  mutate(kid_age = yday(date) - 201) %>%
  filter(nightSleep > 0) %>%
  ggplot(aes(kid_age / 7, longestSleep / (60*60))) +
  geom_point(color = "#fc8d59") +
  geom_smooth(se = F,
              color = "#fc8d59") +
  theme_bw() +
  labs(title = "Henry's longest stretch of sleep in the Snoo",
       y = "Hours asleep",
       x = "Henry's age (weeks)")

ggsave("plots/snoo_longest.png", snoo_longest, height = 4, width = 6)
