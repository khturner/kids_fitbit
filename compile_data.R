library(tidyverse)
library(lubridate)
library(grid)

alldata <- lapply(list.files("exports/"), function(f) {
  read_csv(paste0("exports/", f), skip = 1)
}) %>% bind_rows %>%
  unique %>%
  # Convert to POSIXct
  mutate(start_time = as.POSIXct(`Start Time`, tz = "America/Chicago",
                                 format = "%Y-%m-%d %I:%M%p"),
         end_time = as.POSIXct(`End Time`, tz = "America/Chicago",
                               format = "%Y-%m-%d %I:%M%p")) %>%
  # Set midnight to noon the day after
  mutate(start_time = start_time + 12 * 60 * 60,
         end_time = end_time + 12 * 60 * 60) %>%
  select(start_time, end_time,
         minutes_asleep = `Minutes Asleep`) %>%
  mutate(start_day = ifelse(year(start_time) == 2017,
                            yday(start_time) - 260,
                            yday(start_time) + 105), # Born 105 days before the end of 2017
         start_week = start_day / 7)
  
# Everything on the same day?
alldata %>%
  filter(day(start_time) != day(end_time))

# smooth sleep data
alldata %>%
  group_by(start_week) %>%
  summarize(sleep_per_night = sum(minutes_asleep)) %>%
  ggplot(aes(start_week, sleep_per_night)) +
  geom_point() +
  geom_smooth()

# Get loess smoothed sleep per night
rolling <- alldata %>%
  group_by(start_week) %>%
  summarize(sleep_per_night = sum(minutes_asleep))
rolling$smoothed_sleep_per_night <-
  loess(sleep_per_night ~ start_week, data = rolling, span = 0.5) %>% predict

toplot <- alldata %>%
  inner_join(rolling)

# Intervals
ints <- toplot %>%
  ggplot() +
  geom_rect(aes(ymax = -(hour(start_time) * 60 + minute(start_time)),
                ymin = -(hour(end_time) * 60 + minute(end_time)),
                xmin = start_week - (0.5 / 7),
                xmax = start_week + (0.5 / 7),
                fill = smoothed_sleep_per_night/60)) +
  scale_fill_distiller("Hours of sleep\nper night (smoothed)",
                       palette = "GnBu", direction = 1, guide = F) +
  scale_y_continuous("Time",
                     breaks = -1440 + seq(180, 1440, 180),
                     labels = c("9 AM", "6 AM", "3 AM", "12 AM",
                              "9 PM", "6 PM", "3 PM", "12 PM")) +
  scale_x_continuous("Nathaniel's age (weeks)", limits = c(2.2, max(toplot$start_week) + 1/14)) +
  theme_dark() +
  theme(legend.position = "bottom")

# Nightly sleep
nightly <- rolling %>%
  ggplot(aes(start_week, color = smoothed_sleep_per_night/60)) +
  geom_point(aes(y = sleep_per_night/60), size = 2) +
  geom_line(aes(y = smoothed_sleep_per_night/60), lineend = "round", size = 2) +
  scale_color_distiller(palette = "GnBu", direction = 1, guide = F) +
  ylab("Sleep per night (hours)") +
  scale_x_continuous(limits = c(2.2, max(toplot$start_week) + 1/14)) +
  theme_dark() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("Dad's sleep with a newborn in the house", "Data from Fitbit")

## Align two plots
grid.newpage()
grid.draw(rbind(ggplotGrob(nightly), ggplotGrob(ints), size = "last"))
