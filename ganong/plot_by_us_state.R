library(lubridate)
library(tidyverse)

path <- "csse_covid_19_data/csse_covid_19_daily_reports/03-"

read_day <- function(date, state, state_var_name_, metric_var_name_) {
  read_csv(str_c(path, date, "-2020.csv")) %>%
    filter({{state_var_name_}} == state) %>%
    summarise(n = sum({{metric_var_name_}})) %>%
    mutate(day = dmy(str_c(date, " March 2020")))
}

read_month <- function(state, ...) {
  bind_rows(
    #note: days before March 10th do not to have state-level data
    #without additional cleaning first
    seq(10, 21) %>% map_dfr(read_day, state, `Province/State`, ...),
    seq(22, 25) %>% map_dfr(read_day, state, Province_State, ...)
  ) %>%
    mutate(state = state)
}

df <- c("New York", "Illinois", "Washington", "Florida") %>%
  map_dfr(read_month, metric_var_name_ = Confirmed)

df_deaths <- c("New York", "Illinois", "Washington", "Florida") %>%
  map_dfr(read_month, metric_var_name_ = Deaths)

(df %>%
  ggplot(aes(x = day, y = n, group = state, color = state)) +
  geom_line() +
  labs(x = "", y = "n positive tests", title = "Covid cases by state") +
  scale_y_log10(breaks = c(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000), limits = c(3, NA)) +
  scale_x_date(date_breaks = "3 days",
               date_labels = "%b %d")) %>%
  ggsave("ganong/state_level_plot.png", .)

(df_deaths %>%
    ggplot(aes(x = day, y = n, group = state, color = state)) +
    geom_line() +
    labs(x = "", y = "n deaths", title = "Covid deaths by state") +
    scale_y_log10(breaks = c(3, 10, 30, 100, 300, 1000, 3000, 10000, 30000), limits = c(3, NA)) +
    scale_x_date(date_breaks = "3 days",
                 date_labels = "%b %d")) %>%
  ggsave("ganong/state_level_deaths_plot.png", .)

