library(tidyverse)
library(nflfastR)

games <- readRDS(url("http://www.habitatring.com/games.rds"))

home <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team)

away <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result)

results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )

team_wins <- results %>%
  group_by(team, season) %>%
  summarize(
    wins = sum(win),
    point_diff = sum(result)) %>%
  ungroup()

seasons <- 2012:2020
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(rush == 1 | pass == 1, week <= 18, !is.na(epa), !is.na(posteam), posteam != "") %>%
    select(season, posteam, pass, defteam, epa)
})

pbp %>%
  group_by(posteam, season, pass) %>%
  summarize(epa = mean(epa))

pbp %>%
  group_by(posteam, season, pass) %>%
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa)

offense <- pbp %>%
  group_by(posteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(off_pass_epa = `1`, off_rush_epa = `0`)

defense <- pbp %>%
  group_by(defteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(def_pass_epa = `1`, def_rush_epa = `0`)

team_wins %>%
  group_by(team) %>%
  summarize(n=n()) %>%
  arrange(n)

team_wins %>%
  group_by(team) %>%
  summarize(n=n()) %>%
  arrange(n)

team_wins <- team_wins %>%
  mutate(
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

team_wins %>%
  group_by(team) %>%
  summarize(n=n()) %>%
  arrange(n)

data <- team_wins %>%
  left_join(offense, by = c('team' = 'posteam', 'season')) %>%
  left_join(defense, by = c('team' = 'defteam', 'season')) %>%
  filter(!is.na(off_rush_epa)) %>%
  filter(!is.na(wins))

data <- data %>%
  arrange(team, season) %>%
  mutate(
    prior_off_rush_epa = lag(off_rush_epa),
    prior_off_pass_epa = lag(off_pass_epa),
    prior_def_rush_epa = lag(def_rush_epa),
    prior_def_pass_epa = lag(def_pass_epa),
    prior_point_diff = lag(point_diff)
  )

data %>% 
  select(-team, -season) %>%
  cor(use="complete.obs") %>%
  round(2)

data <- data %>% filter(season >= 2009)

fit <- lm(wins ~ prior_off_pass_epa  + prior_off_rush_epa + prior_def_pass_epa + prior_def_rush_epa, data = data)

summary(fit)

fit2 <- lm(wins ~ prior_point_diff, data = data)

summary(fit2)

preds <- predict(fit, data %>% filter(season == 2020)) %>%
  as_tibble() %>%
  rename(prediction = value) %>%
  round(1) %>%
  bind_cols(
    data %>% filter(season == 2020) %>% select(team))


