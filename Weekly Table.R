library(tidyverse)
library(nflfastR)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggrepel)
library(ggimage)
library(gt)

o_and_d_forecast <- merge(final_predictions_2020, defense_predictions_2020, by.x='team', by.y='home_team')

o_and_d_forecast <- o_and_d_forecast %>%
  select(team, pred_points_per_game, pred_d_ppg, team_nick, team_logo_espn)

o_and_d_forecast %>%
  ggplot(aes(x = pred_points_per_game, y = pred_d_ppg)) +
  geom_hline(yintercept = mean(o_and_d_forecast$pred_d_ppg), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(o_and_d_forecast$pred_points_per_game), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  geom_abline(slope = -1.5, intercept = c(0, 3, 6, 9, 12, 15, 18, 21, 24), alpha = .4) +
  labs(x = "Average Offensive Forecasted Points",
       y = "Average Defensive Forecasted Points",
       title = "NFL Tiers Based on Post-Game Forecasted Points",
       subtitle = "The forecasted points are determined using advanced statistics from each game, weeks 1-13",
       caption = "Graph by Tej Seth | @mfbanalytics") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('tiers_13.png', dpi=300)


wild_card_guide <- wild.card %>%
  gt() %>%
    tab_header(title = html("<strong>NFL Super Wild Card Weekend Preview</strong>"),
    subtitle = md("Only responsible for the bets you win, not responsible for the bets you lose")) %>%
  cols_label(date_time = "Date and Time", away_team = "Away Team",
             proj_a_points = "Projected Points", home_team = "Home Team", proj_home_points = "Projected Points",
             spread = "Vegas Spread", team_to_bet = "Team to Bet", bet_str = "Bet Strength", 
             vegas_over_under = "Over/Under", total_proj_points = "Total Projected Points") %>%
  data_color(
    columns = vars(bet_str), 
    colors = scales::col_factor(
      palette = c(
        "dodgerblue2","navy", "skyblue1"),
      domain = c("Weak","Medium", "Strong"))) %>%
  data_color(
    columns = vars(date_time), 
    colors = scales::col_factor(
      palette = c(
        "grey20","lightblue1", "lightblue2", "lightblue3", "lightblue4", "plum1"),
      domain = c("Thursday, 8:20 PM","Sunday, 1:00 PM", "Sunday, 4:05 PM", 
                 "Sunday, 4:25 PM", "Sunday, 8:20 PM", "Monday, 8:15 PM"))) %>%
  tab_source_note(md("Model and table by Tej Seth: Using weeks 1-13 of a linear regression forecasted points model, 
                      which is modeled based on advanced post-game statistics,
                     this table attempts to project the score to each NFL game for week 14")) %>%
  text_transform(locations = cells_body(vars(away_team)),
                 fn = function(x) {
                   web_image(url = week.13.betting.guide$away_team,
                   height = px(40))}) %>%
  text_transform(locations = cells_body(vars(home_team)),
                 fn = function(x) {
                   web_image(url = week.13.betting.guide$home_team,
                             height = px(40))}) %>%
  text_transform(locations = cells_body(vars(team_to_bet)),
                 fn = function(x) {
                   web_image(url = week.13.betting.guide$team_to_bet,
                             height = px(40))}) %>%
  data_color(
    columns = vars(total_proj_points),
    colors = scales::col_numeric(
      palette = c(
        "indianred1","lightgoldenrod1", "darkolivegreen1"), 
      domain = c(42,57))) %>%
  cols_align(
    align = "center")
week_13_guide
gtsave(week_13_guide, "week_13_guide.png")


############################################################################
week_14_guide <- week.14.guide %>%
  gt() %>%
  tab_header(title = html("<strong>NFL Week 14 Preview</strong>"),
             subtitle = md("")) %>%
  cols_label(date_time = "Date and Time", away_team = "Away Team",
             proj_a_points = "Projected Points", home_team = "Home Team", proj_home_points = "Projected Points",
             projected_winner = "Projected Winner", tv_channel = "TV Channel") %>%
  data_color(
    columns = vars(date_time), 
    colors = scales::col_factor(
      palette = c(
        "grey20","lightblue1", "lightblue2", "lightblue3", "lightblue4", "plum1"),
      domain = c("Thursday, 8:20 PM","Sunday, 1:00 PM", "Sunday, 4:05 PM", 
                 "Sunday, 4:25 PM", "Sunday, 8:20 PM", "Monday, 8:15 PM"))) %>%
  tab_source_note(md("Model and table by Tej Seth: Using weeks 1-13 of a linear regression forecasted points model, 
                      which is modeled based on advanced post-game statistics,
                     this table attempts to project the score to each NFL game for week 14")) %>%
  text_transform(locations = cells_body(vars(away_team)),
                 fn = function(x) {
                   web_image(url = week.14.guide$away_team,
                             height = px(40))}) %>%
  text_transform(locations = cells_body(vars(home_team)),
                 fn = function(x) {
                   web_image(url = week.14.guide$home_team,
                             height = px(40))}) %>%
  text_transform(locations = cells_body(vars(projected_winner)),
                 fn = function(x) {
                   web_image(url = week.14.guide$projected_winner,
                             height = px(40))}) %>%
  data_color(
    columns = vars(tv_channel), 
    colors = scales::col_factor(
      palette = c(
        "firebrick4","chocolate4", "gray0", "darkblue"),
      domain = c("FOX", "CBS", "NBC", "ESPN"))) %>%
  cols_align(
    align = "center")
week_14_guide
gtsave(week_14_guide, "week_14_guide.png")

############################################################################
week.15.betting.guide <- week.14.betting.guide %>%
  select(date_time, away_team, proj_a_points, home_team, proj_home_points, projected_winner, abs_diff, total_proj)

week.15.betting.guide <- week.15.betting.guide %>%
  filter(!is.na(proj_a_points))

wild_card_guide <- wild.card %>%
  gt() %>%
  tab_header(title = html("<strong>NFL Super Wild Card Weekend Preview</strong>"),
             subtitle = md("")) %>%
  cols_label(date_time = "Date and Time", away_team = "Away Team",
             proj_a_points = "Projected Points", home_team = "Home Team", proj_home_points = "Projected Points",
             projected_winner = "Projected Winner", proj_diff = "Projected Diff.", vegas_winner = "Vegas Winner", vegas_diff = "Vegas Proj. Diff.") %>%
  data_color(
    columns = vars(date_time), 
    colors = scales::col_factor(
      palette = c(
        "green3", "forestgreen", "darkgreen", "skyblue", "skyblue3", "midnightblue"),
      domain = c("Saturday, 1:05 PM", "Saturday, 4:40 PM", "Saturday, 8:15 PM", "Sunday, 1:05 PM", "Sunday, 4:40 PM", "Sunday, 8:15 PM"))) %>%
  tab_source_note(md("Model and table by Tej Seth: Using weeks 1-16 of a linear regression forecasted points model, 
                      which is modeled based on advanced post-game statistics,
                     this table attempts to project the score to each NFL game for super wild card weekend")) %>%
  text_transform(locations = cells_body(vars(away_team)),
                 fn = function(x) {
                   web_image(url = wild.card$away_team,
                             height = px(45))}) %>%
  text_transform(locations = cells_body(vars(home_team)),
                 fn = function(x) {
                   web_image(url = wild.card$home_team,
                             height = px(45))}) %>%
  text_transform(locations = cells_body(vars(projected_winner)),
                 fn = function(x) {
                   web_image(url = wild.card$projected_winner,
                             height = px(45))}) %>%
  text_transform(locations = cells_body(vars(vegas_winner)),
                 fn = function(x) {
                   web_image(url = wild.card$vegas_winner,
                             height = px(45))}) %>%
  data_color(
    columns = vars(proj_diff),
    colors = scales::col_numeric(
      palette = c(
        "darkolivegreen1", "lightgoldenrod1", "indianred1"), 
      domain = c(0,10))) %>%
  data_color(
    columns = vars(vegas_diff),
    colors = scales::col_numeric(
      palette = c(
        "darkolivegreen1", "lightgoldenrod1", "indianred1"), 
      domain = c(0,10))) %>%
  cols_align(
    align = "center")
wild_card_guide

gtsave(wild_card_guide, "wild_card_guide.png")







