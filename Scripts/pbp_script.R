library(tidyverse)
library(baseballr)

hit <- c("home_run", "single", "double", "triple")
not_hit <- c("field_out","strikeout","walk","sac_bunt","fielders_choice","grounded_into_double_play","force_out","hit_by_pitch","sac_fly")

phi_pbp_data <- function(Date, Team) {
  
  game_pk_table <- mlb_game_pks(Date, level_ids = c(1)) %>%
    filter_all(any_vars(. %in% c(Team)))
  
  game_pk <- game_pk_table$game_pk
  
  df_pbp <- mlb_pbp(game_pk) %>%
    filter(isPitch == "TRUE") %>%
    select(matchup.pitcher.fullName, matchup.batter.fullName, count.outs.end, last.pitch.of.ab, pitchNumber, atBatIndex, result.event, result.eventType, fielding_team) %>%
    group_by(atBatIndex) %>%
    filter(pitchNumber == max(pitchNumber))
  
  df_pbp$atBatIndex <- df_pbp$atBatIndex %>%
    as.numeric()
  
  df_pbp$pitchNumber <- as.numeric(df_pbp$pitchNumber)
  
  df_pbp$result.eventType <- as.factor(df_pbp$result.eventType)
  
  df_pbp <- df_pbp[order(df_pbp$atBatIndex),] %>%
    group_by(matchup.batter.fullName) %>%
    mutate("atbatNumber" = 1:n()) %>%
    group_by(result.eventType)
}

df_pbp <- phi_pbp_data("2022-06-12", "Philadelphia Phillies")

### Team hits
df_pbp %>%
  filter(atbatNumber > 0 & result.eventType %in% hit & fielding_team == "Philadelphia Phillies") %>%
  count()

### Opponent hits
df_pbp %>%
  filter(atbatNumber > 0 & result.eventType %in% hit & fielding_team != "Philadelphia Phillies") %>%
  count()
