library(tidyverse)
library(baseballr)
library(ggthemes)
library(forecast)
library(showtext)
library(mgsub)

### Loading games from 2017 - 2022

Tm <- c("OAK", "PIT", "SD",  "SEA", "SF",  "STL", "TB",  "TEX", "TOR", "MIN",
        "PHI", "ATL", "CHW", "MIA", "NYY", "MIL", "LAA", "ARI", "BAL", "BOS",
        "CHC", "CIN", "CLE", "COL", "DET", "HOU", "KC",  "LAD", "WSN", "NYM")

bref_team_results_2017_2022 <- function(Tm) {
  
  bref_team_results_2017 <- function(Tm) {
    bref_team_results(Tm = Tm, year = 2017)
  }
  
  bref_team_results_2018 <- function(Tm) {
    bref_team_results(Tm = Tm, year = 2018)
  }
  
  bref_team_results_2019 <- function(Tm) {
    bref_team_results(Tm = Tm, year = 2019)
  }
  
  bref_team_results_2020 <- function(Tm) {
    bref_team_results(Tm = Tm, year = 2020)
  }
  
  bref_team_results_2021 <- function(Tm) {
    bref_team_results(Tm = Tm, year = 2021)
  }
  
  bref_team_results_2022 <- function(Tm) {
    bref_team_results(Tm = Tm, year = 2022)
  }
  
  df_2017 <- lapply(Tm, bref_team_results_2017) %>%
    bind_rows()
  
  df_2018 <- lapply(Tm, bref_team_results_2018) %>%
    bind_rows()
  
  df_2019 <- lapply(Tm, bref_team_results_2019) %>%
    bind_rows()
  
  df_2020 <- lapply(Tm, bref_team_results_2020) %>%
    bind_rows()
  
  df_2021 <- lapply(Tm, bref_team_results_2021) %>%
    bind_rows()
  
  df_2022 <- lapply(Tm, bref_team_results_2022) %>%
    bind_rows()
  
  df_master <- rbind(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022) %>%
    bind_rows()
  
  df_master$Tm <- as.factor(df_master$Tm)
  
  df_master$Opp <- as.factor(df_master$Opp)
  
  df_master$Result <- df_master$Result %>%
    gsub(pattern = "-wo", replacement = "") %>%
    as.character()
  
  df_master$Date <- df_master$Date %>%
    gsub(pattern = "\\s*\\([^\\)]+\\)", replacement = "") %>%
    mgsub(pattern = c("Monday, ", "Tuesday, ", "Wednesday, ", "Thursday, ", "Friday, ", "Saturday, ", "Sunday, ", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), 
          c("", "", "", "", "", "", "", "March", "April", "May", "June", "July", "August", "September", "October"))
  
  df_master$DateFull <- paste(df_master$Date, df_master$Year, sep = " ") %>%
    as.Date(format = "%B %d %Y")
  
  extractWin <- function(Result) {
    if(length(grep("W", Result)) > 0) {
      return(1)
    } else if(length(grep("L", Result)) > 0) {
      return(0)
    } else {
      return("")
    }
  }
  
  Wins <- NULL
  for (i in 1:nrow(df_master)) {
    Wins <- c(Wins, extractWin(df_master[i,"Result"]))
  }
  
  Wins <- as.numeric(Wins)
  
  df_master <- cbind(df_master, Wins)
  
  df_master <- df_master %>%
    group_by(Tm, Year) %>%
    mutate("wins_so_far" = cumsum(Wins))
  
  df_master
}

df1 <- lapply(Tm, bref_team_results_2017_2022) %>%
  bind_rows()

### Loading existing FanGraph Pitcher IDs

pitcher_id_csv <- read_csv("pitcherid.csv")

pitcher_ids <- pitcher_id_csv$x

fg_pitcher_glog_2022 <- function(pitcher) {
  df <- fg_pitcher_game_logs(pitcher, 2022)
  df$Date <- df$Date %>%
    as.Date(format = "%Y - %m - %d")
  df$Opp <- df$Opp %>%
    sub(pattern = "@", replacement = "")
  df
}

starting_pitcher_game_log <- lapply(pitcher_ids, fg_pitcher_glog_2022) %>%
  bind_rows() %>%
  filter(GS == 1)

### Loading New FanGraph Pitcher IDs

playerids <- read_csv("Data/playerids.csv") %>%
  select(PLAYERNAME, IDFANGRAPHS, POS, TEAM)

colnames(playerids) <- c("player_name", "player_id", "position", "team")

playerids$player_id <- as.numeric(playerids$player_id)

pitcher_ids_all <- filter(playerids, position == "P")

pitcher_ids <- pitcher_ids_all$player_id

fg_pitcher_glog_2022 <- function(pitcher) {
  df <- fg_pitcher_game_logs(pitcher, 2022)
  df$Date <- df$Date %>%
    as.Date(format = "%Y - %m - %d")
  df$Opp <- df$Opp %>%
    sub(pattern = "@", replacement = "")
  
  df
}

starting_pitcher_game_log <- lapply(playerid_cutdown, fg_pitcher_glog_2022) %>%
  bind_rows() %>%
  filter(GS == 1)

playerid_cutdown <- starting_pitcher_game_log$playerid

playerid_cutdown <- unique(playerid_cutdown)

write.csv(playerid_cutdown,"pitcherid.csv", row.names = FALSE)
