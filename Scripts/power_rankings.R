library(tidyverse)
library(baseballr)
library(ggthemes)
library(imputeTS)

powerrankings_table <- function() {

df_hitters <- bref_daily_batter(as.Date(Sys.Date() - 10), as.Date(Sys.Date() - 1)) %>%
  group_by(Level, Team) %>%
  filter(Team != "Oakland,Tampa Bay") %>%
  summarise(PA = sum(PA),
            AB = sum(AB),
            H = sum(H),
            R = sum(R),
            X1B = sum(X1B),
            X2B = sum(X2B),
            X3B = sum(X3B),
            HR = sum(HR),
            RBI = sum(RBI),
            BB = sum(BB),
            SO = sum(SO),
            GDP = sum(GDP),
            HBP = sum(HBP),
            OBP = (sum(H,BB,HBP)) / (PA),
            SLG = (sum((X1B),(X2B*2),(X3B*3),(HR*4))/AB),
            OPS = (sum(OBP,SLG)))

df_starting_pitchers <- bref_daily_pitcher(as.Date(Sys.Date() - 10), as.Date(Sys.Date() - 1)) %>%
  na_replace(fill = 0) %>%
  group_by(Level, Team) %>%
  filter(GS > 0) %>%
  summarise(H.allowed = sum(H),
            R.allowed = sum(R),
            ER.allowed = sum(ER),
            BB.allowed = sum(BB))

df_bullpen <- bref_daily_pitcher(as.Date(Sys.Date() - 10), as.Date(Sys.Date() - 1)) %>%
  na_replace(fill = 0) %>%
  group_by(Level, Team) %>%
  filter(GS == 0) %>%
  summarise(H.allowed.bullpen = sum(H),
            R.allowed.bullpen = sum(R),
            ER.allowed.bullpen = sum(ER),
            BB.allowed.bullpen = sum(BB))

df_wins <- bref_daily_pitcher(as.Date(Sys.Date() - 10), as.Date(Sys.Date() - 1)) %>%
  na_replace(fill = 0) %>%
  group_by(Level, Team) %>%
  summarise(Wins = sum(W))

### All season

df_hitters_all <- bref_daily_batter(as.Date("2022-04-07"), as.Date(Sys.Date() - 1)) %>%
  group_by(Level, Team) %>%
  summarise(PA_all = sum(PA),
            AB_all = sum(AB),
            H_all = sum(H),
            R_all = sum(R),
            X1B_all = sum(X1B),
            X2B_all = sum(X2B),
            X3B_all = sum(X3B),
            HR_all = sum(HR),
            RBI_all = sum(RBI),
            BB_all = sum(BB),
            SO_all = sum(SO),
            GDP_all = sum(GDP),
            HBP_all = sum(HBP))

df_starting_pitchers_all <- bref_daily_pitcher(as.Date("2022-04-07"), as.Date(Sys.Date() - 1)) %>%
  na_replace(fill = 0) %>%
  group_by(Level, Team) %>%
  filter(GS > 0) %>%
  summarise(H.allowed_all = sum(H),
            R.allowed_all = sum(R),
            ER.allowed_all = sum(ER),
            BB.allowed_all = sum(BB))

df_bullpen_all <- bref_daily_pitcher(as.Date("2022-04-07"), as.Date(Sys.Date() - 1)) %>%
  na_replace(fill = 0) %>%
  group_by(Level, Team) %>%
  filter(GS == 0) %>%
  summarise(H.allowed.bullpen_all = sum(H),
            R.allowed.bullpen_all = sum(R),
            ER.allowed.bullpen_all = sum(ER),
            BB.allowed.bullpen_all = sum(BB))

df_wins_all <- bref_daily_pitcher(as.Date("2022-04-07"), as.Date(Sys.Date() - 1)) %>%
  na_replace(fill = 0) %>%
  group_by(Level, Team) %>%
  summarise(Wins_all = sum(W))

df_list <- list(df_hitters,df_starting_pitchers,df_bullpen,df_wins,
                df_hitters_all,df_starting_pitchers_all,df_bullpen_all,df_wins_all)

df_powerrankings <- df_list %>%
  reduce(full_join, by = c("Team","Level"))

df_powerrankings <- df_powerrankings %>%
  mutate(Tm = ifelse(Team == "Baltimore", "BAL",
         ifelse(Team == "Boston", "BOS",
         ifelse(Team == "Chicago" & Level == "Maj-AL", "CHW",
         ifelse(Team == "Cleveland", "CLE",
         ifelse(Team == "Detroit", "DET",
         ifelse(Team == "Houston", "HOU",
         ifelse(Team == "Kansas City", "KCR",
         ifelse(Team == "Los Angeles" & Level == "Maj-AL", "LAA",
         ifelse(Team == "Minnesota", "MIN",
         ifelse(Team == "New York" & Level == "Maj-AL", "NYY",
         ifelse(Team == "Oakland", "OAK",
         ifelse(Team == "Seattle", "SEA",
         ifelse(Team == "Tampa Bay", "TBR",
         ifelse(Team == "Texas", "TEX",
         ifelse(Team == "Toronto", "TOR",
         ifelse(Team == "Arizona", "ARI",
         ifelse(Team == "Atlanta", "ATL",
         ifelse(Team == "Chicago" & Level == "Maj-NL", "CHC",
         ifelse(Team == "Cincinnati", "CIN",
         ifelse(Team == "Colorado", "COL",
         ifelse(Team == "Los Angeles" & Level == "Maj-NL", "LAD",
         ifelse(Team == "Miami", "MIA",
         ifelse(Team == "Milwaukee", "MIL",
         ifelse(Team == "New York" & Level == "Maj-NL", "NYM",
         ifelse(Team == "Philadelphia", "PHI",
         ifelse(Team == "Pittsburgh", "PIT",
         ifelse(Team == "San Diego", "SDP",
         ifelse(Team == "San Francisco", "SFG",
         ifelse(Team == "St. Louis", "STL",
         ifelse(Team == "Washington", "WSN", "no"))))))))))))))))))))))))))))))) %>%
  na_replace(fill = 0) %>%
  filter(Tm %in% Teams)

df_powerrankings <- df_powerrankings %>%
  mutate(
    run_opportunity = (H + BB + HBP) / R,
    run_opportunity_all = (H_all + BB_all + HBP_all) / R_all,
    extra_bases_rate = (X2B + X3B + HR) / AB,
    extra_bases_rate_all = (X2B_all + X3B_all + HR_all) / AB_all,
    power_ranking = (((ER.allowed * -1) + (H * 1.5) - (ER.allowed.bullpen * 0.5) - run_opportunity + Wins * (0.5)) +
      (((-ER.allowed_all * 1.5) + (H_all * 1.25) - (ER.allowed.bullpen_all * 0.5) - run_opportunity_all + Wins_all)))
  )

df_powerrankings

}

powerrankings_table() %>%
  select(Tm,power_ranking) %>%
  View()

glm(Wins ~ ER.allowed + H + ER.allowed.bullpen + run_opportunity, data = df_powerrankings) %>%
  summary()

win_percent <- function(Tm, DateFull) {
  df <- rbind(mlb_standings(season = 2022, date = DateFull, league_id = 104),mlb_standings(season = 2022, date = DateFull, league_id = 103))
  
  df$team_records_team_name[df$team_records_team_name == "Baltimore Orioles"] <- "BAL"
  df$team_records_team_name[df$team_records_team_name == "Boston Red Sox"] <- "BOS"
  df$team_records_team_name[df$team_records_team_name == "Chicago White Sox"] <- "CHW"
  df$team_records_team_name[df$team_records_team_name == "Cleveland Guardians"] <- "CLE"
  df$team_records_team_name[df$team_records_team_name == "Detroit Tigers"] <- "DET"
  df$team_records_team_name[df$team_records_team_name == "Houston Astros"] <- "HOU"
  df$team_records_team_name[df$team_records_team_name == "Kansas City Royals"] <- "KCR"
  df$team_records_team_name[df$team_records_team_name == "Los Angeles Angels"] <- "LAA"
  df$team_records_team_name[df$team_records_team_name == "Minnesota Twins"] <- "MIN"
  df$team_records_team_name[df$team_records_team_name == "New York Yankees"] <- "NYY"
  df$team_records_team_name[df$team_records_team_name == "Oakland Athletics"] <- "OAK"
  df$team_records_team_name[df$team_records_team_name == "Seattle Mariners"] <- "SEA"
  df$team_records_team_name[df$team_records_team_name == "Tampa Bay Rays"] <- "TBR"
  df$team_records_team_name[df$team_records_team_name == "Texas Rangers"] <- "TEX"
  df$team_records_team_name[df$team_records_team_name == "Toronto Blue Jays"] <- "TOR"
  df$team_records_team_name[df$team_records_team_name == "Arizona Diamondbacks"] <- "ARI"
  df$team_records_team_name[df$team_records_team_name == "Atlanta Braves"] <- "ATL"
  df$team_records_team_name[df$team_records_team_name == "Chicago Cubs"] <- "CHC"
  df$team_records_team_name[df$team_records_team_name == "Cincinnati Reds"] <- "CIN"
  df$team_records_team_name[df$team_records_team_name == "Colorado Rockies"] <- "COL"
  df$team_records_team_name[df$team_records_team_name == "Los Angeles Dodgers"] <- "LAD"
  df$team_records_team_name[df$team_records_team_name == "Miami Marlins"] <- "MIA"
  df$team_records_team_name[df$team_records_team_name == "Milwaukee Brewers"] <- "MIL"
  df$team_records_team_name[df$team_records_team_name == "New York Mets"] <- "NYM"
  df$team_records_team_name[df$team_records_team_name == "Philadelphia Phillies"] <- "PHI"
  df$team_records_team_name[df$team_records_team_name == "Pittsburgh Pirates"] <- "PIT"
  df$team_records_team_name[df$team_records_team_name == "San Diego Padres"] <- "SDP"
  df$team_records_team_name[df$team_records_team_name == "San Francisco Giants"] <- "SFG"
  df$team_records_team_name[df$team_records_team_name == "St. Louis Cardinals"] <- "STL"
  df$team_records_team_name[df$team_records_team_name == "Washington Nationals"] <- "WSN"
  
  df <- df %>%
    filter(team_records_team_name == Tm) %>%
    select(team_records_winning_percentage)
  value <- as.numeric(df$team_records_winning_percentage)
  
  print(value)
}

win_percent("OAK","2022-04-08")

df2022$DateFull <- as.Date(df2022$DateFull)
  
df2022 %>%
  mutate(
    v = win_percent(Tm, DateFull)
  )

df_test <- df2022 %>%
  as.data.frame() %>%
  select(Tm, DateFull)

df_test$Tm <- as.character(df_test$Tm)

df_test %>%
  mutate(
    win_perc = win_percent(Tm, DateFull)
  )


test %>%
  mutate(
    win_perc = win_percent(Tm, DateFull)
  )
