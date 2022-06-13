library(tidyverse)
library(baseballr)
library(ggthemes)
library(mgsub)
library(gt)

simulate_game <- function(Team, Opponent) {
  
  sim <- function(Team, Opponent) {
    
  df_2022 <- filter(df2022, Year == "2022")
  
  poisson_df_2022 <- rbind(
    data.frame(runs = df_2022$R,
               team = df_2022$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_last_10 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-9):(max(Gm))))
  
  poisson_df_last_10 <- rbind(
    data.frame(runs = df_last_10$R,
               team = df_last_10$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_last_11_20 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-19):(max(Gm)-10)))
  
  poisson_df_last_11_20 <- rbind(
    data.frame(runs = df_last_11_20$R,
               team = df_last_11_20$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_opp_pitching_last_10 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-9):(max(Gm))))
  
  poisson_df_opp_pitching_last_10 <- rbind(
    data.frame(runs = df_opp_pitching_last_10$RA,
               team = df_opp_pitching_last_10$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_opp_pitching_last_11_20 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-19):(max(Gm)-10)))
  
  poisson_df_opp_pitching_last_11_20 <- rbind(
    data.frame(runs = df_opp_pitching_last_11_20$RA,
               team = df_opp_pitching_last_11_20$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  ### Prediction for team
  
  runs_2022_team <- predict(poisson_df_2022, data.frame(team = Team), type = "response")
  
  runs_last_10_team <- predict(poisson_df_last_10, data.frame(team = Team), type = "response")
  
  runs_opp_pitching_last_10_team <- predict(poisson_df_opp_pitching_last_10, data.frame(team = Opponent), type = "response")
  
  runs_last_11_20_team <- predict(poisson_df_last_11_20, data.frame(team = Team), type = "response")
  
  runS_opp_pitching_last_11_20_team <- predict(poisson_df_opp_pitching_last_11_20, data.frame(team = Opponent), type = "response")
  
  ### Prediction for opponent
  
  runs_2022_opponent <- predict(poisson_df_2022, data.frame(team = Opponent), type = "response")
  
  runs_last_10_opponent <- predict(poisson_df_last_10, data.frame(team = Opponent), type = "response")
  
  runs_opp_pitching_last_10_opponent <- predict(poisson_df_opp_pitching_last_10, data.frame(team = Team), type = "response")
  
  runs_last_11_20_opponent <- predict(poisson_df_last_11_20, data.frame(team = Opponent), type = "response")
  
  runS_opp_pitching_last_11_20_opponent <- predict(poisson_df_opp_pitching_last_11_20, data.frame(team = Team), type = "response")
  
  ### Bringing it all together
  
  team_overall <- (runs_2022_team + ((runs_last_10_team * 0.7) + (runs_last_11_20_team * 0.3)) + ((runs_opp_pitching_last_10_team * 0.7) + (runS_opp_pitching_last_11_20_team * 0.3))) / 3
  
  opponent_overall <- (runs_2022_opponent + ((runs_last_10_opponent * 0.7) + (runs_last_11_20_opponent * 0.3)) + ((runs_opp_pitching_last_10_opponent * 0.7) + (runS_opp_pitching_last_11_20_opponent * 0.3))) / 3
  
  dpois(0:20, team_overall) %o% dpois(0:20, opponent_overall)
  
  }
  
  game_predict <- function(Team, Opponent) {
    game <- sim(Team, Opponent)
    game_frame <- data.frame(sum(game[lower.tri(game)]), sum(game[upper.tri(game)]), sum(diag(game)))
    
    if (Team == "PHI") {
      team_full <- c("Phillies")
    } else if (Team == "ARI") {
      team_full <- c("Diamondbacks")
    } else if (Team == "ATL") {
      team_full <- c("Braves")
    } else if (Team == "BAL") {
      team_full <- c("Orioles")
    } else if (Team == "BOS") {
      team_full <- c("Red Sox")
    } else if (Team == "CHC") {
      team_full <- c("Cubs")
    } else if (Team == "CHW") {
      team_full <- c("White Sox")
    } else if (Team == "CIN") {
      team_full <- c("Reds")
    } else if (Team == "CLE") {
      team_full <- c("Guardians")
    } else if (Team == "COL") {
      team_full <- c("Rockies")
    } else if (Team == "DET") {
      team_full <- c("Tigers")
    } else if (Team == "HOU") {
      team_full <- c("Astros")
    } else if (Team == "KCR") {
      team_full <- c("Royals")
    } else if (Team == "LAA") {
      team_full <- c("Angels")
    } else if (Team == "LAD") {
      team_full <- c("Dodgers")
    } else if (Team == "MIA") {
      team_full <- c("Marlins")
    } else if (Team == "MIL") {
      team_full <- c("Brewers")
    } else if (Team == "MIN") {
      team_full <- c("Twins")
    } else if (Team == "NYM") {
      team_full <- c("Mets")
    } else if (Team == "NYY") {
      team_full <- c("Yankees")
    } else if (Team == "OAK") {
      team_full <- c("Athletics")
    } else if (Team == "PIT") {
      team_full <- c("Pirates")
    } else if (Team == "SDP") {
      team_full <- c("Padres")
    } else if (Team == "SFG") {
      team_full <- c("Giants")
    } else if (Team == "SEA") {
      team_full <- c("Mariners")
    } else if (Team == "STL") {
      team_full <- c("Cardinals")
    } else if (Team == "TBR") {
      team_full <- c("Rays")
    } else if (Team == "TEX") {
      team_full <- c("Texans")
    } else if (Team == "TOR") {
      team_full <- c("Blue Jays")
    } else if (Team == "WSN") {
      team_full <- c("Nationals")
    } else {
      team_full <- c("NULL")
    }
    
    if (Opponent == "PHI") {
      opponent_full <- c("Phillies")
    } else if (Opponent == "ARI") {
      opponent_full <- c("Diamondbacks")
    } else if (Opponent == "ATL") {
      opponent_full <- c("Braves")
    } else if (Opponent == "BAL") {
      opponent_full <- c("Orioles")
    } else if (Opponent == "BOS") {
      opponent_full <- c("Red Sox")
    } else if (Opponent == "CHC") {
      opponent_full <- c("Cubs")
    } else if (Opponent == "CHW") {
      opponent_full <- c("White Sox")
    } else if (Opponent == "CIN") {
      opponent_full <- c("Reds")
    } else if (Opponent == "CLE") {
      opponent_full <- c("Guardians")
    } else if (Opponent == "COL") {
      opponent_full <- c("Rockies")
    } else if (Opponent == "DET") {
      opponent_full <- c("Tigers")
    } else if (Opponent == "HOU") {
      opponent_full <- c("Astros")
    } else if (Opponent == "KCR") {
      opponent_full <- c("Royals")
    } else if (Opponent == "LAA") {
      opponent_full <- c("Angels")
    } else if (Opponent == "LAD") {
      opponent_full <- c("Dodgers")
    } else if (Opponent == "MIA") {
      opponent_full <- c("Marlins")
    } else if (Opponent == "MIL") {
      opponent_full <- c("Brewers")
    } else if (Opponent == "MIN") {
      opponent_full <- c("Twins")
    } else if (Opponent == "NYM") {
      opponent_full <- c("Mets")
    } else if (Opponent == "NYY") {
      opponent_full <- c("Yankees")
    } else if (Opponent == "OAK") {
      opponent_full <- c("Athletics")
    } else if (Opponent == "PIT") {
      opponent_full <- c("Pirates")
    } else if (Opponent == "SDP") {
      opponent_full <- c("Padres")
    } else if (Opponent == "SFG") {
      opponent_full <- c("Giants")
    } else if (Opponent == "SEA") {
      opponent_full <- c("Mariners")
    } else if (Opponent == "STL") {
      opponent_full <- c("Cardinals")
    } else if (Opponent == "TBR") {
      opponent_full <- c("Rays")
    } else if (Opponent == "TEX") {
      opponent_full <- c("Texans")
    } else if (Opponent == "TOR") {
      opponent_full <- c("Blue Jays")
    } else if (Opponent == "WSN") {
      opponent_full <- c("Nationals")
    } else {
      opponent_full <- c("NULL")
    }
    
    colnames(game_frame) <- c(team_full, opponent_full, "Extra Innings")
    game_frame
    
  }
  
  game_predict(Team, Opponent)

}
simulate_game("PHI","MIA")

run_dist_graph <- function(Team, Opponent) {
  
  df_2022 <- filter(df2022, Year == "2022")
  
  poisson_df_2022 <- rbind(
    data.frame(runs = df_2022$R,
               team = df_2022$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_last_10 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-9):(max(Gm))))
  
  poisson_df_last_10 <- rbind(
    data.frame(runs = df_last_10$R,
               team = df_last_10$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_last_11_20 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-19):(max(Gm)-10)))
  
  poisson_df_last_11_20 <- rbind(
    data.frame(runs = df_last_11_20$R,
               team = df_last_11_20$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_opp_pitching_last_10 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-9):(max(Gm))))
  
  poisson_df_opp_pitching_last_10 <- rbind(
    data.frame(runs = df_opp_pitching_last_10$RA,
               team = df_opp_pitching_last_10$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  df_opp_pitching_last_11_20 <- df2022 %>%
    filter(Year == 2022) %>%
    group_by(Tm) %>%
    filter(Gm %in% ((max(Gm)-19):(max(Gm)-10)))
  
  poisson_df_opp_pitching_last_11_20 <- rbind(
    data.frame(runs = df_opp_pitching_last_11_20$RA,
               team = df_opp_pitching_last_11_20$Tm)) %>%
    glm(runs ~ team, family = poisson(link = "log"), data = .)
  
  ### Prediction for team
  
  runs_2022_team <- predict(poisson_df_2022, data.frame(team = Team), type = "response")
  
  runs_last_10_team <- predict(poisson_df_last_10, data.frame(team = Team), type = "response")
  
  runs_opp_pitching_last_10_team <- predict(poisson_df_opp_pitching_last_10, data.frame(team = Opponent), type = "response")
  
  runs_last_11_20_team <- predict(poisson_df_last_11_20, data.frame(team = Team), type = "response")
  
  runS_opp_pitching_last_11_20_team <- predict(poisson_df_opp_pitching_last_11_20, data.frame(team = Opponent), type = "response")
  
  ### Prediction for opponent
  
  runs_2022_opponent <- predict(poisson_df_2022, data.frame(team = Opponent), type = "response")
  
  runs_last_10_opponent <- predict(poisson_df_last_10, data.frame(team = Opponent), type = "response")
  
  runs_opp_pitching_last_10_opponent <- predict(poisson_df_opp_pitching_last_10, data.frame(team = Team), type = "response")
  
  runs_last_11_20_opponent <- predict(poisson_df_last_11_20, data.frame(team = Opponent), type = "response")
  
  runS_opp_pitching_last_11_20_opponent <- predict(poisson_df_opp_pitching_last_11_20, data.frame(team = Team), type = "response")
  
  ### Bringing it all together
  
  team_overall <- (runs_2022_team + ((runs_last_10_team * 0.7) + (runs_last_11_20_team * 0.3)) + ((runs_opp_pitching_last_10_team * 0.7) + (runS_opp_pitching_last_11_20_team * 0.3))) / 3
  
  opponent_overall <- (runs_2022_opponent + ((runs_last_10_opponent * 0.7) + (runs_last_11_20_opponent * 0.3)) + ((runs_opp_pitching_last_10_opponent * 0.7) + (runS_opp_pitching_last_11_20_opponent * 0.3))) / 3
  
  df <- as.data.frame(dpois(0:20, team_overall) %o% dpois(0:20, opponent_overall))
  
  df_2 <- data.frame(sum(df[(col(df)-20) == row(df)]), 
                     sum(df[(col(df)-19) == row(df)]),
                     sum(df[(col(df)-18) == row(df)]),
                     sum(df[(col(df)-17) == row(df)]),
                     sum(df[(col(df)-16) == row(df)]),
                     sum(df[(col(df)-15) == row(df)]),
                     sum(df[(col(df)-14) == row(df)]),
                     sum(df[(col(df)-13) == row(df)]),
                     sum(df[(col(df)-12) == row(df)]),
                     sum(df[(col(df)-11) == row(df)]),
                     sum(df[(col(df)-10) == row(df)]),
                     sum(df[(col(df)-9) == row(df)]),
                     sum(df[(col(df)-8) == row(df)]),
                     sum(df[(col(df)-7) == row(df)]),
                     sum(df[(col(df)-6) == row(df)]),
                     sum(df[(col(df)-5) == row(df)]),
                     sum(df[(col(df)-4) == row(df)]),
                     sum(df[(col(df)-3) == row(df)]),
                     sum(df[(col(df)-2) == row(df)]),
                     sum(df[(col(df)-1) == row(df)]),
                     sum(df[(col(df)-0) == row(df)]),
                     sum(df[(col(df)+1) == row(df)]),
                     sum(df[(col(df)+2) == row(df)]),
                     sum(df[(col(df)+3) == row(df)]),
                     sum(df[(col(df)+4) == row(df)]),
                     sum(df[(col(df)+5) == row(df)]),
                     sum(df[(col(df)+6) == row(df)]),
                     sum(df[(col(df)+7) == row(df)]),
                     sum(df[(col(df)+8) == row(df)]),
                     sum(df[(col(df)+9) == row(df)]),
                     sum(df[(col(df)+10) == row(df)]),
                     sum(df[(col(df)+11) == row(df)]),
                     sum(df[(col(df)+12) == row(df)]),
                     sum(df[(col(df)+13) == row(df)]),
                     sum(df[(col(df)+14) == row(df)]),
                     sum(df[(col(df)+15) == row(df)]),
                     sum(df[(col(df)+16) == row(df)]),
                     sum(df[(col(df)+17) == row(df)]),
                     sum(df[(col(df)+18) == row(df)]),
                     sum(df[(col(df)+19) == row(df)]),
                     sum(df[(col(df)+20) == row(df)]))
  colnames(df_2) <- c(-20:20)
  
  df_2 <- t(df_2)
  
  colnames(df_2) <- c("RunProbability")
  
  df_2 <- data.frame(df_2)
  
  if (Team == "PHI") {
    team_full <- c("Phillies")
  } else if (Team == "ARI") {
    team_full <- c("Diamondbacks")
  } else if (Team == "ATL") {
    team_full <- c("Braves")
  } else if (Team == "BAL") {
    team_full <- c("Orioles")
  } else if (Team == "BOS") {
    team_full <- c("Red Sox")
  } else if (Team == "CHC") {
    team_full <- c("Cubs")
  } else if (Team == "CHW") {
    team_full <- c("White Sox")
  } else if (Team == "CIN") {
    team_full <- c("Reds")
  } else if (Team == "CLE") {
    team_full <- c("Guardians")
  } else if (Team == "COL") {
    team_full <- c("Rockies")
  } else if (Team == "DET") {
    team_full <- c("Tigers")
  } else if (Team == "HOU") {
    team_full <- c("Astros")
  } else if (Team == "KCR") {
    team_full <- c("Royals")
  } else if (Team == "LAA") {
    team_full <- c("Angels")
  } else if (Team == "LAD") {
    team_full <- c("Dodgers")
  } else if (Team == "MIA") {
    team_full <- c("Marlins")
  } else if (Team == "MIL") {
    team_full <- c("Brewers")
  } else if (Team == "MIN") {
    team_full <- c("Twins")
  } else if (Team == "NYM") {
    team_full <- c("Mets")
  } else if (Team == "NYY") {
    team_full <- c("Yankees")
  } else if (Team == "OAK") {
    team_full <- c("Athletics")
  } else if (Team == "PIT") {
    team_full <- c("Pirates")
  } else if (Team == "SDP") {
    team_full <- c("Padres")
  } else if (Team == "SFG") {
    team_full <- c("Giants")
  } else if (Team == "SEA") {
    team_full <- c("Mariners")
  } else if (Team == "STL") {
    team_full <- c("Cardinals")
  } else if (Team == "TBR") {
    team_full <- c("Rays")
  } else if (Team == "TEX") {
    team_full <- c("Texans")
  } else if (Team == "TOR") {
    team_full <- c("Blue Jays")
  } else if (Team == "WSN") {
    team_full <- c("Nationals")
  } else {
    team_full <- c("NULL")
  }
  
  if (Opponent == "PHI") {
    opponent_full <- c("Phillies")
  } else if (Opponent == "ARI") {
    opponent_full <- c("Diamondbacks")
  } else if (Opponent == "ATL") {
    opponent_full <- c("Braves")
  } else if (Opponent == "BAL") {
    opponent_full <- c("Orioles")
  } else if (Opponent == "BOS") {
    opponent_full <- c("Red Sox")
  } else if (Opponent == "CHC") {
    opponent_full <- c("Cubs")
  } else if (Opponent == "CHW") {
    opponent_full <- c("White Sox")
  } else if (Opponent == "CIN") {
    opponent_full <- c("Reds")
  } else if (Opponent == "CLE") {
    opponent_full <- c("Guardians")
  } else if (Opponent == "COL") {
    opponent_full <- c("Rockies")
  } else if (Opponent == "DET") {
    opponent_full <- c("Tigers")
  } else if (Opponent == "HOU") {
    opponent_full <- c("Astros")
  } else if (Opponent == "KCR") {
    opponent_full <- c("Royals")
  } else if (Opponent == "LAA") {
    opponent_full <- c("Angels")
  } else if (Opponent == "LAD") {
    opponent_full <- c("Dodgers")
  } else if (Opponent == "MIA") {
    opponent_full <- c("Marlins")
  } else if (Opponent == "MIL") {
    opponent_full <- c("Brewers")
  } else if (Opponent == "MIN") {
    opponent_full <- c("Twins")
  } else if (Opponent == "NYM") {
    opponent_full <- c("Mets")
  } else if (Opponent == "NYY") {
    opponent_full <- c("Yankees")
  } else if (Opponent == "OAK") {
    opponent_full <- c("Athletics")
  } else if (Opponent == "PIT") {
    opponent_full <- c("Pirates")
  } else if (Opponent == "SDP") {
    opponent_full <- c("Padres")
  } else if (Opponent == "SFG") {
    opponent_full <- c("Giants")
  } else if (Opponent == "SEA") {
    opponent_full <- c("Mariners")
  } else if (Opponent == "STL") {
    opponent_full <- c("Cardinals")
  } else if (Opponent == "TBR") {
    opponent_full <- c("Rays")
  } else if (Opponent == "TEX") {
    opponent_full <- c("Texans")
  } else if (Opponent == "TOR") {
    opponent_full <- c("Blue Jays")
  } else if (Opponent == "WSN") {
    opponent_full <- c("Nationals")
  } else {
    opponent_full <- c("NULL")
  }
  
  ggplot(df_2, aes(x = c(-20:20))) +
    geom_col(aes(y = RunProbability), fill = "#6d3c3c") +
    scale_x_continuous(breaks = c(-5,0,5), limits = c(-5.5,5.5)) +
    ylim(0,0.2) +
    labs(x = paste("Runs", team_full, "Wins/Loses By"),
         y = "Probability",
         caption = "Data compiled from the baseballr package.") +
    ggtitle(paste(team_full, "vs.", opponent_full)) +
    theme_fivethirtyeight() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey", size = 0.5),
      plot.title = element_text(size = 20),
      axis.ticks = element_line(color = "grey"),
      axis.title = element_text()
    )
}
run_dist_graph("PHI","MIA")

