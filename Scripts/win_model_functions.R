library(tidyverse)
library(baseballr)
library(ggthemes)

simulate_game_both_pitchers <- function(team, opponent, team_pitcher, opp_pitcher) {
  sim <- function(team, opponent, team_pitcher, opp_pitcher) {
    
    df_2022 <- filter(df1, Year == 2022)
    
    poisson_2022_team <- rbind(
      data.frame(runs = df_2022$R,
                 team = df_2022$Tm,
                 opponent = df_2022$Opp)) %>%
      glm(runs ~ team + opponent, family = poisson(link = "log"), data =.)
    
    poisson_2022_opp <- rbind(
      data.frame(runs = df_2022$RA,
                 team = df_2022$Tm,
                 opponent = df_2022$Opp)) %>%
      glm(runs ~ team + opponent, family = poisson(link = "log"), data =.)
    
    poisson_2022_team_pitcher <- rbind(
      data.frame(runs = starting_pitcher_game_log$R,
                 team_pitcher = starting_pitcher_game_log$PlayerName)) %>%
      glm(runs ~ team_pitcher, family = poisson(link = "log"), data =.)
    
    poisson_2022_opp_pitcher <- rbind(
      data.frame(runs = starting_pitcher_game_log$R,
                 opp_pitcher = starting_pitcher_game_log$PlayerName)) %>%
      glm(runs ~ opp_pitcher, family = poisson(link = "log"), data =.)
    
    poisson_2022_team_runs <- predict(poisson_2022_team,
                                      data.frame(team = team, opponent = opponent), type = "response")
    
    poisson_2022_opp_runs <- predict(poisson_2022_opp,
                                     data.frame(team = team, opponent = opponent), type = "response")
    
    poisson_2022_team_pitcher_runs <- predict(poisson_2022_team_pitcher,
                                              data.frame(team_pitcher = team_pitcher), type = "response")
    
    poisson_2022_opp_pitcher_runs <- predict(poisson_2022_opp_pitcher,
                                             data.frame(opp_pitcher = opp_pitcher), type = "response")
    
    team_overall <- (poisson_2022_opp_pitcher_runs * 0.5) + (poisson_2022_team_runs * 0.5)
    
    opp_overall <- (poisson_2022_team_pitcher_runs * 0.5) + (poisson_2022_opp_runs * 0.5)
    
    dpois(0:20, team_overall) %o% dpois(0:20, opp_overall)
    
  }
  
  game_predict <- function(team, opponent, team_pitcher, opp_pitcher) {
    game <- sim(team, opponent, team_pitcher, opp_pitcher)
    game_frame <- data.frame(sum(game[lower.tri(game)]), sum(game[upper.tri(game)]), sum(diag(game)))
    colnames(game_frame) <- c(team, opponent, "Extra Innings")
    game_frame
  }
  
  game_predict(team, opponent, team_pitcher, opp_pitcher)
}

simulate_game_both_pitchers("PHI","ARI","Kyle Gibson","Zac Gallen")

run_distribution_graph <- function(team, opponent, team_pitcher, opp_pitcher) {
  
  df_2022 <- filter(df1, Year == 2022)
  
  poisson_2022_team <- rbind(
    data.frame(runs = df_2022$R,
               team = df_2022$Tm,
               opponent = df_2022$Opp)) %>%
    glm(runs ~ team + opponent, family = poisson(link = "log"), data =.)
  
  poisson_2022_opp <- rbind(
    data.frame(runs = df_2022$RA,
               team = df_2022$Tm,
               opponent = df_2022$Opp)) %>%
    glm(runs ~ team + opponent, family = poisson(link = "log"), data =.)
  
  poisson_2022_team_pitcher <- rbind(
    data.frame(runs = starting_pitcher_game_log$R,
               team_pitcher = starting_pitcher_game_log$PlayerName)) %>%
    glm(runs ~ team_pitcher, family = poisson(link = "log"), data =.)
  
  poisson_2022_opp_pitcher <- rbind(
    data.frame(runs = starting_pitcher_game_log$R,
               opp_pitcher = starting_pitcher_game_log$PlayerName)) %>%
    glm(runs ~ opp_pitcher, family = poisson(link = "log"), data =.)
  
  poisson_2022_team_runs <- predict(poisson_2022_team,
                                    data.frame(team = team, opponent = opponent), type = "response")
  
  poisson_2022_opp_runs <- predict(poisson_2022_opp,
                                   data.frame(team = team, opponent = opponent), type = "response")
  
  poisson_2022_team_pitcher_runs <- predict(poisson_2022_team_pitcher,
                                            data.frame(team_pitcher = team_pitcher), type = "response")
  
  poisson_2022_opp_pitcher_runs <- predict(poisson_2022_opp_pitcher,
                                           data.frame(opp_pitcher = opp_pitcher), type = "response")
  
  team_overall <- (poisson_2022_opp_pitcher_runs * 0.5) + (poisson_2022_team_runs * 0.5)
  
  opp_overall <- (poisson_2022_team_pitcher_runs * 0.5) + (poisson_2022_opp_runs * 0.5)
  
  df <- as.data.frame(dpois(0:20, team_overall) %o% dpois(0:20, opp_overall))
  
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
  
  df_3 <- simulate_game_both_pitchers(team, opponent, team_pitcher, opp_pitcher)
  
  ggplot(df_2, aes(x = c(-20:20))) +
    geom_col(aes(y = RunProbability), fill = "#6d3c3c") +
    scale_x_continuous(breaks = c(-5,0,5), limits = c(-5.5,5.5)) +
    ylim(0,0.2) +
    labs(x = paste("Runs", team, "Wins/Loses By"),
         y = "Probability",
         caption = "Data compiled from the baseballr package.") +
    ggtitle(paste(team, "vs.", opponent)) +
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

run_distribution_graph("PHI","MIL","Zach Eflin","Corbin Burnes")
