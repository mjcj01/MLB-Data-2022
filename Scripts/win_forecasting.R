library(tidyverse)
library(baseballr)
library(ggthemes)
library(forecast)
library(showtext)

font_add_google("Roboto Condensed")
showtext_auto()

### For purposes of predicting game wins needed for the 2022 playoffs, I 
### retroactively figured out which teams would make the playoffs in the past
### using the 2022 playoff rules (i.e. 3 Wildcard spots)

projected_playoff_teams_2022 <- c("MIN","NYY","HOU","TOR","TBR","BOS","NYM","MIL","LAD","STL","SDP","SFG")
playoff_teams_2021 <- c("TBR","BOS","NYY","CHW","HOU","TOR","ATL","MIL","STL","SFG","LAD","CIN")
playoff_teams_2020 <- c("MIA","TOR","NYY","CHC","CHW","CLE","ATL","MIN","OAK","SDP","TBR","LAD")
playoff_teams_2019 <- c("NYY","TBR","MIN","HOU","OAK","CLE","ATL","WSN","STL","MIL","LAD","NYM")

wins_plot <- function(Team) {
  
  previous_seasons <- rbind(filter(df1, Year == 2017 & Tm == Team),filter(df1, Year == 2018 & Tm == Team),
                            filter(df1, Year == 2019 & Tm == Team),filter(df1, Year == 2020 & Tm == Team),
                            filter(df1, Year == 2021 & Tm == Team))
  
  playoff_team_wins_2022 <- filter(df1, Tm %in% projected_playoff_teams_2022 & Year == 2022)
  
  team_wins <- filter(df1, Tm == Team & Year == 2022)
  
  most_recent_game_number <- max(team_wins$Gm)
  
  ggplot(data = team_wins, aes(x = Gm)) +
    geom_line(aes(y = wins_so_far, color = "Team's Current Wins"), size = 2) +
    xlim(0,most_recent_game_number) +
    geom_smooth(data = previous_seasons, method = loess, aes(x = Gm, y = wins_so_far, color = "Team's Past Seasons")) +
    geom_smooth(data = playoff_team_wins_2022, method = loess, aes(x = Gm, y = wins_so_far, color = "Current Playoff Averages")) +
    ggtitle(paste(
Team, "Wins Relative to Past Seasons 
& Current Playoff Contenders")) +
    labs(caption = "Data compiled from the baseballr package.",
         colour = "") +
    theme_fivethirtyeight() +
    scale_color_solarized() +
    theme(
      legend.position = "bottom",
    )
}

wins_plot("PHI")

filter(df1, Tm %in% projected_playoff_teams_2022 & Year == 2022) %>%
  ggplot(aes(x = Gm, y = wins_so_far, color = Tm)) +
  geom_line() +
  geom_smooth(aes(x = Gm, y = wins_so_far))
