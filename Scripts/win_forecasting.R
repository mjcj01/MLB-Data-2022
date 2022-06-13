library(tidyverse)
library(baseballr)
library(ggthemes)
library(forecast)
library(showtext)
library(mgsub)

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
  
  previous_seasons <- rbind(filter(df2017_2022, Year == 2017 & Tm == Team),filter(df2017_2022, Year == 2018 & Tm == Team),
                            filter(df2017_2022, Year == 2019 & Tm == Team),filter(df2017_2022, Year == 2020 & Tm == Team),
                            filter(df2017_2022, Year == 2021 & Tm == Team))
  
  playoff_team_wins_2022 <- filter(df2017_2022, Tm %in% projected_playoff_teams_2022 & Year == 2022)
  
  team_wins <- filter(df2017_2022, Tm == Team & Year == 2022)
  
  if (Team == "PHI") {
    color <- c("#E81828","#002d72","#FFFFFF")
  } else if (Team == "ARI") {
    color <- c("#A71930","#E3D4AD","#000000")
  } else if (Team == "ATL") {
    color <- c("#CE1141","#13274F","#EAAA00")
  } else if (Team == "BAL") {
    color <- c("#DF4601","#000000","#FFFFFF")
  } else if (Team == "BOS") {
    color <- c("#BD3039","#0C2340","#FFFFFF")
  } else if (Team == "CHC") {
    color <- c("#0E3386","#CC3433","#FFFFFF")
  } else if (Team == "CHW") {
    color <- c("#27251F","#C4CED4","#FFFFFF")
  } else if (Team == "CIN") {
    color <- c("#C6011F","#000000","#FFFFFF")
  } else if (Team == "CLE") {
    color <- c("#00385D","#E50022","#FFFFFF")
  } else if (Team == "COL") {
    color <- c("#333366","#C4CED4","#131413")
  } else if (Team == "DET") {
    color <- c("#0C2340","#FA4616","#FFFFFF")
  } else if (Team == "HOU") {
    color <- c("#002D62","#EB6E1F","#F4911E")
  } else if (Team == "KCR") {
    color <- c("#004687","#BD9B60","#FFFFFF")
  } else if (Team == "LAA") {
    color <- c("#003263","#BA0021","#862633")
  } else if (Team == "LAD") {
    color <- c("#005A9C","#EF3E42","#FFFFFF")
  } else if (Team == "MIA") {
    color <- c("#00A3E0","#FF6600","#FFD100")
  } else if (Team == "MIL") {
    color <- c("#ffc52f","#12284b","#FFFFFF")
  } else if (Team == "MIN") {
    color <- c("#002B5C","#D31145","#B9975B")
  } else if (Team == "NYM") {
    color <- c("#002D72","#FF5910","#FFFFFF")
  } else if (Team == "NYY") {
    color <- c("#0C2340","#E4002C","#FFFFFF")
  } else if (Team == "OAK") {
    color <- c("#003831","#EFB21E","#FFFFFF")
  } else if (Team == "PIT") {
    color <- c("#27251F","#FFC72C","#E4002B")
  } else if (Team == "SDP") {
    color <- c("#2F241D","#FFC425","#002d62")
  } else if (Team == "SFG") {
    color <- c("#FD5A1E","#27251F","#EFD19F")
  } else if (Team == "SEA") {
    color <- c("#0C2C56","#005C5C","#D50032")
  } else if (Team == "STL") {
    color <- c("#C41E3A","#0C2340","#FEDB00")
  } else if (Team == "TBR") {
    color <- c("#092C5C","#8FBCE6","#F5D130")
  } else if (Team == "TEX") {
    color <- c("#003278","#C0111F","#FFFFFF")
  } else if (Team == "TOR") {
    color <- c("#134A8E","#1D2D5C","#E8291C")
  } else if (Team == "WSN") {
    color <- c("#AB0003","#14225A","#FFFFFF")
  } else {
    color <- c("000000","000000","000000")
  }
  
  most_recent_game_number <- max(team_wins$Gm)
  
  ggplot(data = team_wins, aes(x = Gm)) +
    geom_line(aes(y = wins_so_far, color = paste(team_full, "Current Wins")), size = 2) +
    xlim(0,most_recent_game_number) +
    geom_smooth(data = previous_seasons, method = loess, aes(x = Gm, y = wins_so_far, color = paste(team_full, "Past Seasons"))) +
    geom_smooth(data = playoff_team_wins_2022, method = loess, aes(x = Gm, y = wins_so_far, color = "Current Playoff Averages")) +
    ggtitle(paste(
team_full, "Wins Relative to Past Seasons 
& Current Playoff Contenders")) +
    labs(caption = "Data compiled from the baseballr package.",
         colour = "") +
    theme_fivethirtyeight() +
    scale_color_manual(values = color) +
    theme(
      legend.position = "bottom",
    )
}

wins_plot("PHI")

