library(shiny)
library(tidyverse)
library(baseballr)
library(rsconnect)
library(plotly)
library(mgsub)

ui <- fluidPage(
  navbarPage(
    title = "mjcMLB Data",
    tabPanel("About",
             HTML(r"(<h1>Welcome to my MLB data project</h1>
                  <h4>This page serves primarily as a function for me to learn R and how to build Shiny applications through the plentiful amount of baseball data that exists.</h4>
                  <h4>Please give the data some time to load when you go to the different pages on this site.</h4>
                  <h4>More features will gradually be added to this site.</h4>
                  <h3>Win Probability Methodology</h3>
                  <h4>In order to calculate the win probability of a team against another team, the model looks at how many runs a team has scored and runs allowed on the year and in the last 20 games using a Poisson distribution.</h4>
                  <h4>Predicted Runs = (Season Run Total + (Recent 10 Game Run Total * 0.7) + (Next 10 Most Recent Game Run Total * 0.3) + (Recent 10 Game Runs Allowed by Opponent * 0.7) + (Next 10 Most Recent Game Runs Allowed by Opponent * 0.3)) / 3</h4>
             )"),
            ),
    tabPanel("Win Probability at Game Start",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "team",
                              label = "Select a team",
                              choices = c(Tm),
                              selected = NULL),
                 selectInput(inputId = "opponent",
                             label = "Select a team",
                             choices = c(Tm),
                             selected = NULL)),
               mainPanel(plotlyOutput("run_dist"),
                         div(dataTableOutput("run_dist_table"), style = "font-size:100%")
                         )
               )),
    tabPanel("Team Wins So Far This Season",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "team2",
                             label = "Select a team",
                             choices = c(Tm),
                             selected = NULL)),
               mainPanel(plotlyOutput("wins", height = "100%"))
               )
             ))
  )


server <- function(input, output) {
  
  Team <- c("OAK", "PIT", "SD",  "SEA", "SF",  "STL", "TB",  "TEX", "TOR", "MIN",
            "PHI", "ATL", "CHW", "MIA", "NYY", "MIL", "LAA", "ARI", "BAL", "BOS",
            "CHC", "CIN", "CLE", "COL", "DET", "HOU", "KC",  "LAD", "WSN", "NYM")
  Tm <- as.factor(c("OAK", "PIT", "SDP",  "SEA", "SFG",  "STL", "TBR",  "TEX", "TOR", "MIN",
                    "PHI", "ATL", "CHW", "MIA", "NYY", "MIL", "LAA", "ARI", "BAL", "BOS",
                    "CHC", "CIN", "CLE", "COL", "DET", "HOU", "KCR",  "LAD", "WSN", "NYM"))
  
  bref_team_results_2022 <- function(Tm) {
    
    bref_team_results_2022 <- function(Tm) {
      bref_team_results(Tm = Tm, year = 2022)
    }
    
    df_master <- lapply(Tm, bref_team_results_2022) %>%
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
  
  refresh_df2022 <- function() {
    
    lapply(Team, bref_team_results_2022) %>%
      bind_rows()
  }
  
  df2022_1 <- refresh_df2022()
  
  run_dist_graph_shiny <- function(Team, Opponent) {
    
    if (Team == "PHI") {
      color <- c("#E81828")
    } else if (Team == "ARI") {
      color <- c("#A71930")
    } else if (Team == "ATL") {
      color <- c("#CE1141")
    } else if (Team == "BAL") {
      color <- c("#DF4601")
    } else if (Team == "BOS") {
      color <- c("#BD3039")
    } else if (Team == "CHC") {
      color <- c("#0E3386")
    } else if (Team == "CHW") {
      color <- c("#27251F")
    } else if (Team == "CIN") {
      color <- c("#C6011F")
    } else if (Team == "CLE") {
      color <- c("#00385D")
    } else if (Team == "COL") {
      color <- c("#333366")
    } else if (Team == "DET") {
      color <- c("#0C2340")
    } else if (Team == "HOU") {
      color <- c("#002D62")
    } else if (Team == "KCR") {
      color <- c("#004687")
    } else if (Team == "LAA") {
      color <- c("#003263")
    } else if (Team == "LAD") {
      color <- c("#005A9C")
    } else if (Team == "MIA") {
      color <- c("#00A3E0")
    } else if (Team == "MIL") {
      color <- c("#ffc52f")
    } else if (Team == "MIN") {
      color <- c("#002B5C")
    } else if (Team == "NYM") {
      color <- c("#002D72")
    } else if (Team == "NYY") {
      color <- c("#0C2340")
    } else if (Team == "OAK") {
      color <- c("#003831")
    } else if (Team == "PIT") {
      color <- c("#27251F")
    } else if (Team == "SDP") {
      color <- c("#2F241D")
    } else if (Team == "SFG") {
      color <- c("#FD5A1E")
    } else if (Team == "SEA") {
      color <- c("#0C2C56")
    } else if (Team == "STL") {
      color <- c("#C41E3A")
    } else if (Team == "TBR") {
      color <- c("#092C5C")
    } else if (Team == "TEX") {
      color <- c("#003278")
    } else if (Team == "TOR") {
      color <- c("#134A8E")
    } else if (Team == "WSN") {
      color <- c("#AB0003")
    } else {
      color <- c("000000")
    }
    
    df_2022 <- filter(df2022_1, Year == "2022")
    
    poisson_df_2022 <- rbind(
      data.frame(runs = df_2022$R,
                 team = df_2022$Tm)) %>%
      glm(runs ~ team, family = poisson(link = "log"), data = .)
    
    df_last_10 <- df2022_1 %>%
      filter(Year == 2022) %>%
      group_by(Tm) %>%
      filter(Gm %in% ((max(Gm)-9):(max(Gm))))
    
    poisson_df_last_10 <- rbind(
      data.frame(runs = df_last_10$R,
                 team = df_last_10$Tm)) %>%
      glm(runs ~ team, family = poisson(link = "log"), data = .)
    
    df_last_11_20 <- df2022_1 %>%
      filter(Year == 2022) %>%
      group_by(Tm) %>%
      filter(Gm %in% ((max(Gm)-19):(max(Gm)-10)))
    
    poisson_df_last_11_20 <- rbind(
      data.frame(runs = df_last_11_20$R,
                 team = df_last_11_20$Tm)) %>%
      glm(runs ~ team, family = poisson(link = "log"), data = .)
    
    df_opp_pitching_last_10 <- df2022_1 %>%
      filter(Year == 2022) %>%
      group_by(Tm) %>%
      filter(Gm %in% ((max(Gm)-9):(max(Gm))))
    
    poisson_df_opp_pitching_last_10 <- rbind(
      data.frame(runs = df_opp_pitching_last_10$RA,
                 team = df_opp_pitching_last_10$Tm)) %>%
      glm(runs ~ team, family = poisson(link = "log"), data = .)
    
    df_opp_pitching_last_11_20 <- df2022_1 %>%
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
      geom_col(aes(y = RunProbability), fill = color) +
      scale_x_continuous(breaks = c(-5,0,5), limits = c(-5.5,5.5)) +
      ylim(0,0.2) +
      labs(x = paste("Runs", team_full, "Wins/Loses By"),
           y = "Probability",
           caption = "Data compiled from the baseballr package.") +
      ggtitle(paste(team_full, "vs.", opponent_full))
  }
  
  simulate_game_shiny <- function(Team, Opponent) {
    
    sim <- function(Team, Opponent) {
      
      df_2022 <- filter(df2022_1, Year == "2022")
      
      poisson_df_2022 <- rbind(
        data.frame(runs = df_2022$R,
                   team = df_2022$Tm)) %>%
        glm(runs ~ team, family = poisson(link = "log"), data = .)
      
      df_last_10 <- df2022_1 %>%
        filter(Year == 2022) %>%
        group_by(Tm) %>%
        filter(Gm %in% ((max(Gm)-9):(max(Gm))))
      
      poisson_df_last_10 <- rbind(
        data.frame(runs = df_last_10$R,
                   team = df_last_10$Tm)) %>%
        glm(runs ~ team, family = poisson(link = "log"), data = .)
      
      df_last_11_20 <- df2022_1 %>%
        filter(Year == 2022) %>%
        group_by(Tm) %>%
        filter(Gm %in% ((max(Gm)-19):(max(Gm)-10)))
      
      poisson_df_last_11_20 <- rbind(
        data.frame(runs = df_last_11_20$R,
                   team = df_last_11_20$Tm)) %>%
        glm(runs ~ team, family = poisson(link = "log"), data = .)
      
      df_opp_pitching_last_10 <- df2022_1 %>%
        filter(Year == 2022) %>%
        group_by(Tm) %>%
        filter(Gm %in% ((max(Gm)-9):(max(Gm))))
      
      poisson_df_opp_pitching_last_10 <- rbind(
        data.frame(runs = df_opp_pitching_last_10$RA,
                   team = df_opp_pitching_last_10$Tm)) %>%
        glm(runs ~ team, family = poisson(link = "log"), data = .)
      
      df_opp_pitching_last_11_20 <- df2022_1 %>%
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
  
  output$run_dist <- renderPlotly({
    run_dist_graph_shiny(input$team, input$opponent)
  })
  
  output$run_dist_table <- renderDataTable(
    simulate_game_shiny(input$team, input$opponent)
  )
  
  wins_plot_shiny <- function(Team) {
    
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
    
    al_teams <- c("BAL","BOS","CHW","CLE","DET","HOU","KCR","LAA","MIN","NYY",
                  "OAK","SEA","TBR","TEX","TOR")
    
    nl_teams <- c("ARI","ATL","CHC","CIN","COL","LAD","MIA","MIL","NYM","PHI",
                  "PIT","SDP","SFG","STL","WSN")
    
    if (Team %in% al_teams) {
      df <- mlb_standings(season = 2022, date = Sys.Date(), league_id = 103)
    } else if (Team %in% nl_teams) {
      df <- mlb_standings(season = 2022, date = Sys.Date(), league_id = 104)
    }
    
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
      filter(team_records_wild_card_rank %in% (1:3))
    
    wild_card_teams <- df$team_records_team_name
    
    previous_seasons <- df2017_2021 %>%
      filter(Tm == Team)
    
    playoff_team_wins_2022 <- filter(df2022_1, Tm %in% wild_card_teams) %>%
      group_by(Gm) %>%
      mutate(
        min_playoff_wins = min(wins_so_far, na.rm = T),
        max_playoff_wins = max(wins_so_far, na.rm = T)
      )
    
    team_wins <- filter(df2022_1, Tm == Team)
    
    if (Team == "PHI") {
      color <- c("#E81828","#002d72","#000000")
    } else if (Team == "ARI") {
      color <- c("#A71930","#E3D4AD","#000000")
    } else if (Team == "ATL") {
      color <- c("#CE1141","#13274F","#EAAA00")
    } else if (Team == "BAL") {
      color <- c("#DF4601","#000000","#000000")
    } else if (Team == "BOS") {
      color <- c("#BD3039","#0C2340","#000000")
    } else if (Team == "CHC") {
      color <- c("#0E3386","#CC3433","#000000")
    } else if (Team == "CHW") {
      color <- c("#27251F","#C4CED4","#000000")
    } else if (Team == "CIN") {
      color <- c("#C6011F","#000000","#000000")
    } else if (Team == "CLE") {
      color <- c("#00385D","#E50022","#000000")
    } else if (Team == "COL") {
      color <- c("#333366","#C4CED4","#131413")
    } else if (Team == "DET") {
      color <- c("#0C2340","#FA4616","#000000")
    } else if (Team == "HOU") {
      color <- c("#002D62","#EB6E1F","#F4911E")
    } else if (Team == "KCR") {
      color <- c("#004687","#BD9B60","#000000")
    } else if (Team == "LAA") {
      color <- c("#003263","#BA0021","#862633")
    } else if (Team == "LAD") {
      color <- c("#005A9C","#EF3E42","#000000")
    } else if (Team == "MIA") {
      color <- c("#00A3E0","#FF6600","#FFD100")
    } else if (Team == "MIL") {
      color <- c("#ffc52f","#12284b","#000000")
    } else if (Team == "MIN") {
      color <- c("#002B5C","#D31145","#B9975B")
    } else if (Team == "NYM") {
      color <- c("#002D72","#FF5910","#000000")
    } else if (Team == "NYY") {
      color <- c("#0C2340","#E4002C","#000000")
    } else if (Team == "OAK") {
      color <- c("#003831","#EFB21E","#000000")
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
      color <- c("#003278","#C0111F","#000000")
    } else if (Team == "TOR") {
      color <- c("#134A8E","#1D2D5C","#E8291C")
    } else if (Team == "WSN") {
      color <- c("#AB0003","#14225A","#000000")
    } else {
      color <- c("000000","000000","000000")
    }
    
    most_recent_game_number <- max(team_wins$Gm)
    
    p1 <- ggplot(data = team_wins, aes(x = Gm)) +
      geom_line(aes(y = wins_so_far, color = paste(team_full, "Current Wins")), size = 2) +
      xlim(0,most_recent_game_number) +
      geom_smooth(data = previous_seasons, method = loess, aes(x = Gm, y = wins_so_far, color = paste(team_full, "Past Seasons")), se = FALSE, size = 2) +
      ggtitle(paste(
        team_full, "Wins Relative to Past Seasons & Current Playoff Contenders")) +
      labs(caption = "Data compiled from the baseballr package.",
           colour = "") +
      scale_color_manual(values = color) +
      theme(
        legend.position = "bottom",
        plot.background = element_blank()
      )
    
    if (Team %in% al_teams) {
      p2 <- p1 +
        geom_smooth(data = playoff_team_wins_2022, method = loess, aes(x = Gm, y = wins_so_far, color = "Current AL Wild Card Teams Average Wins"), se = FALSE, size = 2)
    } else if (Team %in% nl_teams) {
      p2 <- p1 +
        geom_smooth(data = playoff_team_wins_2022, method = loess, aes(x = Gm, y = wins_so_far, color = "Current NL Wild Card Teams Average Wins"), se = FALSE, size = 2)
    }
    
    
  }
  
  output$wins <- renderPlotly(
    wins_plot_shiny(input$team2)
  )
}

shinyApp(ui = ui, server = server)

