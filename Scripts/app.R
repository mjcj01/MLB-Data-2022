library(shiny)
library(tidyverse)
library(plotly)
library(rsconnect)
library(baseballr)
library(showtext)
library(ggthemes)

font_add_google("Roboto Condensed")
showtext_auto()

ui <- fluidPage(
  
  titlePanel("MLB Win Predictions"),
  sidebarLayout(
    sidebarPanel(selectInput(inputId = "team",
                             label = NULL,
                             choices = c(Tm),
                             selected = NULL),
                 selectInput(inputId = "opponent",
                             label = NULL,
                             choices = c(Tm),
                             selected = NULL)),
    mainPanel(plotlyOutput("run_dist")
        )
    )
)

server <- function(input, output) {
  teamInput <- reactive({
    df2022 %>% filter(Tm == input$team)
  })
  
  opponentInput <- reactive({
    df2022 %>% filter(Tm == input$opponent)
  })
  
  run_dist <- function(Team, Opponent) {
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
    
    df_2
  }
  
  output$run_dist <- renderPlotly({
    
    p1 <- run_dist(input$team, input$opponent) %>%
    ggplot(aes(x = c(-20:20))) +
      geom_col(aes(y = RunProbability), fill = "#6d3c3c") +
      scale_x_continuous(breaks = c(-5,0,5), limits = c(-5.5,5.5)) +
      ylim(0,0.2) +
      labs(x = paste("Runs", input$team, "Wins/Loses By"),
           y = "Probability",
           caption = "Data compiled from the baseballr package.") +
      ggtitle(paste(input$team, "vs.", input$opponent)) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.5),
        plot.title = element_text(size = 20),
        axis.ticks = element_line(color = "grey"),
        axis.title = element_text(),
        panel.background = element_blank()
      )
    
  })
}

shinyApp(ui = ui, server = server)
