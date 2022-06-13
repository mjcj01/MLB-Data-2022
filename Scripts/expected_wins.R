library(tidyverse)
library(baseballr)

pythag_win_percent <- function(Team) {
  df_team <- df2022 %>%
    filter(Tm == Team)
  
  runs <- sum(df_team$R)
  runs_allowed <- sum(df_team$RA)
  
  predicted_win <- ((runs^1.83) / ((runs^1.83) + (runs_allowed^1.83))) * max(df_team$Gm)
  
  predicted_win
}

pythag_win_percent("PHI")

df2022_win_percent <- df2022 %>%
  group_by(Tm) %>%
  mutate("predicted_win" = pythag_win_percent(Tm))

wins_vs_expected <- function(Team) {
  
  df <- df2022 %>%
    filter(Tm == Team)
  
  total_wins <- sum(df$Wins)
  
  expected_wins <- pythag_win_percent(Team)
  
  difference <- total_wins - expected_wins
  
  df1 <- data.frame(total_wins, expected_wins, difference)
  
  df1
}

wins_vs_expected("PHI")

df_expected <- lapply(Tm, wins_vs_expected) %>%
  bind_rows()

ggplot(data = df_expected, aes(x = expected_wins, y = total_wins, label = Tm)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  geom_text(hjust = 1.25, vjust = 1.25)
