library(ggplot2)
library(dplyr)
library(readr)

# Load and select relevant columns
premier_league <- read_csv("621 - Gambling/Group Project 2/PremierLeagueMaster_621class(1).csv")
selected_columns <- c("Date", "HomeTeam", "AwayTeam", "FTR", "B365H", "B365D", "B365A", "BWH", "BWD", "BWA")
new_pl_data <- premier_league %>% select(all_of(selected_columns))

# Function to calculate returns based on betting odds and results
calculate_returns <- function(odds, bet_on, result) {
  ifelse(bet_on == result, (odds - 1) * 1, -1)
}

# Calculate returns for each bookmaker and outcome
new_pl_data <- new_pl_data %>%
  mutate(
    Return_B365H = calculate_returns(B365H, "H", FTR),
    Return_B365D = calculate_returns(B365D, "D", FTR),
    Return_B365A = calculate_returns(B365A, "A", FTR),
    Return_BWH = calculate_returns(BWH, "H", FTR),
    Return_BWD = calculate_returns(BWD, "D", FTR),
    Return_BWA = calculate_returns(BWA, "A", FTR)
  )

# Function to plot the distribution of odds
plot_odds_distribution <- function(data, column1, column2, title) {
  ggplot(data, aes(x = !!sym(column1))) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
    geom_histogram(aes(x = !!sym(column2)), bins = 30, fill = "red", alpha = 0.5) +
    labs(title = title, x = "Odds", y = "Frequency")
}

# Function to plot the distribution of returns
plot_returns_distribution <- function(data, column1, column2, title) {
  ggplot(data, aes(x = !!sym(column1))) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
    geom_histogram(aes(x = !!sym(column2)), bins = 30, fill = "red", alpha = 0.5) +
    labs(title = title, x = "Returns", y = "Frequency")
}

# Plot distributions for odds and returns
plot_odds_distribution(new_pl_data, "B365H", "BWH", "Distribution of Home Win Odds")
plot_odds_distribution(new_pl_data, "B365D", "BWD", "Distribution of Draw Odds")
plot_odds_distribution(new_pl_data, "B365A", "BWA", "Distribution of Away Win Odds")

plot_returns_distribution(new_pl_data, "Return_B365H", "Return_BWH", "Distribution of Returns for Home Wins")
plot_returns_distribution(new_pl_data, "Return_B365D", "Return_BWD", "Distribution of Returns for Draws")
plot_returns_distribution(new_pl_data, "Return_B365A", "Return_BWA", "Distribution of Returns for Away Wins")

# Calculate average returns for each bookmaker and outcome
average_returns <- new_pl_data %>% summarise(
  Avg_Return_B365H = mean(Return_B365H, na.rm = TRUE),
  Avg_Return_B365D = mean(Return_B365D, na.rm = TRUE),
  Avg_Return_B365A = mean(Return_B365A, na.rm = TRUE),
  Avg_Return_BWH = mean(Return_BWH, na.rm = TRUE),
  Avg_Return_BWD = mean(Return_BWD, na.rm = TRUE),
  Avg_Return_BWA = mean(Return_BWA, na.rm = TRUE)
)
print(average_returns)

# Hot hand analysis for each team
new_pl_data_hothand <- new_pl_data %>%
  arrange(HomeTeam, Date) %>%
  group_by(HomeTeam) %>%
  mutate(
    Win = FTR == "H",
    Streak = cumsum(Win != lag(Win, default = first(Win)))
  ) %>%
  mutate(
    Streak = ifelse(Win, cumsum(Win) - cumsum(Win != lag(Win, default = first(Win))), 0)
  )

# Summarize hot hand analysis
hot_hand_analysis <- new_pl_data_hothand %>%
  group_by(HomeTeam) %>%
  summarise(
    Avg_Odds_During_Streak = mean(B365H[Streak > 0], na.rm = TRUE),
    Avg_Returns_During_Streak = mean(Return_B365H[Streak > 0], na.rm = TRUE),
    Avg_Odds_Non_Streak = mean(B365H[Streak == 0], na.rm = TRUE),
    Avg_Returns_Non_Streak = mean(Return_B365H[Streak == 0], na.rm = TRUE)
  )
