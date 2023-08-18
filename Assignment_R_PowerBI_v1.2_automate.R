# Load necessary libraries
library(tidyverse)
library(scales)

# Define a function to read data
read_data <- function(file_dir, file_name) {
  full_file_path <- file.path(file_dir, file_name)
  read.csv(full_file_path)
}

# Define a function to clean data
clean_data <- function(df) {
  data_cleaned <- df %>% drop_na()
  data_cleaned <- data_cleaned %>%
    mutate(across(c(Profitability, Worldwide.Gross), ~round(., digits = 2)))
  data_cleaned
}

# Define a function to remove outliers
remove_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], .25)
  Q3 <- quantile(data[[column_name]], .75)
  IQR <- IQR(data[[column_name]])
  no_outliers <- subset(data, data[[column_name]] > (Q1 - 1.5*IQR) & data[[column_name]] < (Q3 + 1.5*IQR))
  no_outliers
}

# Define a function to draw boxplots
draw_boxplot <- function(data, x_col, y_col) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 19) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(ylim = c(0, 1000))
}

# Define a function to perform univariate analysis
perform_univariate_analysis <- function(data, column_name) {
  hist(data[[column_name]], breaks = 10)
  ggplot(data, aes_string(x = column_name)) + geom_density()
}

# Define a function to perform bivariate analysis
perform_bivariate_analysis <- function(data, x_col, y_col) {
  ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 19) +
    scale_x_continuous(labels = comma) +
    coord_cartesian(ylim = c(0, 10))
}

# Load the data
file_dir <- "C:/Users/SSNQ/Documents/Skills Data Technician BootCamp/WK11 Learning R/"
file_name <- "HollywoodsMostProfitableStories.csv"
df <- read_data(file_dir, file_name)

# Clean the data
cleaned_data <- clean_data(df)

# Remove outliers
no_outliers_W <- remove_outliers(cleaned_data, "Worldwide.Gross")
no_outliers_Ad <- remove_outliers(no_outliers_W, "Audience..score..")
no_outliers_Bi <- remove_outliers(no_outliers_Ad, "Profitability")

# Draw boxplots
draw_boxplot(no_outliers_W, "Profitability", "Worldwide.Gross")
draw_boxplot(no_outliers_Ad, "Audience..score..", "Profitability")

# Perform univariate analysis
perform_univariate_analysis(no_outliers_W, "Profitability")

# Perform bivariate analysis
perform_bivariate_analysis(no_outliers_Bi, "Audience..score..", "Profitability")

# ... (continue with the rest of your analysis)
