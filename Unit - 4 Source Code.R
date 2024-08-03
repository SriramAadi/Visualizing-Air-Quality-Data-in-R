
library(tidyverse)
library(ggplot2)
library(heatmaply)

view(airquality)

air_quality_data <- airquality


air_quality_data_clean <- air_quality_data %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))


air_quality_data_clean %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Histograms for Each Variable")


air_quality_data_clean %>%
  ggplot(aes(x = factor(round(Wind)), y = Ozone)) +
  geom_boxplot() +
  ggtitle("Boxplots for Ozone by Wind Values")


air_quality_data_clean %>%
  mutate(Date = as.Date(paste(1973, Month, Day, sep = "-"))) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Ozone, color = "Ozone")) +
  geom_line(aes(y = Temp, color = "Temp")) +
  geom_line(aes(y = Wind, color = "Wind")) +
  geom_line(aes(y = Solar.R, color = "Solar.R")) +
  ggtitle("Line Chart for Ozone, Temp, Wind, and Solar") +
  scale_color_manual(values = c("Ozone" = "blue", "Temp" = "red", "Wind" = "green", "Solar.R" = "orange"))


heatmaply(cor(air_quality_data_clean[, c("Ozone", "Temp", "Wind", "Solar.R")]),
          main = "Heatmap of Correlation Matrix")


air_quality_data_clean %>%
  ggplot(aes(x = Wind, y = Temp, size = Ozone, color = Solar.R)) +
  geom_point() +
  ggtitle("Scatter Chart: Wind vs Temp, Size = Ozone, Color = Solar.R")


