# Load necessary libraries
library(ggplot2)
library(lattice)
library(leaflet)
library(reshape2)

# Load the air quality dataset
data(airquality)

# Clean the data
airquality_clean <- na.omit(airquality)

# Create histograms for each of the variables
ggplot(airquality_clean, aes(Ozone)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  ggtitle("Histogram of Ozone")

ggplot(airquality_clean, aes(Solar.R)) + 
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + 
  ggtitle("Histogram of Solar Radiation")

ggplot(airquality_clean, aes(Wind)) + 
  geom_histogram(binwidth = 0.5, fill = "green", color = "black") + 
  ggtitle("Histogram of Wind")

ggplot(airquality_clean, aes(Temp)) + 
  geom_histogram(binwidth = 1, fill = "red", color = "black") + 
  ggtitle("Histogram of Temperature")

# Create boxplot for Ozone
ggplot(airquality_clean, aes(y = Ozone)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Ozone")

# Create boxplots for different wind values (rounded)
airquality_clean$Wind_round <- round(airquality_clean$Wind)
ggplot(airquality_clean, aes(x = factor(Wind_round), y = Ozone)) + 
  geom_boxplot() + 
  xlab("Wind (rounded)") + 
  ggtitle("Boxplot of Ozone by Rounded Wind Values")

# Explore how the data changes over time
# Create dates
airquality_clean$Date <- as.Date(paste(1973, airquality_clean$Month, airquality_clean$Day, sep = "-"))

# Create line charts for ozone, temp, wind, and solar.R
ggplot(airquality_clean, aes(Date, Ozone)) + 
  geom_line(color = "blue") + 
  ggtitle("Ozone Levels Over Time")

ggplot(airquality_clean, aes(Date, Temp)) + 
  geom_line(color = "red") + 
  ggtitle("Temperature Over Time")

ggplot(airquality_clean, aes(Date, Wind)) + 
  geom_line(color = "green") + 
  ggtitle("Wind Speed Over Time")

ggplot(airquality_clean, aes(Date, Solar.R)) + 
  geom_line(color = "orange") + 
  ggtitle("Solar Radiation Over Time")

# Create a combined line chart
ggplot(airquality_clean) +
  geom_line(aes(Date, Ozone, color = "Ozone")) +
  geom_line(aes(Date, Temp, color = "Temp")) +
  geom_line(aes(Date, Wind, color = "Wind")) +
  geom_line(aes(Date, Solar.R, color = "Solar.R")) +
  ggtitle("Combined Line Chart of Ozone, Temperature, Wind, and Solar Radiation") +
  ylab("Values") + 
  scale_color_manual(values = c("blue", "red", "green", "orange"))

# Create a heatmap
airquality_melt <- melt(airquality_clean, id.vars = "Date")
ggplot(airquality_melt, aes(Date, variable, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") +
  ggtitle("Heatmap of Air Quality Data")

# Create a scatter chart
ggplot(airquality_clean, aes(Wind, Temp, size = Ozone, color = Solar.R)) +
  geom_point() + 
  scale_size_continuous(range = c(1, 10)) +
  scale_color_gradient(low = "yellow", high = "red") +
  ggtitle("Scatter Chart of Air Quality Data")

# Final analysis
# After exploring the data, we can observe that temperature and solar radiation have some correlation with ozone levels.
# The most useful visualization was the scatter chart which provided a clear view of how different variables interact with each other.
