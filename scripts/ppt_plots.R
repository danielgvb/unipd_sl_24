

#install.packages("ggplot2")
#install.packages("ggthemes")


library(readxl)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(scales)



aux_data <- read_excel("GitHub/unipd_sl_24/data/aux_data.xlsx", sheet = "r_data")

# Home ownership----------------
# Melt the data for ggplot


selected_df <- aux_data[, c("Year", "owns_home", "rents_home", "others")]

data_long <- melt(selected_df, id.vars = "Year", variable.name = "Series", value.name = "Value")

# Plot the data
# Create the plot
plot<- ggplot(data = data_long, aes(x = factor(Year), y = Value, fill = Series)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Home Ownership in Colombia by Household",
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_economist() +  # Apply The Economist theme
  scale_fill_economist() +  # Use the correct color scale for fill
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  geom_text(aes(label = scales::percent(Value, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 3)

ggsave(filename = "home_ownership.png", plot = plot, width = 16, height = 6)  # Adjust the height as needed


# Economic Context------------------
aux_data_gdp <- aux_data[, c("Year", "col_gdp_growth", "italy_gdp_growth", "oecd_gdp_growth")]
aux_data_cpi <- aux_data[, c("Year", "colombia_inflation", "italy_inflation", "oecd_inflation")]

# Reshape the data for ggplot
aux_data_long_gdp <- reshape2::melt(aux_data_gdp, id.vars = "Year", variable.name = "Series", value.name = "Growth")
aux_data_long_cpi <- reshape2::melt(aux_data_cpi, id.vars = "Year", variable.name = "Series", value.name = "Inflation")


# Create the plot using ggplot2 and Economist style
# Create the plot
plot <- ggplot(data = aux_data_long_gdp, aes(x = Year, y = Growth, color = Series)) +
  geom_line(size = 1) +
  theme_economist() +  
  scale_colour_economist() +
  labs(title = "Real GDP Growth",
       x = NULL,
       y = NULL,
       color = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = aux_data$Year)

# Save the plot with adjusted height
ggsave(filename = "gdp_growth_plot.png", plot = plot, width = 8, height = 6)  # Adjust the height as needed





plot <- ggplot(data = aux_data_long_cpi, aes(x = Year, y = Inflation, color = Series)) +
  geom_line(size = 1) +
  theme_economist() +  
  scale_colour_economist() +
  labs(title = "CPI Inflation",
       x = NULL,
       y = NULL,
       color = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = aux_data$Year)

# Save the plot with adjusted height
ggsave(filename = "cpi_plot.png", plot = plot, width = 8, height = 6)  # Adjust the height as needed






ggplot(data = aux_data_long_cpi, aes(x = Year, y = Inflation, color = Series)) +
  geom_line(size = 1) +
  theme_economist() +  scale_colour_economist() +
  labs(title = "Inflation",
       x = NULL,
       y = NULL,
       color = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  ) +
  scale_x_continuous(breaks = aux_data$Year)
