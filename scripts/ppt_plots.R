

install.packages("ggplot2")
install.packages("ggthemes")


library(readxl)
library(ggplot2)
library(ggthemes)

aux_data <- read_excel("GitHub/unipd_sl_24/data/aux_data.xlsx", sheet = "r_data")
aux_data_gdp <- aux_data[, c("Year", "col_gdp_growth", "italy_gdp_growth", "oecd_gdp_growth")]
aux_data_cpi <- aux_data[, c("Year", "colombia_inflation", "italy_inflation", "oecd_inflation")]

# Reshape the data for ggplot
aux_data_long_gdp <- reshape2::melt(aux_data_gdp, id.vars = "Year", variable.name = "Series", value.name = "Growth")
aux_data_long_cpi <- reshape2::melt(aux_data_cpi, id.vars = "Year", variable.name = "Series", value.name = "Inflation")


# Create the plot using ggplot2 and Economist style
ggplot(data = aux_data_long_gdp, aes(x = Year, y = Growth, color = Series)) +
  geom_line(size = 1) +
  theme_economist() +
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




ggplot(data = aux_data_long_cpi, aes(x = Year, y = Inflation, color = Series)) +
  geom_line(size = 1) +
  theme_economist() +
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
