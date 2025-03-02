# Load required libraries
library(tidyverse)
library(ggbump)
library(ggrepel)  # For automatic label adjustment
library(openxlsx)
library(janitor)

dat <- read.xlsx("posts/mobile_data_prices/worldwide_mobile_data_pricing_data.xlsx", 
                 sheet = "Historical Data")
dat <- dat %>% 
 select(Name, `Average.price.of.1GB.(USD.–.2023)`, `Average.price.of.1GB.(USD.–.2022)`,
        `Average.price.of.1GB.(USD.–.2021)`, `Average.price.of.1GB.(USD.–.2020)`,
        `Average.price.of.1GB.(USD.–.2019)`, X5, X7, X9, X11, X13) %>% 
 rename(Country = Name, '2019' = X13, '2020' = X11, '2021' = X9, 
        '2022' = X7, '2023' = X5,
        Price_2023 = `Average.price.of.1GB.(USD.–.2023)`,
        Price_2022 = `Average.price.of.1GB.(USD.–.2022)`,
        Price_2021 = `Average.price.of.1GB.(USD.–.2021)`,
        Price_2020 = `Average.price.of.1GB.(USD.–.2020)`,
        Price_2019 = `Average.price.of.1GB.(USD.–.2019)`) 

dat <- dat %>% 
 filter(Country %in% c("Italy", "France", "Australia", "Finland", "United Kingdom", "Mexico", 
                    "Germany", "Sweden", "Japan", "Norway", "Canada", "United States",  
                    "China"))

dat <- dat %>%
 mutate(across(starts_with("Price_"), ~as.numeric(.)))

# Convert data to long format
dat_long <- dat %>%
 pivot_longer(cols = starts_with("Price_"), names_to = "Year", values_to = "Price") %>%
 filter(grepl("^Price_\\d{4}$", Year)) %>%  # Keep only valid "Price_YYYY" patterns
 mutate(Year = as.numeric(gsub("Price_", "", Year))) %>%
 left_join(
  pivot_longer(dat, cols = -Country, names_to = "Year", values_to = "Rank") %>%
   filter(grepl("^\\d{4}$", Year)) %>%  # Ensure Year is only four-digit numbers
   mutate(Year = as.numeric(Year)),
  by = c("Country", "Year")
 )

# Format price as currency
dat_long <- dat_long %>%
 mutate(Price = round(as.numeric(Price), 2)) %>% 
 mutate(Price_Label = paste0("$", formatC(Price, format = "f", digits = 2)))  # Format as "$X.XX"


# Define custom colors:
custom_colors <- c(
  "Canada" = "red", 
  "United States" = "blue", 
  "Italy" = "deepskyblue", 
  "France" = "purple",
  "Australia" = "darkorange", 
  "Finland" = "darkcyan", 
  "United Kingdom" = "navy",  
  "Mexico" = "#480607", 
  "Germany" = "darkgreen",   
  "Sweden" = "#32127a", 
  "Japan" = "#cc7722", 
  "Norway" = "black",
  "China" = "#ae0c00"
)

# Extract data for country labels at the first year (2019) and last year (2023)
label_data_left <- dat_long %>% filter(Year == 2019)
label_data_right <- dat_long %>% filter(Year == 2023)

# Plot the improved bump chart with white-background price labels
ggplot(dat_long, aes(x = Year, y = Rank, group = Country, color = Country)) +
  geom_bump(linewidth = 2, smooth = 10) +  
  geom_point(size = 1, color = "black") +  
  # Price labels with a white background and black border
  geom_label_repel(data = dat_long, aes(label = Price_Label), 
                   size = 3.5, fontface = "bold", color = "black",
                   fill = "white", label.size = 0.3,  # White background with thin black border
                   segment.size = 0.3, segment.color = "gray50",
                   box.padding = 0.3, force = 5) +
  # Labels on the left (2019) with leader lines
  geom_text_repel(data = label_data_left, aes(label = Country), 
                  hjust = 1, size = 6, fontface = "bold", 
                  nudge_x = -1.4, segment.size = 0.5, segment.color = "gray50") +
  # Labels on the right (2023) with leader lines
  geom_text_repel(data = label_data_right, aes(label = Country), 
                  hjust = 0, size = 6, fontface = "bold", 
                  nudge_x = 1.4, segment.size = 0.5, segment.color = "gray50") +
  scale_y_reverse(breaks = seq(0, 225, by = 25)) +  # Scale y-axis in intervals of 25
  scale_x_continuous(breaks = seq(2019, 2023, by = 1)) +  # Ensure only 2019-2023 appear
  scale_color_manual(values = custom_colors) +  # Assign custom colors
  coord_cartesian(xlim = c(2018.0, 2024.0), clip = "off") +  # Adjust space for labels
  labs(title = "The Cost of 1GB Mobile Data (2019-2023, USD)",
       x = NULL,  # Remove "Year" from x-axis
       y = "Rank (Lower is Better)",
       caption = "Source: Image by author. Data from Cable.co.uk, \"The cost of 1GB of mobile data in 237 countries, 2023\". https://www.cable.co.uk/mobiles/worldwide-data-pricing. 
       Prices are in USD at September 2023 exchange rates.") +  # Add footnote
  theme_minimal() +
  theme(
    legend.position = "none",  # Hide legend for clarity
    text = element_text(size = 16),  # Adjust general text size
    plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
    axis.text = element_text(size = 16, color = "black"),  # Increase axis text size and set to black
    axis.title = element_text(size = 22, color = "black"),  # Increase axis labels and set to black
    panel.grid.major = element_line(color = "black", linetype = "dotted"),  # Darker grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.caption = element_text(size = 12, hjust = 0, face = "italic"),  # Left-align footnote
    plot.caption.position = "plot",  # Ensure caption is placed within the plot area
    plot.margin = margin(t = 20, r = 120, b = 20, l = 110)  # Increase left/right margins
  )

ggsave("posts/mobile_data_prices//bump_chart.svg", width = 16, height = 9, device = "svg")
ggsave("posts/mobile_data_prices//bump_chart.png", width = 16, height = 9, dpi = 300, bg = "white")
