# The Canada-U.S. Productivity Growth Divergence, 1961 - 2024

# Load packages
library(tidyverse)
library(cansim)
library(datapasta)
library(stats)
library(broom)
library(janitor)
library(lubridate)
library(readxl)
library(knitr)

# Download annual data
cn_data <- get_cansim("36-10-0208-01", factors = TRUE)
cn_data <- cn_data %>% 
 clean_names()

# Filter the data
cn_data <- cn_data %>%
 rename(sector = north_american_industry_classification_system_naics,
        year = ref_date,
        var_names = multifactor_productivity_and_related_variables) %>% 
 select(year, sector, var_names, value) 

cn_data_wide <- cn_data %>%
 pivot_wider(id_cols = c(year, sector), names_from = var_names, values_from = value) %>% 
 rename(lp = "Labour productivity") %>% 
 select(year, sector, lp) %>% 
 filter(sector == "Business sector") %>% 
 mutate(year = as.numeric(year))

# Download quarterly data
cn_qdata <- get_cansim("36-10-0206-01", factors = TRUE)
cn_qdata <- cn_qdata %>% 
 clean_names()

# Filter the data
cn_qdata <- cn_qdata %>%
 rename(quarter = ref_date,
        var_names = labour_productivity_measures_and_related_measures) %>% 
 select(quarter, sector, var_names, value)

cn_qdata_wide <- cn_qdata %>%
 pivot_wider(id_cols = c(quarter, sector), names_from = var_names, values_from = value) %>% 
 rename(lp = "Labour productivity") %>% 
 select(quarter, sector, lp) %>% 
 filter(sector == "Business sector")

cn_qdata_wide <- cn_qdata_wide %>%
 mutate(date = ym(quarter),          # Parse as Year-Month
        year = year(date))        # Extract the year

# Aggregate by year and calculate the average
cn_adata_wide <- cn_qdata_wide %>%
 group_by(year, sector) %>%
 summarise(lp = mean(lp, na.rm = TRUE), .groups = "drop")

cn_lp <- full_join(cn_data_wide, cn_adata_wide, by = c("year", "sector"),
                   suffix = c("", ".q"))
cn_lp <- cn_lp %>% 
 mutate(lp = case_when(year > 2017 ~ lp.q,
                       TRUE ~ lp)) %>% 
 select(-lp.q) %>% 
 mutate(country = "Canada") %>% 
 mutate(sector = as.character(sector))

cn_lp <- cn_lp %>% 
 mutate(lp = 100 * lp / lp[which(year == 1961)])

# Get BLS Data
# Set the file path
file_path <- "./posts/cn_us_prdty_gap/labor-productivity-major-sectors.xlsx"

# Read the "Annual" worksheet
us_data <- read_excel(file_path, sheet = "Annual", skip = 2) %>% 
 clean_names()

# Filter for rows of "Measure" to keep "Labor productivity"
us_data <- us_data %>%
 filter(measure == "Labor productivity") %>% 
 filter(sector == "Business sector") %>% 
 filter(units == "Index (2017=100)") %>% 
 select(-c(units, basis, measure))

us_data_long <- us_data %>%
 pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "lp") 

us_lp <- us_data_long %>% 
 mutate(year = as.numeric(substr(year,2,5)), lp = as.numeric(lp)) %>% 
 mutate(country = "United States") %>% 
 filter(year >= 1961)

us_lp <- us_lp %>% 
 mutate(lp = 100 * lp / lp[which(year == 1961)])

cn_us_data <- bind_rows(cn_lp, us_lp)

cn_us_data %>% 
 filter(year %in% c(1961, 2000, 2019, 2024)) %>% 
 group_by(country) %>% 
 mutate(cagr = 100 * ((lp / lag(lp))^(1/(year - lag(year))) - 1)) %>% 
 select(year, cagr, country) %>% 
 knitr::kable()

# Create the plot
ggplot(cn_us_data, aes(x = year, y = lp, color = country)) +
 geom_line(linewidth = 1.25) + # Add the line graph
 geom_vline(xintercept = 2000, linetype="dashed", linewidth = .75) + # Add the dashed line for the year 2000
 geom_vline(xintercept = 2019, linetype="dashed", linewidth = .75) + # Add the dashed line for the year 2019
 labs(title = "Canadian and U.S. Business Sector Labour Productivity, 1961-2024",
      x = NULL, y = "Index 100 = 1961", colour = "Country",
      caption = " Data sources: Statistics Canada, Table 36-10-0208-01 and Table 36-10-0208-01; Bureau of Labor Statistics, Business sector data \n are from the data file Labor productivity by major sectors: nonfarm business, business, nonfinancial corporate, and manufacturing, \n https://www.bls.gov/productivity/tables; and author's calculations.") +
 expand_limits(x = c(1961, 2031)) +
 theme_minimal() +
 theme(title = element_text(size = 15, colour = "black", face = "bold", hjust = 0)) +
 theme(axis.text.y = element_text(size = 10.5, colour = "black")) +
 theme(axis.text.x = element_text(size = 10.5, colour = "black")) +
 theme(axis.title.x = element_blank()) +
 theme(axis.title.y = element_text(size = 12, colour = "black", face = "plain", hjust = .5)) +
 theme(panel.grid.major.y = element_line(linewidth = .5, colour = "grey80")) +
 theme(legend.title = element_blank()) +
 theme(legend.text = element_text(size = 12)) +
 theme(panel.grid.major.x = element_blank()) +
 theme(panel.grid.minor.x = element_blank()) +
 theme(panel.grid.minor.y = element_blank()) +
 theme(legend.position="bottom") +
 scale_color_manual(labels = c("Canada", "United States"), values = c("red", "blue")) +
 scale_y_continuous(breaks = seq(100, 450, 50)) +
 scale_x_continuous(breaks = seq(1961, 2026, 5)) +
 theme(plot.caption = element_text(size = 9, face = "plain", hjust = 0)) + 
 annotate("text", x = 1975, y = 275, label = "1961 - 2000 \n Canada: 2.31% \n US: 2.22%",
          size = 4, hjust = 0.5) +
 annotate("text", x = 2010, y = 175, label = "2000 - 2019 \n Canada: 0.87% \n US: 1.92%",
          size = 4, hjust = 0.5) +
 annotate("text", x = 2028, y = 325, label = "2019 - 2024 \n Canada: 0.26% \n US: 2.10%",
          size = 4, hjust = 0.5)

ggsave('./posts/cn_us_prdty_gap/cn_us_lp_gap_61-24.svg', 
       width = 10, height = 5.625, units = "in", dpi = 192, bg = "white")
