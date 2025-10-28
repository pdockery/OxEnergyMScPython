# Clear all objects
rm(list = ls())

# Trigger garbage collection
gc()

# Print objects in the workspace
#print(ls())
# Install necessary packages

install.packages(c("urca", "dynlm", "lmtest", "strucchange", "tseries", "ARDL"))
required_packages <- c("languageserver", "ggplot2", "png", "grid"
  , "tseries", "jsonlite", "httpgd", "ggrepel"
  , "dplyr", "tidyr", "broom", "car", "MASS", "lme4"
  , "caret", "survival", "psych", "gridExtra", "zoo"
  ,"urca", "dynlm", "lmtest", "strucchange", "tseries", "ARDL")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load necessary libraries
library(languageserver)
library(ggplot2)
library(png)
library(grid)
library(tseries)
library(jsonlite)
library(httpgd)
library(ggrepel)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(MASS)
library(lme4)
library(caret)
library(survival)
library(psych)
library(gridExtra)
library(zoo)
library(patchwork)

getwd()
setwd("/Users/pdockery/Documents/MSc Energy Systems/Small Group/")

##################################################################
######### Data Fetching, Consolidation, and Manipulation #########
##################################################################

# Load data from assignment tables
Table_1 <- read.csv("Bangladesh_Table_1.csv")
Table_2 <- read.csv("Bangladesh_Table_2.csv")
Table_3 <- read.csv("Bangladesh_Table_3.csv")
Table_4 <- read.csv("Bangladesh_Table_4.csv")

# Fetch the data from: https://ourworldindata.org/grapher/national-gdp-constant-usd-wb
GDPbyCountry <- read.csv("https://ourworldindata.org/grapher/national-gdp-constant-usd-wb.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
#metadata <- fromJSON("https://ourworldindata.org/grapher/national-gdp-constant-usd-wb.metadata.json?v=1&csvType=full&useColumnShortNames=true")

View(GDPbyCountry[GDPbyCountry$Entity == "Bangladesh", ])

# Fetch the data https://ourworldindata.org/grapher/co2-by-source
CO2_Emissions_byIndustry_byCountry <- read.csv("https://ourworldindata.org/grapher/co2-by-source.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
#metadata <- fromJSON("https://ourworldindata.org/grapher/co2-by-source.metadata.json?v=1&csvType=full&useColumnShortNames=true")

View(CO2_Emissions_byIndustry_byCountry[CO2_Emissions_byIndustry_byCountry$Entity == "Bangladesh", ])

# Perform an inner join on GDPbyCountry and CO2_Emissions_byIndustry_byCountry based on Entity and Year
merged_data <- inner_join(GDPbyCountry, CO2_Emissions_byIndustry_byCountry, by = c("Entity", "Year", "Code"))

# View the merged data
View(merged_data[merged_data$Entity == "Bangladesh", ])

# Fetch the data https://ourworldindata.org/grapher/population
population_byCountry <- read.csv("https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
# metadata <- fromJSON("https://ourworldindata.org/grapher/population.metadata.json?v=1&csvType=full&useColumnShortNames=true")

View(population_byCountry[population_byCountry$Entity == "Bangladesh", ])

# Perform an inner join on merged_data and population_byCountry based on Entity and Year
merged_data <- inner_join(merged_data, population_byCountry, by = c("Entity", "Year", "Code"))

# Fetch the data https://ourworldindata.org/grapher/oil-production-by-country
world_oil <- read.csv("https://ourworldindata.org/grapher/oil-production-by-country.csv?v=1&csvType=full&useColumnShortNames=true")

View(world_oil[world_oil$Entity == "Bangladesh", ])

# Fetch the metadata
#metadata <- fromJSON("https://ourworldindata.org/grapher/oil-production-by-country.metadata.json?v=1&csvType=full&useColumnShortNames=true")

merged_data <- inner_join(merged_data, world_oil, by = c("Entity" = "Entity", "Code" = "Code", "Year" = "Year"))



# Add a column to merged_data that contains the region for each country
merged_data <- inner_join(merged_data, OWID_Regions, by = c("Entity" = "Entity", "Code" = "Code"))

merged_data$emissions_from_energy <- merged_data$emissions_from_gas +
                                        merged_data$emissions_from_oil + 
                                        merged_data$emissions_from_coal

print(names(merged_data))

library(jsonlite)

# Fetch the data https://ourworldindata.org/grapher/oil-consumption-by-country
oil_consumption <- read.csv("https://ourworldindata.org/grapher/oil-consumption-by-country.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
#metadata <- fromJSON("https://ourworldindata.org/grapher/oil-consumption-by-country.metadata.json?v=1&csvType=full&useColumnShortNames=true")

#merge oil consumption to merged_data
merged_data <- inner_join(merged_data, oil_consumption, by = c("Entity" = "Entity", "Code" = "Code", "Year" = "Year"))

# convert TWh to MMBD
merged_data$oil_production <- merged_data$oil_production__twh * 0.59 / 365 # convert TWh to MMBD
merged_data$oil_consumption <- merged_data$oil_consumption_twh * 0.59 / 365 # convert TWh to MMBD
View(merged_data[merged_data$Entity == "Bangladesh", ])
#drop column oil_production__twh
merged_data <- merged_data[, !names(merged_data) %in% c("oil_consumption_twh", "oil_production__twh")]

# Fetch the data https://ourworldindata.org/world-region-map-definitions
OWID_Regions <- read.csv("https://ourworldindata.org/grapher/continents-according-to-our-world-in-data.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
# metadata <- fromJSON("https://ourworldindata.org/grapher/continents-according-to-our-world-in-data.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# Remove "Year" column from OWID_Regions dataframe
OWID_Regions <- dplyr::select(OWID_Regions, -Year)
# Add a column to merged_data that contains the region for each country
merged_data <- inner_join(merged_data, OWID_Regions, by = c("Entity" = "Entity", "Code" = "Code"))

print(names(Table_1))

# Create a new data frame with the required columns that match the merged data set
equatoria_data <- data.frame(
  Entity = "Equatoria",
  Code = "EQA",
  Year = Table_1$StudyYear,
  ny_gdp_mktp_kd = Table_1$GDP_Real_USD_billions * 1000000000,
  emissions_from_other_industry = NA,
  emissions_from_flaring = NA,
  emissions_from_cement = NA,
  emissions_from_gas = NA,
  emissions_from_oil = NA,
  emissions_from_coal = NA,
  population_historical = Table_1$Population_mm * 1000000,
  owid_region = "Global South",
  emissions_from_energy = Table_1$Emissions_CO2_Energy_mt * 1000000
  )
#join Table 2 to Equatoria data 
equatoria_data <- inner_join(equatoria_data, Table_2, by = c("Year" = "StudyYear"))

print(names(equatoria_data))
print(names(merged_data))

# drop the oil price column from equatoria_data
equatoria_data <- equatoria_data[, !names(equatoria_data) %in% c("Oil_Price_USDpB")]

# Rename the columns in equatoria_data to match the merged data set
equatoria_data <- equatoria_data %>%
  dplyr::rename(
    oil_production = Oil_Production_mmbd,
    oil_consumption = Oil_Consumption_mmbd
  )

# Append the new data to the merged data frame, ensuring column names match
merged_data <- rbind(merged_data, equatoria_data)

View(merged_data[merged_data$Entity == "Equatoria", ])

# View the merged data
View(merged_data[merged_data$Entity == "Bangladesh", ])
View(merged_data)
#view list of distinct entities in merged_data
View(distinct(merged_data, Entity, .keep_all = TRUE))


merged_data$OECD_Label <- 0
OECD_Countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile",
                    "Colombia", "Costa Rica", "Czechia", "Denmark", "Estonia",
                    "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland"
                    , "Israel", "Italy", "Japan", "South Korea", "Latvia", "Lithuania", "Luxembourg"
                    , "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal"
                   ,"Slovakia" , "Slovenia", 
                    "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")
merged_data$OECD_Label[merged_data$Entity %in% OECD_Countries] <- 1
merged_data$gdp_per_capita <- merged_data$ny_gdp_mktp_kd / merged_data$population_historical
merged_data$emissions_per_capita <- merged_data$emissions_from_energy / merged_data$population_historical

# Fetch the data https://ourworldindata.org/grapher/energy-consumption-by-source-and-country?stackMode=absolute
Primary_Energy_Consumption <- read.csv("https://ourworldindata.org/grapher/energy-consumption-by-source-and-country.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
#metadata <- fromJSON("https://ourworldindata.org/grapher/energy-consumption-by-source-and-country.metadata.json?v=1&csvType=full&useColumnShortNames=true")

# View the data for Bangladesh
View(Primary_Energy_Consumption[Primary_Energy_Consumption$Entity == "Bangladesh", ])

# create a new data frame with two columns, one Source and the other "Value"
equatoria_energy_data <- data.frame(
  Entity = "Equatoria",
  Code = "EQA",
  Year = 2023,
  primary_energy = c("Oil", "Natural Gas", "Coal", "Nuclear energy", "Hydro electric", "Renewables"),
  Primary_energy_amount = c(.271, .058, .563, .011, .036, .061 )
)
View(equatoria_energy_data)
##################################################################
######### Equatoria's Global Standing Graphs #####################
##################################################################

# Create a new data frame for 2022 values from the merged data
ourworldindata_co2_gdp <- merged_data[merged_data$Year == 2022, ]
View(ourworldindata_co2_gdp)

# Add a column to indicate if the row should be labeled in charts
ourworldindata_co2_gdp$label <- 0
countries_to_label <- c("Bengladesh", "India", "Russia", "Bangladesh", "United States", 
  "Norway", "Nigeria", "France", "United Kingdom", 
  "China", "Japan", "Brazil", "South Africa",
  "Australia", "Canada", "Saudi Arabia",
  "Democratic Republic of Congo", "Venezuela",
  "North Korea", "Ethiopia"
  )
ourworldindata_co2_gdp$label[ourworldindata_co2_gdp$Entity %in% countries_to_label] <- 1

#calculate average gdp per capita for OECD countries in the ourworldindata_co2_gdp data frame
oecd_avg_gdp_pc <- mean(ourworldindata_co2_gdp$gdp_per_capita[ourworldindata_co2_gdp$OECD_Label == 1], na.rm = TRUE)
print(oecd_avg_gdp_pc)
#multiply the oecd_avg_gdp_pc by Equatoria's population in 2022 to get the total GDP
bangladesh_population_2022 <- ourworldindata_co2_gdp$population_historical[ourworldindata_co2_gdp$Entity == "Bangladesh" & ourworldindata_co2_gdp$Year == 2022]
bangladesh_gdp_target <- oecd_avg_gdp_pc * bangladesh_population_2022
options(scipen = 999)  # Turn off scientific notation
print(bangladesh_gdp_target)
options(scipen = 0)  # Reset to default

#calcualte Equatoria's emissions intensitive in 2005 by dividing gdp by emissions from energy
bangladesh_emissions_intensity_2005 <- merged_data$emissions_from_energy[merged_data$Entity == "Equatoria" & merged_data$Year == 2005] / merged_data$ny_gdp_mktp_kd[merged_data$Entity == "Equatoria" & merged_data$Year == 2005]
print(equatoria_emissions_intensity_2005)

#calculate a 45% reduction from Equatoria's emissions intensity in 2005
equatoria_emissions_intensity_INDC_Target <- equatoria_emissions_intensity_2005 * 0.55

options(scipen = 999)  # Turn off scientific notation
print(equatoria_emissions_intensity_INDC_Target)
options(scipen = 0)  # Reset to default

#calculate target total emissions for Equatoria in 2040 by dividing the target gdp by the target emissions intensity
equatoria_target_total_emissions_2040 <- equatoria_gdp_target * equatoria_emissions_intensity_INDC_Target

print(equatoria_target_total_emissions_2040)

# Check math by dividing target GDP by target total emissions
equatoria_target_emissions_intensity_2040 <- equatoria_target_total_emissions_2040 / equatoria_gdp_target 
#print both the equatoria target intensity 2040 and the equatoria emissions intensity INDC Target
print(equatoria_target_emissions_intensity_2040)
print(equatoria_emissions_intensity_INDC_Target)
print(colnames(ourworldindata_co2_gdp))

equatoria_target <- data.frame(
  Entity = "Equatoria",
  Code = "EQA",
  Year = 2040,
  ny_gdp_mktp_kd = equatoria_gdp_target,
  emissions_from_other_industry = NA,
  emissions_from_flaring = NA,
  emissions_from_cement = NA,
  emissions_from_gas = NA,
  emissions_from_oil = NA,
  emissions_from_coal = NA,
  population_historical = equatoria_population_2022,
  owid_region = "Global South",
  emissions_from_energy = equatoria_target_total_emissions_2040,
  oil_production= NA,
  oil_consumption = NA,
  OECD_Label = 0,
  gdp_per_capita = equatoria_gdp_target / equatoria_population_2022,
  emissions_per_capita = equatoria_target_total_emissions_2040 / equatoria_population_2022,
  label = 1
)
# Overwrite existing data for Equatoria in the ourworldindata_co2_gdp data frame
ourworldindata_co2_gdp <- ourworldindata_co2_gdp %>%
  dplyr::filter(!(Entity == "Equatoria" & Year == 2040)) %>%
  rbind(equatoria_target)

View(ourworldindata_co2_gdp[ourworldindata_co2_gdp$Entity == "Democratic Republic of Congo", ])
View(ourworldindata_co2_gdp)

# Scatter plot of our world in data co2 vs gpd
ggplot(ourworldindata_co2_gdp, aes(x = gdp_per_capita, y = emissions_per_capita, size = population_historical / 1000000, color = owid_region)) +
  geom_point(data = ourworldindata_co2_gdp, alpha = 0.5) +
  scale_size(name = "Population (M)", range = c(1, 20)) +  # Adjust the range to make the bubbles bigger for large populations
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(limits = c(min(ourworldindata_co2_gdp$emissions_per_capita * .8, na.rm = TRUE), max(ourworldindata_co2_gdp$emissions_per_capita, na.rm = TRUE) * 1.4)) +  # Add some space to the top of the y-axis
  theme_minimal() +
  labs(
       x = "GDP per Capita (USD)",
       y = "CO2 Emissions per Capita (tonnes)",
       color = "Region") +  # Change the label in the legend
  theme(legend.position = "bottom") +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
      geom_point(data = subset(ourworldindata_co2_gdp, OECD_Label == 1), 
             aes(x = gdp_per_capita, y = emissions_per_capita,, size = population_historical / 1000000), 
             shape = 21, color = "gold", fill = NA, stroke = 1.5) +
       geom_point(data = subset(ourworldindata_co2_gdp, Year == 2040), 
             aes(x = gdp_per_capita, y = emissions_per_capita, size = population_historical / 1000000), 
             color = "red", shape = 21, fill = NA, stroke = 1.5) +
  geom_text_repel(data = subset(ourworldindata_co2_gdp, Year == 2040), 
                  aes(label = "Equatoria Target"), 
                  size = 5, color = "red", 
                  point.padding = 1, box.padding = 1, nudge_y = .4) +
  geom_text_repel(
    data = subset(ourworldindata_co2_gdp, label == 1),
    aes(label = Entity),
    size = 5,
    point.padding = .5,
    box.padding = .8  # Add padding around the text labels
    #, nudge_y = 0.1  # Nudge the text labels slightly above the points
  ) 

  #guides(size = guide_legend(override.aes = list(color = NA)),
  #       color = guide_legend(override.aes = list(shape = 21, fill = NA, color = "grey"), title = "OECD Nation"))

# Save the plot
ggsave("mimic_OWIN2.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("mimic_OWIN.png"))

# Calculate Equatoria's percentile rank of GDP per capita
equatoria_gdp_pc <- ourworldindata_co2_gdp$gdp_per_capita[ourworldindata_co2_gdp$Entity == "Equatoria"]
gdp_pc_values <- ourworldindata_co2_gdp$gdp_per_capita

# Calculate the percentile rank
percentile_rank_GDP <- ecdf(gdp_pc_values)(equatoria_gdp_pc) * 100
print(paste("Equatoria's GDP per capita percentile rank is:", percentile_rank_GDP))

# Calculate Equatoria's percentile rank of emissions per capita
equatoria_emissions_pc <- ourworldindata_co2_gdp$emissions_per_capita[ourworldindata_co2_gdp$Entity == "Equatoria"]
emissions_pc_values <- ourworldindata_co2_gdp$emissions_per_capita

# Calculate the percentile rank
percentile_rank_emissions <- ecdf(emissions_pc_values)(equatoria_emissions_pc) * 100
print(paste("Equatoria's emissions per capita percentile rank is:", percentile_rank_emissions))

# Calculate Equatoria's percentile rank of emissions per capita
equatoria_population <- ourworldindata_co2_gdp$population_historical[ourworldindata_co2_gdp$Entity == "Equatoria"]
population_pc_values <- ourworldindata_co2_gdp$population_historical

# Calculate the percentile rank
percentile_rank_population <- ecdf(population_pc_values)(equatoria_population) * 100
print(paste("Equatoria's population percentile rank is:", percentile_rank_population))

# Calculate Equatoria's percentile rank of emissions from energy
equatoria_emissions_energy <- ourworldindata_co2_gdp$emissions_from_energy[ourworldindata_co2_gdp$Entity == "Equatoria"]
emissions_energy_values <- ourworldindata_co2_gdp$emissions_from_energy

# Calculate the percentile rank
percentile_rank_emissions_energy <- ecdf(emissions_energy_values)(equatoria_emissions_energy) * 100
print(paste("Equatoria's emissions from energy percentile rank is:", percentile_rank_emissions_energy))

# Calculate Equatoria's percentile rank of GDP (ny_gdp_mktp_kd)
equatoria_gdp <- ourworldindata_co2_gdp$ny_gdp_mktp_kd[ourworldindata_co2_gdp$Entity == "Equatoria"]
gdp_values <- ourworldindata_co2_gdp$ny_gdp_mktp_kd

# Calculate the percentile rank
percentile_rank_gdp <- ecdf(gdp_values)(equatoria_gdp) * 100
print(paste("Equatoria's GDP percentile rank is:", percentile_rank_gdp))

# List of countries with higher GDP than Equatoria
higher_gdp_countries <- ourworldindata_co2_gdp %>%
  dplyr::filter(ny_gdp_mktp_kd > equatoria_gdp) %>%
  dplyr::select(Entity) %>%
  distinct()

print("Countries with higher GDP than Equatoria:")
print(higher_gdp_countries)

# List of countries with higher population than Equatoria
higher_population_countries <- ourworldindata_co2_gdp %>%
  dplyr::filter(population_historical > equatoria_population) %>%
  dplyr::select(Entity) %>%
  distinct()

print("Countries with higher population than Equatoria:")
print(higher_population_countries)

# List of countries with higher emissions from energy than Equatoria
higher_emissions_countries <- ourworldindata_co2_gdp %>%
  dplyr::filter(emissions_from_energy > equatoria_emissions_energy) %>%
  dplyr::select(Entity) %>%
  distinct()

print("Countries with higher emissions from energy than Equatoria:")
print(higher_emissions_countries)

#define a list of the columns in Primary_Energy_Consumption
Primary_Energy_Consumption_columns <- colnames(Primary_Energy_Consumption)
print(Primary_Energy_Consumption_columns)
#remove column names that contain "Entity", "Code", "Year"
Primary_Energy_Consumption_columns <- Primary_Energy_Consumption_columns[!grepl("Entity|Code|Year", Primary_Energy_Consumption_columns)]
print(Primary_Energy_Consumption_columns)

# Define the order and colors for the energy sources
energy_levels <- c("Nuclear energy", "Hydro electric", "Natural Gas", "Renewables - Other", "Renewables - Bioenergy", "Renewables - Solar", "Renewables - Wind", "Oil", "Coal")
energy_colors <- c("Coal" = "black", "Oil" = "brown", "Renewables - Wind" = "green", "Renewables - Solar" = "yellow", "Renewables - Bioenergy" = "darkgreen", "Renewables - Other" = "lightgreen", "Natural Gas" = "blue", "Hydro electric" = "cyan", "Nuclear energy" = "purple",  "Renewables" = "green")

# Transform the data for Bangladesh's primary energy consumption into long format
Bangladesh_energy_long <- Primary_Energy_Consumption %>%
  dplyr::filter(Entity == "Bangladesh") %>%
  pivot_longer(cols = all_of(Primary_Energy_Consumption_columns), names_to = "variable", values_to = "value") %>%
  mutate(variable = dplyr::recode(variable,
                           "coal_consumption_twh" = "Coal",
                           "oil_consumption_twh" = "Oil",
                           "wind_consumption_equivalent_twh" = "Renewables - Wind",
                           "solar_consumption_equivalent_twh" = "Renewables - Solar",
                            "bioenergy_consumption_twh" = "Renewables - Bioenergy",
                            "other_renewables_consumption_twh" = "Renewables - Other",
                            "gas_consumption_twh" = "Natural Gas",
                           "hydro_consumption_equivalent_twh" = "Hydro electric",
                           "nuclear_consumption_equivalent_twh" = "Nuclear energy")) %>%
  mutate(variable = factor(variable, levels = energy_levels))
View(Bangladesh_energy_long)
#remove rows with varable NA
Bangladesh_energy_long <- Bangladesh_energy_long[!is.na(Bangladesh_energy_long$variable), ]
View(Bangladesh_energy_long)

# Set the factor levels for primary_energy in equatoria_energy_data
equatoria_energy_data <- equatoria_energy_data %>%
  mutate(primary_energy = factor(primary_energy, levels = c("Nuclear energy", "Hydro electric", "Natural Gas", "Renewables", "Oil", "Coal")))

# Create a two-panel GGplot

# Left-hand stacked area plot of Bangladesh's primary energy consumption over time
Bangladesh_energy_plot <- ggplot(Bangladesh_energy_long, aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack", alpha = 0.6, linewidth = 0.5, colour = "white") +
  scale_fill_manual(values = energy_colors, breaks = energy_levels) +
  theme_minimal() +
  labs(title = "Bangladesh",
    x = "Year",
    y = "Energy Consumption (TWh)",
    fill = "Primary Source") +
  theme(
  plot.title = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.position = "bottom"
  ) 

# Right-hand vertical stacked bar chart of Equatoria's energy data
equatoria_energy_plot <- ggplot(equatoria_energy_data, aes(x = "", y = Primary_energy_amount * 100, fill = primary_energy)) +
  geom_bar(stat = "identity", position = "stack", width = 0.2) +
  scale_fill_manual(values = energy_colors) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  theme_minimal() +
  labs(title = "Equatoria",
    x = NULL,
     y = "Primary Energy Share (%)",
     fill = "Primary Source") +
  theme(
  plot.title = element_text(size = 20),
  axis.title.x = element_text(size = 15),
  axis.title.y = element_text(size = 15),
  legend.position = "none"
  )

# Combine the two plots into a two-panel plot using patchwork

combined_plot <- Bangladesh_energy_plot + equatoria_energy_plot + plot_layout(ncol = 2, widths = c(3, 1))
plot(combined_plot)
# Save the combined plot
ggsave("combined_energy_plot.png", plot = combined_plot, width = 16, height = 8, units = "in", dpi = 300)


# Plot Bangladesh's and Equatoria's emissions over time
plot_emissions_over_time <- function(data, countries) {
  data_filtered <- data %>%
    dplyr::filter(Entity %in% countries) %>%
    mutate(Entity = factor(Entity, levels = countries))
  
  ggplot(data_filtered, aes(x = Year, y = emissions_from_energy, color = Entity)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Emissions from Energy Over Time",
         x = "Year",
         y = "Emissions from Energy (tonnes)",
         color = "Country") +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    )
}

# Example usage
countries_to_plot <- c("Bangladesh", "Equatoria")
plot_emissions_over_time(merged_data, countries_to_plot)
plot(last_plot())
# Save the plot
ggsave("Bangladesh_equatoria_emissions_over_time.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("Bangladesh_equatoria_emissions_over_time.png"))

##################################################################
######### Natural Resource's Contribution to contry's economy ####
##################################################################
# Perform a Phillips-Perron (PP) unit root test on the data set
pp.test(GDP_pc)
pp.test(Table_1$emissions_per_capita)

##scatter plot using ggplot with custom dot colors x and y label that are user defined, and custom width and height
gross_scatter <- ggplot(Table_1, aes(x = Emissions_CO2_Energy_mt, y = GDP_Real_USD_billions)) +
  geom_point() +
  theme_minimal() +
  labs(title = "CO2 Emissions vs GDP",
       x = "CO2 Emissions (mt)",
       y = "GDP (USD billions)") +
  theme(legend.position = "bottom") +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  )

#add regression line with 95% confidence interval to the scatter plot
gross_scatter <- gross_scatter + geom_smooth(method = "lm", se = TRUE, color = "red")

# Function to plot change in per capita emissions and per capita GDP over time
plot_emissions_gdp_over_time <- function(data, countries, start_year) {
  data_filtered <- data %>% 
    dplyr::filter(Entity %in% countries, Year >= start_year) %>%
    mutate(gdp_per_capita = ny_gdp_mktp_kd / population_historical,
           emissions_per_capita = emissions_from_energy / population_historical) %>%
    group_by(Entity) %>%
    mutate(
      gdp_per_capita_change = (gdp_per_capita / first(gdp_per_capita) - 1) * 100,
      emissions_per_capita_change = (emissions_per_capita / first(emissions_per_capita) - 1) * 100
    ) %>%
    ungroup() %>%
    mutate(Entity = factor(Entity, levels = countries))  # Force the sequence of plots
  
  ggplot(data_filtered, aes(x = Year)) +
    geom_line(aes(y = gdp_per_capita_change, color = "GDP per Capita Change (%)")) +
    geom_line(aes(y = emissions_per_capita_change, color = "CO2 Emissions per Capita Change (%)")) +
    facet_wrap(~ Entity, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Percent Change in Per Capita Emissions and GDP Over Time (from", start_year, ")"),
         x = "Year",
         y = "Percent Change",
         color = "Metric") +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    )
}

# New function to plot change in total emissions and total GDP over time
plot_total_emissions_gdp_over_time <- function(data, countries, start_year) {
  data_filtered <- data %>% 
    dplyr::filter(Entity %in% countries, Year >= start_year) %>%
    mutate(Entity = factor(Entity, levels = countries)) %>%
    group_by(Entity) %>%
    mutate(
      gdp_change = (ny_gdp_mktp_kd / first(ny_gdp_mktp_kd) - 1) * 100,
      emissions_change = (emissions_from_energy / first(emissions_from_energy) - 1) * 100
    ) %>%
    ungroup()
  
  ggplot(data_filtered, aes(x = Year)) +
    geom_line(aes(y = gdp_change, color = "GDP Change (%)")) +
    geom_line(aes(y = emissions_change, color = "CO2 Emissions Change (%)")) +
    facet_wrap(~ Entity, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Percent Change in Total Emissions and GDP Over Time (from", start_year, ")"),
         x = "Year",
         y = "Percent Change",
         color = "Metric") +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    )
}

# New function to plot change in total GDP over time
plot_gdp_over_time <- function(data, countries, start_year) {
  data_filtered <- data %>% 
    dplyr::filter(Entity %in% countries, Year >= start_year) %>%
    mutate(Entity = factor(Entity, levels = countries)) %>%
    group_by(Entity) %>%
    mutate(
      gdp_change = (ny_gdp_mktp_kd / first(ny_gdp_mktp_kd) - 1) * 100
    ) %>%
    ungroup()
  
  ggplot(data_filtered, aes(x = Year)) +
    geom_line(aes(y = gdp_change, color = "GDP Change (%)")) +
    facet_wrap(~ Entity, scales = "free_y") +
    theme_minimal() +
    labs(title = paste("Percent Change in GDP Over Time (from", start_year, ")"),
         x = "Year",
         y = "Percent Change",
         color = "Metric") +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    )
}


# Example usage
countries_to_plot <- c("Equatoria", "Bangladesh", "Kenya", "Venezuela", "Russia", "Nigeria", "Brazil")
start_year <- 1970
plot_gdp_over_time(merged_data, countries_to_plot, start_year)
plot(last_plot())
# Save the plot
ggsave("gdp_over_time.png", plot = last_plot(), width = 16, height = 10, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("gdp_over_time.png"))

# Example usage
countries_to_plot <- c("Equatoria", "Bangladesh", "Kenya", "Venezuela", "Russia", "Nigeria", "Brazil")
start_year <- 1970
plot_total_emissions_gdp_over_time(merged_data, countries_to_plot, start_year)
plot(last_plot())
# Save the plot
ggsave("total_emissions_gdp_over_time.png", plot = last_plot(), width = 16, height = 10, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("total_emissions_gdp_over_time.png"))

# Example usage
countries_to_plot <- c("Equatoria", "Bangladesh", "Pakistan", "Nigeria", "Mexico", "Iran")
countries_to_plot <- c("United Kingdom", "Norway", "Finland", "Australia", "France", "Germany")
start_year <- 1970
plot_emissions_gdp_over_time(merged_data, countries_to_plot, start_year)
plot(last_plot())
# Save the plot
ggsave("high_income_emissions_gdp_over_time.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("emissions_gdp_over_time.png"))

countries_to_plot <- c("Equatoria", "Iran", "Nigeria")
plot_emissions_gdp_over_time(merged_data, countries_to_plot, start_year)
ggsave("rc_emissions_gdp_over_time.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)




################################################################
# Calculate 10-year rolling averages of GDP from 1980 to 2022 for each country
merged_data_2 <- merged_data %>%
  dplyr::filter(Year >= 1970 & Year <= 2022) %>%
  group_by(Entity) %>%
  arrange(Year) %>%
  mutate(rolling_avg_gdp = ifelse(Year >= 1980, zoo::rollapply(ny_gdp_mktp_kd, width = 10, FUN = mean, align = "right", fill = NA), NA)) %>%
  ungroup()
# View the merged_data_2 data frame for year 2022
View(merged_data_2[merged_data_2$Year == 2022, ])

# Calculate the change in rolling average GDP over the period from 1980 to 2022
gdp_changes <- merged_data_2 %>%
  group_by(Entity) %>%
  arrange(Year) %>%
  mutate(change_in_rolling_avg_gdp = rolling_avg_gdp - lag(rolling_avg_gdp, order_by = Year)) %>%
  ungroup()

# Display a list of the largest negative changes by country (including Equatoria) over the period
largest_negative_changes <- gdp_changes %>%
  dplyr::filter(!is.na(change_in_rolling_avg_gdp) & change_in_rolling_avg_gdp < 0) %>%
  arrange(change_in_rolling_avg_gdp)

largest_negative_changes <- gdp_changes %>%
  dplyr::filter(change_in_rolling_avg_gdp < 0) %>%
  arrange(change_in_rolling_avg_gdp)
View(largest_negative_changes)

#for countries that have a negative change in rolling average GDP, count the number of years they had a negative change and rank countries by their count
negative_change_count <- gdp_changes %>%
  dplyr::filter(change_in_rolling_avg_gdp < 0) %>%
  group_by(Entity) %>%
  arrange(Year) %>%
  mutate(consecutive_negative = cumsum(c(1, diff(Year) != 1))) %>%
  group_by(Entity, consecutive_negative) %>%
  summarise(consecutive_count = n()) %>%
  filter(consecutive_count > 1) %>%
  group_by(Entity) %>%
  summarise(max_consecutive_negative_years = max(consecutive_count)) %>%
  left_join(merged_data %>% dplyr::filter(Year == 2022) %>% dplyr::select(Entity, ny_gdp_mktp_kd), by = "Entity") %>%
  arrange(desc(ny_gdp_mktp_kd))
View(negative_change_count)

# Filter countries with more than 3 consecutive years of negative GDP growth and plot the top 12 by ny_gdp_mktp_kd
top_12_negative_change_count <- negative_change_count %>%
  dplyr::filter(max_consecutive_negative_years > 3) %>%
  top_n(12, ny_gdp_mktp_kd) %>%
  pull(Entity)

View(top_12_negative_change_count)
plot_gdp_over_time(merged_data, top_12_negative_change_count, start_year)

#do a similar plot to plot_gdp_over_time but instead of doing the percent change, plot the total GDP values for each country
plot_total_gdp_over_time <- function(data, countries, start_year) {
  data_filtered <- data %>%
    dplyr::filter(Entity %in% countries, Year >= start_year) %>%
    mutate(Entity = factor(Entity, levels = countries)) %>%
    group_by(Entity) %>%
    arrange(Year) %>%
    mutate(rolling_avg_gdp = ifelse(
      Year >= 1980, 
      zoo::rollapply(ny_gdp_mktp_kd, width = 10, FUN = mean, align = "right", fill = NA), 
      NA
    )) %>%
    mutate(change_in_rolling_avg_gdp = rolling_avg_gdp - lag(rolling_avg_gdp, order_by = Year)) %>%
    ungroup()
  
  ggplot(data_filtered, aes(x = Year)) +
    geom_line(aes(y = ny_gdp_mktp_kd / 1e9, color = "Total GDP"), size = 1) +
    geom_line(aes(y = rolling_avg_gdp / 1e9, color = "10-yr Rolling Avg GDP"), linetype = "dashed", size = 1) +
    geom_bar(aes(y = change_in_rolling_avg_gdp / max(abs(change_in_rolling_avg_gdp), na.rm = TRUE) * max(ny_gdp_mktp_kd / 1e9, na.rm = TRUE), fill = "Change in Rolling Avg GDP"), stat = "identity", alpha = 0.5) +
    facet_wrap(~ Entity, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("Total GDP and Change in Rolling Average GDP Over Time (from", start_year, ")"),
      x = "Year",
      y = "Total GDP (Billion USD)",
      color = "Metric",
      fill = "Metric"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . * max(abs(data_filtered$change_in_rolling_avg_gdp), na.rm = TRUE) / max(data_filtered$ny_gdp_mktp_kd / 1e9, na.rm = TRUE) / 1e9, 
        name = "Change in 10-yr Rolling Average GDP (Billion USD)", 
        labels = scales::comma
      )
    )
}

plot_total_gdp_over_time(merged_data, c("Equatoria", "Bangladesh"), 1970)
plot(last_plot())
# Save the plot
ggsave("Bangladesh_Equatoria_GDP.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Example usage
plot_total_gdp_over_time(merged_data, top_12_negative_change_count, start_year)

# Save the plot
ggsave("top_12_negative_change_count_GDP.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Load necessary libraries for ARDL and Granger causality analysis
library(urca)
library(dynlm)
library(lmtest)
library(strucchange)
library(tseries)
library(ARDL)

# Prepare data for Equatoria
equatoria_data <- merged_data %>%
  dplyr::filter(Entity == "Equatoria") %>%
  dplyr::select(Year, ny_gdp_mktp_kd, emissions_from_energy, population_historical) %>%
  inner_join(Table_2, by = c("Year" = "StudyYear")) %>%
  mutate(
    gdp_per_capita = ny_gdp_mktp_kd / population_historical,
    emissions_per_capita = emissions_from_energy / population_historical,
    lnGDP = log(ny_gdp_mktp_kd),
    lnEM = log(emissions_from_energy),
    lnOil = log(Oil_Production_mmbd),
    lnP = log(population_historical),
    lnGDPpc = log(gdp_per_capita),
    lnEMpc = log(emissions_per_capita)
  )

print(colnames(equatoria_data))

# Scatter plot with fit line for GDP vs Emissions per capita
emissions <- ggplot(equatoria_data, aes(x = emissions_per_capita, y = gdp_per_capita)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "GDP vs Emissions per Capita",
    x = "Emissions per Capita (tonnes)",
    y = "GDP per Capita (USD)",
    color = "Legend"
  ) +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("Adj R2 = ", round(summary(lm(gdp_per_capita ~ emissions_per_capita, data = equatoria_data))$adj.r.squared, 2),
                                                   "\nIntercept =", round(summary(lm(gdp_per_capita ~ emissions_per_capita, data = equatoria_data))$coefficients[1, 1], 2),
                                                   "\nSlope =", round(summary(lm(gdp_per_capita ~ emissions_per_capita, data = equatoria_data))$coefficients[2, 1], 2),
                                                   "\nP =", formatC(summary(lm(gdp_per_capita ~ emissions_per_capita, data = equatoria_data))$coefficients[2, 4], format = "e", digits = 2)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "blue")
plot(emissions)

# Fit a linear regression model
lm_model <- lm(gdp_per_capita ~ emissions_per_capita, data = equatoria_data)

# Print the summary of the model
summary(lm_model)

oil_production <- ggplot(equatoria_data, aes(x = Oil_Production_mmbd, y = gdp_per_capita)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "GDP vs Oil Production",
    x = "Oil Production (mmbd)",
    y = "GDP per Capita (USD)",
    color = "Legend"
  ) +
  theme(
    plot.title = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom"
  ) +
  annotate("text", x = Inf, y = Inf, label = paste("Adj R2 = ", round(summary(lm(gdp_per_capita ~ Oil_Production_mmbd, data = equatoria_data))$adj.r.squared, 2),
                                                   "\nIntercept =", round(summary(lm(gdp_per_capita ~ Oil_Production_mmbd, data = equatoria_data))$coefficients[1, 1], 2),
                                                   "\nSlope =", round(summary(lm(gdp_per_capita ~ Oil_Production_mmbd, data = equatoria_data))$coefficients[2, 1], 2),
                                                   "\nP =", formatC(summary(lm(gdp_per_capita ~ Oil_Production_mmbd, data = equatoria_data))$coefficients[2, 4], format = "e", digits = 2)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "blue")
plot(oil_production)

# Create a two-panel plot for oil production and emissions

# Combine the two plots into a side-by-side plot using patchwork
combined_oil_emissions_plot <- emissions + oil_production + plot_layout(ncol = 2, widths = c(1, 1))

# Display the combined plot
plot(combined_oil_emissions_plot)

# Save the combined plot
ggsave("combined_oil_emissions_plot.png", plot = combined_oil_emissions_plot, width = 10, height = 5, units = "in", dpi = 600)

# Display the plot in vscode window
grid.raster(readPNG("combined_oil_emissions_plot.png"))

# Calculate annual net import oil dependency using Table_2
Table_2 <- Table_2 %>%
  mutate(
    net_imports = Oil_Consumption_mmbd - Oil_Production_mmbd,
    net_import_oil_dependency = net_imports / Oil_Consumption_mmbd
  )

# Print the calculated net import oil dependency
print(Table_2$net_import_oil_dependency)

# View the data to verify calculations
View(Table_2)

  # Fit a linear model using least squares
  lm_model <- lm(lnGDP ~ lnEM + lnOil + lnP, data = equatoria_data)
  lm_model_2 <- lm(lnGDPpc ~ lnOil, data = equatoria_data)
  summary(lm_model_2)

  # Check for multicollinearity using Variance Inflation Factor (VIF)
  vif(lm_model)

  # Perform Granger causality tests
  granger_test_lnEM <- grangertest(lnGDP ~ lnEM, order = 2, data = equatoria_data)
  summary(granger_test_lnEM)

  granger_test_lnOil <- grangertest(lnGDP ~ lnOil, order = 2, data = equatoria_data)
  summary(granger_test_lnOil)

  granger_test_lnP <- grangertest(lnGDP ~ lnP, order = 2, data = equatoria_data)
  summary(granger_test_lnP)

  # Define a function to plot GDP vs Oil Production for multiple countries
  plot_gdp_corr <- function(data, countries) {
    data_filtered <- data %>%
      dplyr::filter(Entity %in% countries) %>%
      mutate(Entity = factor(Entity, levels = countries))
    
    ggplot(data_filtered, aes(x = Oil_Production_mmbd, y = gdp_per_capita)) +
      geom_point(aes(color = "Data Points")) +
      geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
      facet_wrap(~ Entity, scales = "free") +
      theme_minimal() +
      labs(title = "GDP vs Oil Production",
           x = "Oil Production (mmbd)",
           y = "Per capita GDP",
           color = "Legend") +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
      )
  }

  # Example usage
  countries_to_plot <- c("Equatoria")
  plot_gdp_corr(equatoria_data, countries_to_plot)
  plot(last_plot())

  # Save the plot
  ggsave("gdp_vs_oil.png", plot = last_plot(), width = 16, height = 10, units = "in", dpi = 300)

    # Display the plot in vscode window
    grid.raster(readPNG("gdp_vs_emissions.png"))

# Check for stationarity using ADF test
adf_gdp <- ur.df(equatoria_data$gdp_per_capita, type = "trend", lags = 1)
summary(adf_gdp)

adf_emissions <- ur.df(equatoria_data$emissions_per_capita, type = "trend", lags = 1)
summary(adf_emissions)

# Identify structural breaks using Bai-Perron test
bp_gdp <- breakpoints(gdp_per_capita ~ 1, data = equatoria_data)
summary(bp_gdp)

bp_emissions <- breakpoints(emissions_per_capita ~ 1, data = equatoria_data)
summary(bp_emissions)

# Create dummy variables for breakpoints
breakpoints_gdp <- breakpoints(gdp_per_capita ~ 1, data = equatoria_data, h = 3)
summary(breakpoints_gdp)
equatoria_data <- equatoria_data %>%
  mutate(breakpoint_dummy1 = ifelse(Year == breakpoints_gdp$breakpoints[1], 1, 0),
         breakpoint_dummy2 = ifelse(Year == breakpoints_gdp$breakpoints[2], 1, 0),
         breakpoint_dummy3 = ifelse(Year == breakpoints_gdp$breakpoints[3], 1, 0))

# Scatter plot of GDP over time with fit lines through the 3 breakpoints
ggplot(equatoria_data, aes(x = Year, y = gdp_per_capita)) +
  geom_point() +
  # geom_vline(xintercept = breakpoints_gdp, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = subset(equatoria_data, Year <= breakpoints_gdp$breakpoints[1]), 
              method = "lm", se = FALSE, color = "green") +
  geom_smooth(data = subset(equatoria_data, Year > breakpoints_gdp$breakpoints[1] & 
                            Year <= breakpoints_gdp$breakpoints[2]), 
              method = "lm", se = FALSE, color = "purple") +
  geom_smooth(data = subset(equatoria_data, Year > breakpoints_gdp$breakpoints[2]), 
              method = "lm", se = FALSE, color = "orange") +
  theme_minimal() +
  labs(title = "GDP per Capita Over Time with Breakpoints",
       x = "Year",
       y = "GDP per Capita (USD)") +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  )

# Fit ARDL model manually using dynlm with breakpoints as dummy variables
ardl_model <- dynlm(gdp_per_capita ~ L(gdp_per_capita, 1) + L(emissions_per_capita, 1) + breakpoint_dummy, data = equatoria_data) # nolint
summary(ardl_model)

# Perform bounds testing manually
residuals <- residuals(ardl_model)
adf_residuals <- ur.df(residuals, type = "none", lags = 1)
summary(adf_residuals)

# Granger causality test
granger_test <- grangertest(gdp_per_capita ~ emissions_per_capita, order = 2, data = equatoria_data)
summary(granger_test)

granger_test_reverse <- grangertest(emissions_per_capita ~ gdp_per_capita, order = 2, data = equatoria_data)
summary(granger_test_reverse)

##############################################################################
##############################################################################
############## Kuznet Curve for Equatoria  ###################################
##############################################################################
##############################################################################
##############################################################################

# Plot Equatoria's emissions per capita vs GDP per capita with year as color gradient
equatoria_data <- merged_data %>%
  dplyr::filter(Entity == "Equatoria") %>%
  mutate(year_color = scales::col_numeric("Reds", domain = range(Year))(Year))

ggplot(equatoria_data, aes(x = gdp_per_capita, y = emissions_per_capita)) +
  geom_point(aes(color = year_color), size = 3) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = "Equatoria's Emissions per Capita vs GDP per Capita",
    x = "GDP per Capita (USD)",
    y = "Emissions per Capita (tonnes)",
    color = "Year"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  geom_text_repel(aes(label = Year), size = 3, color = "black", point.padding = 0.5, box.padding = 0.5)
  # Function to plot emissions per capita vs GDP per capita for multiple countries
  plot_kuznets_curve <- function(data, countries) {
    data_filtered <- data %>%
      dplyr::filter(Entity %in% countries) %>%
      mutate(year_color = scales::col_numeric("Reds", domain = range(Year))(Year),
             Entity = factor(Entity, levels = countries))

#save the plot
ggsave("equatoria_emissions_gdp.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

    ggplot(data_filtered, aes(x = gdp_per_capita, y = emissions_per_capita)) +
      geom_point(aes(color = year_color), size = 3) +
      scale_color_identity() +
      theme_minimal() +
      labs(
        title = "Emissions per Capita vs GDP per Capita",
        x = "GDP per Capita (USD)",
        y = "Emissions per Capita (tonnes)",
        color = "Year"
      ) +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none"
      ) +
      geom_text_repel(aes(label = Year), size = 3, color = "black", point.padding = 0.5, box.padding = 0.5) +
      facet_wrap(~ Entity, scales = "free")
  }

  # Example usage
  countries_to_plot <- c("Equatoria", "Australia")
  plot_kuznets_curve(merged_data, countries_to_plot)
  plot(last_plot())
#save the plot
ggsave("kuznets_curve.png", plot = last_plot(), width = 10, height = 4, units = "in", dpi = 300)

ggplot(equatoria_data, aes(x = ny_gdp_mktp_kd, y = emissions_from_energy)) +
  geom_point(aes(color = year_color), size = 3) +
  scale_color_identity() +
  theme_minimal() +
  labs(
    title = "Equatoria's Emissions per Capita vs GDP per Capita",
    x = "GDP per Capita (USD)",
    y = "Emissions per Capita (tonnes)",
    color = "Year"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) +
  geom_text_repel(aes(label = Year), size = 3, color = "black", point.padding = 0.5, box.padding = 0.5)



##############################################################################
##############################################################################
############## Oil Dependency  ###############################################
##############################################################################
##############################################################################
##############################################################################

#combine tables 1 and 2 by an inner join on the year column
combined_data <- merge(Table_1, Table_2, by = "StudyYear", all = FALSE)

# Add the Entity column to combined_data
combined_data$Entity <- "Equatoria"

# Calculate annual net import oil dependency using Table_2
combined_data <- combined_data %>%
  mutate(
    net_imports = Oil_Consumption_mmbd - Oil_Production_mmbd,
    net_import_oil_dependency = net_imports / Oil_Consumption_mmbd
  )

# Print the calculated net import oil dependency
print(combined_data$net_import_oil_dependency)

# View the data to verify calculations
View(combined_data)

# Calculate the value of net oil imports for Equatoria by multiplying the net imports by the oil price
combined_data$net_import_cost <- combined_data$net_imports * 1000000 * 365 * combined_data$Oil_Price

# Calculate resource dependency as total oil consumption times oil price as a percentage of GDP
combined_data <- combined_data %>%
  mutate(
    resource_dependency = (Oil_Consumption_mmbd * 1000000 * 365 * Oil_Price_USDpB) / (GDP_Real_USD_billions * 1e9) * 100,
    total_oil_cost = Oil_Consumption_mmbd * 1000000 * 365 * Oil_Price_USDpB / 1e9  # Total oil cost in billion USD
  )

# Print the calculated resource dependency and total oil cost
print(combined_data$resource_dependency)
print(combined_data$total_oil_cost)

# View the data to verify calculations
View(combined_data)

# Manually set the values for the different axes
primary_y_limits <- c(0, 100)
primary_y_breaks <- seq(0, 100, by = 20)
secondary_y_limits <- c(0, max(combined_data$GDP_Real_USD_billions / combined_data$Population_mm) * 1000)
secondary_y_breaks <- seq(0, max(combined_data$GDP_Real_USD_billions / combined_data$Population_mm) * 1000, by = 2000)

#plot net import oil dependency over time using scale_y_continuous
ggplot(combined_data, aes(x = StudyYear)) +
  geom_line(aes(y = net_import_oil_dependency * 100, color = "Net Import Oil Dependency"), size = 1) +
  geom_line(aes(y = (GDP_Real_USD_billions / Population_mm) * 1000, color = "GDP per Capita"), size = 1) +
  scale_y_continuous(
    name = "Net Import Oil Dependency (%)",
    limits = primary_y_limits,
    breaks = primary_y_breaks,
    sec.axis = sec_axis(~ . / 1000 * max(combined_data$GDP_Real_USD_billions / combined_data$Population_mm), 
                        name = "GDP per Capita (USD)",
                        breaks = secondary_y_breaks)
  ) +
  theme_minimal() +
  labs(
    title = "Net Import Oil Dependency and GDP per Capita Over Time",
    x = "Year",
    color = "Metric"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )

#save the plot
ggsave("net_import_oil_dependency_gdp_per_capita.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

#add a plot like plot_total_gdp_over_time that plots the net oil import dependency as a line and net import cost as a bar
plot_net_import_dependency_net_import_cost_over_time <- function(data, start_year) {
  data_filtered <- data %>%
    dplyr::filter(StudyYear >= start_year) %>%
    arrange(StudyYear)
  
  # Calculate scaling factor for secondary y-axis
  scaling_factor <- 100 / max(data_filtered$net_import_cost / 1e9)
  
  ggplot(data_filtered, aes(x = StudyYear)) +
    geom_line(aes(y = net_import_oil_dependency * 100, color = "Net Import Oil Dependency (%)"), size = 1) +
    geom_bar(aes(y = net_import_cost / 1e9 * scaling_factor, fill = "Net Import Cost"), stat = "identity", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = paste("Net Import Oil Dependency (%) and Net Import Cost Over Time (from", start_year, ")"),
      x = "Year",
      y = "Net Import Oil Dependency (%)",
      color = "Metric",
      fill = "Metric"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      sec.axis = sec_axis(
        ~ . / scaling_factor, 
        name = "Net Import Cost (Billion USD)",
        labels = scales::comma
      )
    )
}

# Example usage
plot_net_import_dependency_net_import_cost_over_time(combined_data, 1970)
plot(last_plot())

# Save the plot
ggsave("net_import_dependency_net_import_cost_over_time.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("total_gdp_net_import_cost_over_time.png"))

# Add a new plot that graphs the annual change in GDP compared to the price of oil
plot_gdp_change_oil_price_over_time <- function(data, start_year) {
  data_filtered <- data %>%
    dplyr::filter(StudyYear >= start_year) %>%
    arrange(StudyYear) %>%
    mutate(gdp_change = (GDP_Real_USD_billions / lag(GDP_Real_USD_billions) - 1) * 100)
  
  # Calculate scaling factor for secondary y-axis
  scaling_factor <- max(data_filtered$gdp_change, na.rm = TRUE) / max(data_filtered$Oil_Price, na.rm = TRUE)
  
  ggplot(data_filtered, aes(x = StudyYear)) +
    geom_line(aes(y = gdp_change, color = "GDP Change (%)"), size = 1) +
    geom_line(aes(y = Oil_Price_USDpB * scaling_factor, color = "Oil Price (scaled)"), size = 1, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = paste("Annual GDP Change (%) and Oil Price Over Time (from", start_year, ")"),
      x = "Year",
      y = "GDP Change (%)",
      color = "Metric"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . / scaling_factor, 
        name = "Oil Price (USD)",
        labels = scales::comma
      )
    )
}

# Example usage
plot_gdp_change_oil_price_over_time(combined_data, 1970)
plot(last_plot())

# Save the plot
ggsave("gdp_change_oil_price_over_time.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("gdp_change_oil_price_over_time.png"))

# Add a new plot with oil price on the x-axis and annual change in GDP as a percentage on the y-axis, along with a trendline
plot_oil_price_vs_gdp_change <- function(data) {
  data_filtered <- data %>%
    arrange(StudyYear) %>%
    mutate(gdp_change = (GDP_Real_USD_billions / lag(GDP_Real_USD_billions) - 1) * 100)
  
  ggplot(data_filtered, aes(x = Oil_Price_USDpB, y = gdp_change)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    theme_minimal() +
    labs(
      title = "Oil Price vs Annual GDP Change (%)",
      x = "Oil Price (USD)",
      y = "Annual GDP Change (%)"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15)
    )
}

# Example usage
plot_oil_price_vs_gdp_change(combined_data)
plot(last_plot())

# Save the plot
ggsave("oil_price_vs_gdp_change.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

#add a columns that calculated year over year GDP change and year over year oil price change
combined_data <- combined_data %>%
  arrange(StudyYear) %>%
  mutate(
    gdp_change = (GDP_Real_USD_billions - lag(GDP_Real_USD_billions) - 1) * 100,
    oil_price_change = (Oil_Price_USDpB - lag(Oil_Price_USDpB) - 1),
    consumption_change = (Oil_Consumption_mmbd - lag(Oil_Consumption_mmbd) - 1)
  )

summary(lm(gdp_change ~ oil_price_change, data = combined_data))
 #plott the relationship between oil price change and GDP change
# Perform linear regression
regression_model <- lm(gdp_change ~ oil_price_change, data = combined_data)
regression_summary <- summary(regression_model)

# Create the plot
price_gdp <- ggplot(combined_data, aes(x = oil_price_change, y = gdp_change)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Oil Price Change vs GDP Change",
    x = "Oil Price Change ($/bbl)",
    y = "GDP Change (USD Billion)"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Data Points" = "black", "Fit Line" = "red")) +
  guides(color = guide_legend(title = "Legend")) +
  annotate("text", x = Inf, y = Inf, label = paste("Adj R2 = ", round(regression_summary$adj.r.squared, 2),
                                                   "\nIntercept =", round(regression_summary$coefficients[1, 1], 2),
                                                   "\nSlope =", round(regression_summary$coefficients[2, 1], 2),
                                                   "\nP =", round(regression_summary$coefficients[2, 4], 4)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "blue")

plot(price_gdp)
#save the plot
ggsave("oil_price_change_vs_gdp_change.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

summary(lm(consumption_change ~ oil_price_change, data = combined_data))
 #plott the relationship between oil price change and GDP change
# Perform linear regression
regression_model_cons <- lm(consumption_change ~ oil_price_change, data = combined_data)
regression_summary_cons <- summary(regression_model_cons)

price_cons <- ggplot(combined_data, aes(x = oil_price_change, y = consumption_change)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Oil Price Change vs Consumption Change",
    x = "Oil Price Change ($/bbl)",
    y = "Consumption Change (mmbd)"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Data Points" = "black", "Fit Line" = "red")) +
  guides(color = guide_legend(title = "Legend")) +
  annotate("text", x = Inf, y = Inf, label = paste("Adj R2 = ", round(regression_summary_cons$adj.r.squared, 2),
                                                   "\nIntercept =", round(regression_summary_cons$coefficients[1, 1], 2),
                                                   "\nSlope =", round(regression_summary_cons$coefficients[2, 1], 2),
                                                   "\nP =", round(regression_summary_cons$coefficients[2, 4], 4)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "blue")

plot(price_cons)
#save the plot
ggsave("oil_price_change_vs_gdp_change.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

# Combine the two plots into a side-by-side plot using patchwork
combined_price_plot <- price_gdp + price_cons + plot_layout(ncol = 2, widths = c(1, 1))

# Display the combined plot
plot(combined_price_plot)

# Save the combined plot
ggsave("combined_price_plot.png", plot = combined_price_plot, width = 16, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("combined_price_plot.png"))

# Add a stacked area plot for Equatoria with annual total GDP on the bottom and annual net import cost on top
plot_stacked_gdp_net_import_cost <- function(data, start_year) {
  data_filtered <- data %>%
    dplyr::filter(StudyYear >= start_year) %>%
    arrange(StudyYear) %>%
    mutate(
      net_import_cost = net_imports * 1000000 * 365 * Oil_Price_USDpB,
      total_cost = GDP_Real_USD_billions + net_import_cost / 1e9
    )
  
  ggplot(data_filtered, aes(x = StudyYear)) +
    geom_area(aes(y = total_cost, fill = "Net Import Cost"), alpha = 0.6) +
    geom_area(aes(y = GDP_Real_USD_billions, fill = "Total GDP"), alpha = 0.6) +
    theme_minimal() +
    labs(
      title = paste("Stacked Area Plot of Total GDP and Net Import Cost (from", start_year, ")"),
      x = "Year",
      y = "Cost (Billion USD)",
      fill = "Metric"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    )
}

# Example usage
plot_stacked_gdp_net_import_cost(combined_data, 1970)
plot(last_plot())

# Save the plot
ggsave("stacked_gdp_net_import_cost.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("stacked_gdp_net_import_cost.png"))

# Add a plot of resource dependency as a percent of GDP and total oil cost
plot_resource_dependency_total_oil_cost_over_time <- function(data, start_year) {
  data_filtered <- data %>%
    dplyr::filter(StudyYear >= start_year) %>%
    arrange(StudyYear)
  
  # Calculate scaling factor for secondary y-axis
  scaling_factor <- 12.5 / max(data_filtered$total_oil_cost)
  
  ggplot(data_filtered, aes(x = StudyYear)) +
    geom_line(aes(y = resource_dependency, color = "Resource Dependency (%)"), size = 1) +
    geom_bar(aes(y = total_oil_cost * scaling_factor, fill = "Total Oil Cost"), stat = "identity", alpha = 0.5) +
    geom_line(aes(y = Oil_Price_USDpB * scaling_factor, color = "Oil Price (USD)"), size = 1, linetype = "solid") +
    theme_minimal() +
    labs(
      title = paste("Resource Dependency (%) and Total Oil Cost Over Time (from", start_year, ")"),
      x = "Year",
      y = "Resource Dependency (%)",
      color = "Metric",
      fill = "Metric"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    ) +
    scale_y_continuous(
      limits = c(0, 12.5),
      sec.axis = sec_axis(
        ~ . / scaling_factor, 
        name = "Total Oil Cost (Billion USD) / Oil Price (USD)",
        labels = scales::comma
      )
    ) +
    scale_color_manual(values = c("Resource Dependency (%)" = "black", "Oil Price (USD)" = "blue"))
}

# Example usage
plot_resource_dependency_total_oil_cost_over_time(combined_data, 1970)
plot(last_plot())

plot_res_dep_oil_cost(combined_data, 1970)
plot(last_plot())
# Save the plot
ggsave("resource_dependency_total_oil_cost_over_time.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("resource_dependency_total_oil_cost_over_time.png"))

# Add a plot of oil price as a line and resource dependency as bar charts over time
plot_oil_price_resource_dependency_over_time <- function(data, start_year) {
  data_filtered <- data %>%
    dplyr::filter(StudyYear >= start_year) %>%
    arrange(StudyYear)
  
  # Calculate scaling factor for secondary y-axis
  scaling_factor <- max(data_filtered$total_oil_cost) / max(data_filtered$resource_dependency)
  
  ggplot(data_filtered, aes(x = StudyYear)) +
    geom_line(aes(y = resource_dependency, color = "Resource Dependency (%)"), size = 1) +
    geom_bar(aes(y = total_oil_cost / scaling_factor, fill = "Total Oil Cost (Billion USD)"), stat = "identity", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = paste("Resource Dependency (%) and Total Oil Cost (Billion USD) Over Time (from", start_year, ")"),
      x = "Year",
      y = "Resource Dependency (%)",
      color = "Metric",
      fill = "Metric"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . * scaling_factor, 
        name = "Total Oil Cost (Billion USD)",
        labels = scales::comma
      )
    )
}

# Example usage
plot_oil_price_resource_dependency_over_time(combined_data, 1970)
plot(last_plot())

# Save the plot
ggsave("oil_price_resource_dependency_over_time.png", plot = last_plot(), width = 12, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("oil_price_resource_dependency_over_time.png"))

# Combine the two plots into a two-panel plot using patchwork
combined_resource_dependency_plot <- plot_resource_dependency_total_oil_cost_over_time(combined_data, 1970) + 
  plot_oil_price_resource_dependency_over_time(combined_data, 1970) + 
  plot_layout(ncol = 2, widths = c(1, 1))

# Display the combined plot
plot(combined_resource_dependency_plot)

# Save the combined plot
ggsave("combined_resource_dependency_plot.png", plot = combined_resource_dependency_plot, width = 16, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("combined_resource_dependency_plot.png"))

##############################################################################
##############################################################################
############## Transportation  ###############################################
##############################################################################
##############################################################################
##############################################################################

#add transportation data from Table_3 to combined_data
combined_data <- merge(combined_data, Table_3, by = "StudyYear", all = FALSE)
print(colnames(combined_data))

# perform a regression analysis between oil consumption and the 5 columns added to merged_data from Table_3
# Perform linear regression
regression_model_transport <- lm(Oil_Consumption_mmbd * 1000000 ~ X2_Wheelers + Cars + Buses + Commercial + Others, data = combined_data)
summary(regression_model_transport)

# create a plot of oil consumption vs the 5 columns in regression_Model_transport
plot_transportation_oil_consumption <- function(data) {
  ggplot(data, aes(x = X2_Wheelers)) +
    geom_point(aes(y = Oil_Consumption_mmbd, color = "2 Wheelers")) +
    geom_smooth(aes(y = Oil_Consumption_mmbd, color = "2 Wheelers"), method = "lm", se = TRUE) +
    geom_point(aes(y = Cars, color = "Cars")) +
    geom_smooth(aes(y = Cars, color = "Cars"), method = "lm", se = TRUE) +
    geom_point(aes(y = Buses, color = "Buses")) +
    geom_smooth(aes(y = Buses, color = "Buses"), method = "lm", se = TRUE) +
    geom_point(aes(y = Commercial, color = "Commercial")) +
    geom_smooth(aes(y = Commercial, color = "Commercial"), method = "lm", se = TRUE) +
    geom_point(aes(y = Others, color = "Others")) +
    geom_smooth(aes(y = Others, color = "Others"), method = "lm", se = TRUE) +
    theme_minimal() +
    labs(
      title = "Transportation vs Oil Consumption",
      x = "Transportation",
      y = "Oil Consumption (mmbd)",
      color = "Transportation"
    ) +
    theme(
      plot.title = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.position = "bottom"
    )
}
plot(plot_transportation_oil_consumption(combined_data))

# Calculate the estimated value of consumption using the regression model
combined_data$estimated_consumption <- predict(regression_model_transport, newdata = combined_data)

# Create a scatter plot of estimated consumption vs total consumption
consumption_plot <- ggplot(combined_data, aes(x = estimated_consumption, y = Oil_Consumption_mmbd)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Estimated Consumption vs Total Consumption",
    x = "Estimated Consumption (mmbd)",
    y = "Total Consumption (mmbd)",
    color = "Legend"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Data Points" = "black", "Fit Line" = "red")) +
  guides(color = guide_legend(title = "Legend")) +
  annotate("text", x = Inf, y = Inf, label = paste("Adj R2 = ", round(summary(regression_model_transport)$adj.r.squared, 2),
                                                   "\nIntercept =", round(summary(regression_model_transport)$coefficients[1, 1], 2),
                                                   "\nX2_Wheelers =", round(summary(regression_model_transport)$coefficients[2, 1], 2),
                                                   "\nCars =", round(summary(regression_model_transport)$coefficients[3, 1], 2),
                                                   "\nBuses =", round(summary(regression_model_transport)$coefficients[4, 1], 2),
                                                   "\nCommercial =", round(summary(regression_model_transport)$coefficients[5, 1], 2),
                                                   "\nOthers =", round(summary(regression_model_transport)$coefficients[6, 1], 2),
                                                   "\nP =", round(summary(regression_model_transport)$coefficients[2, 4], 4)),
           hjust = 1.1, vjust = 1.1, size = 4, color = "blue")

# Display the plot
plot(consumption_plot)

# Save the plot
ggsave("estimated_vs_total_consumption.png", plot = consumption_plot, width = 10, height = 6, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("estimated_vs_total_consumption.png"))

# Fit a linear regression model
model <- lm(Oil_Consumption_mmbd ~ X2_Wheelers + Cars + Buses + Commercial + Others, data = combined_data)
# Add a column of total vehicles
combined_data <- combined_data %>%
  mutate(total_vehicles = X2_Wheelers + Cars + Buses + Commercial + Others)
# Scatter plot with regression line
scatter_plot <- ggplot(combined_data, aes(x = total_vehicles, y = Oil_Consumption_mmbd)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(title = "Scatter Plot with Regression Line",
       x = "2 Wheelers",
       y = "Oil Consumption (mmbd)")
plot(scatter_plot)
# Residuals vs Fitted Values Plot
residuals_fitted_plot <- ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

# Normal Q-Q Plot
qq_plot <- ggplot(model, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Standardized Residuals")

# Scale-Location Plot
scale_location_plot <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "Square Root of Standardized Residuals")

# Residuals vs Leverage Plot
residuals_leverage_plot <- ggplot(model, aes(.hat, .stdresid)) +
  geom_point() +
  geom_smooth(se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals")

# Combine the plots into a multi-panel plot using patchwork
multi_panel_plot <- (scatter_plot | residuals_fitted_plot) / (qq_plot | scale_location_plot) / residuals_leverage_plot

# Display the combined plot
plot(multi_panel_plot)

# Save the combined plot
ggsave("multi_panel_regression_diagnostics.png", plot = multi_panel_plot, width = 16, height = 12, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("multi_panel_regression_diagnostics.png"))


library(jsonlite)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/registered-vehicles-per-1000-people.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/registered-vehicles-per-1000-people.metadata.json?v=1&csvType=full&useColumnShortNames=true")

#upload data from Registered_Motor_Vehicles_in_Bangladesh_1951-2013.csv in the working directory to a new table
Bangladesh_registered_vehicles <- read.csv("Registered_Motor_Vehicles_in_Bangladesh_1951-2013.csv")

# plot the data from Bangladesh_registered_vehicles along with the 5 columns of vehicle data from Table_3 with lines for columns of data over time

# Combine the data from Bangladesh_registered_vehicles and Table_3
# Ensure column names match before merging
print(colnames(Bangladesh_registered_vehicles))
colnames(Bangladesh_registered_vehicles)[colnames(Bangladesh_registered_vehicles) == "Year..As.on.31st.March."] <- "StudyYear"

combined_vehicles_data <- merge(Bangladesh_registered_vehicles, Table_3, by = "StudyYear", all = TRUE)
print(colnames(combined_vehicles_data))

# Check if the columns exist before plotting
required_columns <- c("Two.Wheelers.", "Cars..Jeeps.and.Taxis", "Buses..", "Goods.Vehicles", "Others.")
missing_columns <- setdiff(required_columns, colnames(combined_vehicles_data))

if (length(missing_columns) > 0) {
  stop(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
}



  # Add "Entity" column to Table_3 with values of "Equatoria"
  Table_3$Entity <- "Equatoria"

  # Save the updated Table_3 as "Eq_In_Vehicle_data"
  Eq_In_Vehicle_data <- Table_3

  # View the updated Eq_In_Vehicle_data to verify the new column
  View(Eq_In_Vehicle_data)
  
  # Add "Entity" column to Bangladesh_registered_vehicles with values of "Bangladesh"
  Bangladesh_registered_vehicles$Entity <- "Bangladesh"

  # View the updated Bangladesh_registered_vehicles to verify the new column
  View(Bangladesh_registered_vehicles)

Bangladesh_registered_vehicles <- Bangladesh_registered_vehicles %>%  
  relocate(Entity, .after = StudyYear)

# Add a column to Table_3 that is a sum of the other columns and insert it adjacent to the StudyYear
Eq_In_Vehicle_data <- Eq_In_Vehicle_data %>%
  mutate(Total_Vehicles = rowSums(dplyr::select(., X2_Wheelers, Common_Vehicles, Buses, Commercial, Others), na.rm = TRUE)) %>%
  relocate(Total_Vehicles, .after = StudyYear)
Eq_In_Vehicle_data <- Eq_In_Vehicle_data %>%  
  relocate(Entity, .after = StudyYear)

# View the updated Table_3 to verify the new column
View(Eq_In_Vehicle_data)

# Append the Bangladesh_registered_vehicles data table to Eq_In_Vehicle_data
colnames(Bangladesh_registered_vehicles) <- colnames(Eq_In_Vehicle_data)

Eq_In_Vehicle_data <- rbind(Eq_In_Vehicle_data, Bangladesh_registered_vehicles)

# View the combined data to verify the append operation
View(Eq_In_Vehicle_data)

# Combine the two plots into a two-panel plot using patchwork
combined_vehicle_plot <- vehicle_plot / vehicle_plot_Bangladesh + plot_layout(ncol = 1, heights = c(1, 1))

# Display the combined plot
plot(combined_vehicle_plot)

# Append the Bangladesh_registered_vehicles data table to Table_3
Table_3 <- rbind(Table_3, Bangladesh_registered_vehicles)

# View the combined data to verify the append operation
View(Table_3)


# Plot the data from Eq_In_Vehicle_data for Equatoria
vehicle_plot <- ggplot(Eq_In_Vehicle_data %>% filter(Entity == "Equatoria"), aes(x = StudyYear)) +
  geom_line(aes(y = X2_Wheelers, color = "2 Wheelers")) +
  geom_line(aes(y = Common_Vehicles, color = "Cars")) +
  geom_line(aes(y = Buses, color = "Buses")) +
  geom_line(aes(y = Commercial, color = "Commercial")) +
  geom_line(aes(y = Others, color = "Others")) +
  theme_minimal() +
  labs(
    title = "Vehicle Data Over Time for Equatoria",
    x = "Year",
    y = "Number of Vehicles",
    color = "Vehicle Type"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )

# Plot the data from Eq_In_Vehicle_data for Bangladesh
vehicle_plot_Bangladesh <- ggplot(Eq_In_Vehicle_data %>% filter(Entity == "Bangladesh"), aes(x = StudyYear)) +
  geom_line(aes(y = X2_Wheelers, color = "2 Wheelers")) +
  geom_line(aes(y = Common_Vehicles, color = "Cars")) +
  geom_line(aes(y = Buses, color = "Buses")) +
  geom_line(aes(y = Commercial, color = "Commercial")) +
  geom_line(aes(y = Others, color = "Others")) +
  theme_minimal() +
  labs(
    title = "Vehicle Data Over Time for Bangladesh",
    x = "Year",
    y = "Number of Vehicles",
    color = "Vehicle Type"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )

# Display the plots
plot(vehicle_plot)
plot(vehicle_plot_Bangladesh)

# Create a pie chart for Equatoria's vehicle data
equatoria_vehicle_data <- Eq_In_Vehicle_data %>%
  filter(Entity == "Equatoria" & StudyYear == max(StudyYear)) %>%
  dplyr::select(X2_Wheelers, Common_Vehicles, Buses, Commercial, Others) %>%
  pivot_longer(cols = everything(), names_to = "Vehicle_Type", values_to = "Count")

equatoria_pie_chart <- ggplot(equatoria_vehicle_data, aes(x = "", y = Count, fill = Vehicle_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Equatoria Vehicle Data by Type",
       fill = "Vehicle Type") +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "bottom"
  )

# Create a pie chart for Bangladesh's vehicle data
Bangladesh_vehicle_data <- Eq_In_Vehicle_data %>%
  filter(Entity == "Bangladesh" & StudyYear == max(StudyYear)) %>%
  dplyr::select(X2_Wheelers, Common_Vehicles, Buses, Commercial, Others) %>%
  pivot_longer(cols = everything(), names_to = "Vehicle_Type", values_to = "Count")

Bangladesh_pie_chart <- ggplot(Bangladesh_vehicle_data, aes(x = "", y = Count, fill = Vehicle_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_minimal() +
  labs(title = "Bangladesh Vehicle Data by Type",
       fill = "Vehicle Type") +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "bottom"
  )

plot(Bangladesh_pie_chart)
# Combine the two pie charts into a side-by-side plot using patchwork
combined_pie_charts <- equatoria_pie_chart + Bangladesh_pie_chart + plot_layout(ncol = 2, widths = c(1, 1))

# Display the combined pie charts
plot(combined_pie_charts)

# Save the combined pie charts
ggsave("combined_pie_charts.png", plot = combined_pie_charts, width = 16, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("combined_pie_charts.png"))

print(colnames(Eq_In_Vehicle_data))

# Save the combined plot
ggsave("combined_vehicle_plot.png", plot = combined_vehicle_plot, width = 12, height = 16, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("combined_vehicle_plot.png"))
# Save the plot
ggsave("vehicle_data_over_time.png", plot = vehicle_plot, width = 12, height = 8, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("vehicle_data_over_time.png"))


##############################################################################
##############################################################################
############## Social Cost of Carbon  ########################################
##############################################################################
##############################################################################
##############################################################################

# Scatter plot with fit line for GDP vs Emissions per capita
ggplot(equatoria_data, aes(x = emissions_per_capita, y = gdp_per_capita)) +
  geom_point(aes(color = "Data Points")) +
  geom_smooth(aes(color = "Fit Line"), method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "GDP vs Emissions per Capita",
    x = "Emissions per Capita (tonnes)",
    y = "GDP per Capita (USD)",
    color = "Legend"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.position = "bottom"
  )

# Save the plot
ggsave("gdp_vs_emissions_per_capita.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 300)

# Display the plot in vscode window
grid.raster(readPNG("gdp_vs_emissions_per_capita.png"))

