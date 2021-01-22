library(readxl)
library(readr)
library(ggplot2)
library(NbClust)
library(factoextra)
library(usmap)
library(maps)
library(dplyr)
library(tools)
library(mapproj)
library(GGally)
library(RColorBrewer)
library(shinyWidgets)
library(tidyr)

#colorblind friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000")

# geospatial data for mapping
us_states <- map_data("state")
us_states$region <- toTitleCase(us_states$region)

# co2 emissions data
state_emissions_2017 <- read_excel("co2_emissions_profile_by_state.xlsx", skip = 4, n_max = 51,col_names = c("state", "commercial_tons", "electric_power_tons", "residential_tons",	"industrial_tons",	"transportation_tons", "total_tons", "blank", "commercial_pct",	"electric_power_pct",	"residential_pct",	"industrial_pct",	"transportation_pct" 
))
state_emissions_2017 <- state_emissions_2017[-9,]
state_emissions_2017$blank <- NULL

# population data
pop <- read_csv("nst-est2019-alldata.csv", col_types = cols_only(NAME = "c", POPESTIMATE2017 = "i"))
pop$state <- pop$NAME
pop$NAME <- NULL
pop$population <- pop$POPESTIMATE2017
pop$POPESTIMATE2017 <- NULL
pop <- pop[-c(1:5,14,57),]

# gdp data
gdp <- read_excel("gdp_by_state_2017_bea.xls", range = "B8:C58", col_names = c("state", "gdp"))

# lat long data points for each state
state_lat_long <- read_csv("world_country_and_usa_states_latitude_and_longitude_values.csv", 
                           col_types = cols(country = col_skip(), 
                                            country_code = col_skip(), latitude = col_skip(), 
                                            longitude = col_skip(), usa_state_code = col_skip()))

state_lat_long <- na.omit(state_lat_long)
state_lat_long$state <- state_lat_long$usa_state
state_lat_long$usa_state <- NULL

# partisan lean data
partisan_lean <- read_csv("fivethirtyeight_partisan_lean_STATES.csv")
partisan_lean <- partisan_lean[-c(2:4,6)]
partisan_lean$rep_lean <- -partisan_lean$dem_lean

# electricity price data
avgprice_annual <- read_excel("avgprice_annual.xlsx", skip = 1)
avgprice_annual <- avgprice_annual[avgprice_annual$Year == 2017,]
avgprice_annual <- avgprice_annual[avgprice_annual$`Industry Sector Category` =="Total Electric Industry",]
avgprice_annual$state <- state.name[match(avgprice_annual$State,state.abb)]
avgprice_annual <- avgprice_annual[-c(1:3,8)]
colnames(avgprice_annual) <-  c("Residential Price", "Commercial Price", "Inudstrial Price", "Transportation Price", "Total Price", "state")

# electricity source data
annual_generation_state <- read_excel("annual_generation_state.xls", skip = 1)
annual_generation_state <- annual_generation_state[annual_generation_state$YEAR == 2017,]
annual_generation_state <- annual_generation_state[annual_generation_state$`TYPE OF PRODUCER`=="Total Electric Power Industry",]
annual_generation_state$state <- state.name[match(annual_generation_state$STATE,state.abb)]
annual_generation_state <- annual_generation_state[-c(1:3)]
colnames(annual_generation_state) <- c("generation_tech", "annual_generation", "state")
annual_generation_state <- na.omit(annual_generation_state)
annual_generation_state <- spread(annual_generation_state,generation_tech,annual_generation)
annual_generation_state[is.na(annual_generation_state)] <- 0
colnames(annual_generation_state) <- c("state", "coal_mwh", "geothermal_mwh", "hydro_mwh", "nat_gas_mwh", "nuclear_mwh", "other_mwh", "other_biomass_mwh", "other_gases_mwh", "petroleum_mwh", "pumped_storage_mwh", "solar_mwh", "total_mwh", "wind_mwh", "wood_biomass_mwh")

# climate data
hdd <- read_excel("2017_statelevel_climate_data.xlsx", 
                  sheet = "HDD_plaintext")
cdd <- read_excel("2017_statelevel_climate_data.xlsx", 
                  sheet = "CDD_plaintext")

hdd <- hdd[,c(1,14)]
cdd <- cdd[,c(1,14)]

hdd$state <- tolower(hdd$state)
hdd$state <- toTitleCase(hdd$state)
cdd$state <- tolower(cdd$state)
cdd$state <- toTitleCase(cdd$state)

# merging all data together
emissions_data <- merge(state_emissions_2017, gdp, by = "state")
emissions_data <- merge(emissions_data, pop, by = "state")
emissions_data <- merge(emissions_data, partisan_lean, by = "state")
emissions_data <- merge(emissions_data, state_lat_long, by = "state")
emissions_data <- merge(emissions_data, avgprice_annual, by = "state")
emissions_data <- merge(emissions_data, annual_generation_state, by = "state")
emissions_data <- merge(emissions_data, hdd, by = "state")
emissions_data <- merge(emissions_data, cdd, by = "state")

# deriving new data points
emissions_data$gdp_per_capita <- emissions_data$gdp / emissions_data$population

rownames(emissions_data) <- emissions_data$state
emissions_data$state <- NULL

colnames(emissions_data) <- c("Commercial Emissions (Tons of CO2)", "Electric Power Emissions (Tons of CO2)", "Residential Emissions (Tons of CO2)", "Industrial Emissions (Tons of CO2)", "Transportation Emissions (Tons of CO2)", "Total Emissions (Tons of CO2)", "Commercial Share of Emissions (%)", "Electric Power Share of Emissions (%)", "Residential Share of Emissions (%)", "Industrial Share of Emissions (%)", "Transportation Share of Emissions (%)", "Annual GDP (USD)", "Population", "Democractic Partisan Lean", "Republican Partisan Lean", "Latitude", "Longitude",  "Residential Price of Electricity (Cents/kWh)", "Commercial Price of Electricity (Cents/kWh)", "Inudstrial Price of Electricity (Cents/kWh)", "Transportation Price of Electricity (Cents/kWh)", "Total Price of Electricity (Cents/kWh)", "Coal Generation (MWh)", "Geothermal Generation (MWh)", "Hydroelectric Generation (MWh)", "Natural Gas Generation (MWh)", "Nuclear Generation (MWh)", "Other Generation (MWh)", "Other Biomass Generation (MWh)", "Other Gases Generation (MWh)", "Petroleum Generation (MWh)", "Pumped Storage Generation (MWh)", "Solar Generation (MWh)", "Total Generation (MWh)", "Wind Generation (MWh)", "Wood Biomass Generation (MWh)", "Average Monthly Heating Degree Days", "Average Monthly Cooling Degree Days", "GDP per Capita")
