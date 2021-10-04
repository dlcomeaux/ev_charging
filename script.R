
################################################################################
# Load libraries
################################################################################

library(tidyverse)
library(sf)
library(cmapgeo)
library(cmapplot)

################################################################################
# Load MDT data from separate repo
################################################################################

source("../mydailytravel/R/data_cleaning.R")
source("../mydailytravel/R/helper_fns.R")
setwd("../ev_charging")

################################################################################
# Prep data
################################################################################

household_vehicles <-
  mdt_all_respondents %>% 
  select(sampno,perno,weight,hhinc,hhveh,
         home_state,home_county,home_tract,home_county_chi,geog,
         income,income_c,race_eth) %>% 
  filter(perno == 1) %>% 
  select(-perno) %>%
  left_join(veh, by = "sampno") %>% 
  group_by(sampno) %>% 
  mutate(household_race_eth_concat = paste(race_eth, collapse = ",")) %>% 
  ungroup() %>% 
  mutate(
    household_race_eth = case_when(
      grepl("white",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("hispanic",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "white",
      grepl("hispanic",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "hispanic",
      grepl("other",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("hispanic",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "other",
      grepl("asian",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("hispanic",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "asian",
      grepl("black",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("hispanic",household_race_eth_concat, fixed = TRUE)) ~ "black",
      TRUE ~ "mixed"
    ))

################################################################################
# Parking
################################################################################

household_parking <-
  household_vehicles %>% # 21498
  ungroup() %>% 
  # Add values for households with no vehicle
  replace_na(list(parkd = 0)) %>% 
  # Remove entries without parking information
  filter(parkd > -1) %>% # 21484
  # Remove entries with "other" parking
  filter(parkd != 97) %>% 
  group_by(sampno,weight,hhinc,hhveh,
           home_state,home_county,home_tract,home_county_chi,geog,
           income,income_c,race_eth) %>%
  # First identify the lowest value for parking - i.e., if a household parks any
  # car(s) on the street, that is what they are coded as. Also identify
  # housholds that have any access to off-street parking.
  summarize(min_parking = min(parkd),
            max_parking = max(parkd)
            ) %>% # 12389
  ungroup() %>% 
  # Recode for ease of presentation and cluster "Off street" and "Garage" (2 and 3)
  mutate(all_parking = recode_factor(factor(min_parking,levels = c(0,1,2,3)),
                               "0" = "No vehicle",
                               "1" = "Park car(s) on-street",
                               "2" = "Only park car(s) off-street",
                               "3" = "Only park car(s) off-street"),
         any_parking = recode_factor(factor(max_parking,levels = c(0,1,2,3)),
                                     "0" = "No vehicle",
                                     "1" = "Only has street parking",
                                     "2" = "Has off-street parking",
                                     "3" = "Has off-street parking"),
         combo_parking = case_when(
           min_parking == 1 & 
             max_parking %in% c(2,3) ~ "On- and off-street",
           min_parking == 1 ~ "On-street only",
           min_parking == 0 ~ "No vehicle",
           max_parking %in% c(2,3) ~ "Off-street only"
           
         ))


# Identify percentage of parking behavior and vehicle ownership
any_parking_behavior <-
  pct_calculator(
    household_parking %>% 
      filter(geog != "Homes in multiple jurisdictions (Chicago/Cook)"),
    breakdown_by = "any_parking",
    second_breakdown = "geog",
    weight = "weight") %>% arrange(any_parking,geog) %>% 
  mutate(pct = round(pct,4))

all_parking_behavior <-
  pct_calculator(
    household_parking %>% 
      filter(geog != "Homes in multiple jurisdictions (Chicago/Cook)"),
    breakdown_by = "all_parking",
    second_breakdown = "geog",
    weight = "weight") %>% arrange(all_parking,geog) %>% 
  mutate(pct = round(pct,4))

combo_parking_behavior <-
  pct_calculator(
    household_parking %>% 
      filter(geog != "Homes in multiple jurisdictions (Chicago/Cook)"),
    breakdown_by = "combo_parking",
    second_breakdown = "geog",
    weight = "weight") %>% arrange(combo_parking,geog) %>% 
  mutate(pct = round(pct,4)) %>% 
  mutate(combo_parking = factor(combo_parking,
                                levels = c("No vehicle",
                                           "On-street only",
                                           "On- and off-street",
                                           "Off-street only")))

parking_p1 <-
  # Get data
  combo_parking_behavior %>% 
  # Reorder
  mutate(geog = factor(geog, levels = c("Collar and adjacent counties",
                                        "Suburban Cook County",
                                        "Chicago")),
         combo_parking = factor(combo_parking, 
                              levels = rev(levels(combo_parking)))) %>% 
  # Create ggplot object
  ggplot(aes(x = pct, y = str_wrap_factor(geog,15), fill = combo_parking,
             group = combo_parking,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(position = position_stack(reverse = T)) +
  
  geom_text(position = position_stack(vjust = 0.5, reverse = T),
            color = "white") +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",
             xlab = "Households by vehicle ownership and parking behavior",
             legend.max.columns = 2) +
  cmap_fill_discrete(palette = "environment", reverse = T) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent())


finalize_plot(parking_p1,
              "Chicago households are the most likely to not have a vehicle or 
              to rely on street parking, but there are similar households in the 
              rest of the region as well.",
              paste0("Note: Households noted as \"On- and off-street\" have 
              multiple cars, at least one of which is parked on-street and at 
              least one of which is parked off-street.
              Categories without labels have values of less than 5 percent.
              <br><br>
              Sample size: 
              <br>- Chicago (",
                     parking_behavior %>% ungroup() %>% 
                       filter(geog == "Chicago") %>% 
                       select(total_n) %>% 
                       distinct() %>% 
                       select(total_n)
                     ,");
              <br>- Suburban Cook (",
                     parking_behavior %>% ungroup() %>% 
                       filter(geog == "Suburban Cook County") %>% 
                       select(total_n) %>% distinct() %>% 
                       select(total_n)
                     ,");
              <br>- Collar and adjacent (",
                     parking_behavior %>% ungroup() %>% 
                       filter(geog == "Collar and adjacent counties") %>% 
                       select(total_n) %>% 
                       distinct() %>% 
                       select(total_n)
                     ,").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "parking_p1",
              mode = "png",
              overwrite = T)

################################################################################
# Regression on income and parking behavior
################################################################################

household_parking_lm <-
  household_parking %>% 
  filter(max_parking > 0) %>% 
  mutate(onstreet = max_parking == 1) %>% 
  filter(geog == "Chicago",
         income_c != "missing") %>% 
  mutate(income_dummy = case_when(
    income_c %in% c("high","middle-high") ~ 1,
    TRUE ~ 0
  )) %>% 
  lm(onstreet ~ income_dummy,.)

summary(household_parking_lm)


# ################################################################################
# # Fuel type
# ################################################################################
# 
# household_fuel_type <-
#   household_vehicles %>% 
#   replace_na(list(fuel = 0)) %>% 
#   filter(fuel > -1) %>%
#   # group_by(sampno,wtperfin,hhinc,hhveh,
#   #          home_state,home_county,home_tract,home_county_chi,geog,
#   #          income,income_c,race_eth) %>% 
#   # summarize(park_simp = min(parkd)) %>% 
#   mutate(fuel = recode_factor(factor(fuel,levels = c(0,1,2,3,4,5,97,-7,-8)),
#                               "0" = "No vehicle",
#                               "1" = "Gas",
#                               "2" = "Diesel",
#                               "3" = "Hybrid",
#                               "4" = "EV",
#                               "5" = "Alternative",
#                               "97" = "Other",
#                               "-7" = "Missing",
#                               "-8" = "Missing"))
# 
# pct_calculator(household_fuel_type %>% filter(!(fuel %in% c("No vehicle","Missing"))),
#                breakdown_by = "fuel",
#                second_breakdown = "household_race_eth",
#                weight = "wtperfin") %>% View()
# 
# 
# household_fuel_type %>% filter(fuel == "EV") %>% count(household_race_eth,wt = wtperfin)
