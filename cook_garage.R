library(sf)
library(tidyverse)
library(cmapgeo)
library(cmapplot)

cook_codes <- sf::st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Cook.gdb",
                          layer = "ClassCodes_Cook_2013")

cook_2019 <- sf::st_read(dsn = "V:/Cadastral_and_Land_Planning/AssessorData/AssessorData_Cook.gdb",
                         layer = "AssessorData_Cook_2019") %>%
  select(PIN,
         neighborhood,
         city,zip_4,
         type_res,overall_class,no_apt,
         garage_size) %>% 
  mutate(zip = substr(zip_4,1,5)) %>% 
  left_join(cook_codes,by = "overall_class") %>% 
  select(-Cnt_overall_class)

no_garage_by_zip <-
  cook_2019 %>%
  filter(zip != "00000") %>%
  # Keep only small residential
  filter(MajorClass %in% c("2","9")) %>% 
  # Exclude buildings with more than 6 units
  filter(!(overall_class %in% c("299","913","914","915","918","991","996"))) %>% 
  mutate(flag = case_when(
    garage_size == "0" ~ 0,
    TRUE ~ 1)) %>% 
  group_by(zip) %>% 
  mutate(total_n = n()) %>% 
  filter(total_n > 10) %>% 
  ungroup() %>% 
  count(zip,flag) %>% 
  group_by(zip) %>% 
  mutate(pct = n/sum(n)) %>% 
  filter(flag == 0)

zips <- cmapgeo::zcta_sf %>% 
  right_join(no_garage_by_zip, by = c("geoid_zcta" = "zip"))

plot(zips %>% select(pct))

p1 <- 
  zips %>% 
  ggplot(aes(fill = pct)) +
  geom_sf() +
  scale_fill_binned(labels = scales::label_percent(accuracy = 1),
                    type = "viridis",
                    n.breaks = 10) +
  theme_cmap(axis.text = element_blank(),
             legend.position = "right",
             legend.direction = "vertical",
             legend.key.height = grid::unit(30,"bigpts"))

finalize_plot(p1,
              "Residential parcels with no identified garage by ZIP code, 2019.",
              sidebar_width = 0,
              legend_shift = F,
              width = 5,
              filename = "garage_p1.png",
              # mode = "png",
              overwrite = T)
