# cargar paquetes ---------------------------------------------------------

if( !require( "pacman" ) ) {
  install.packages( "pacman" )
}

library("pacman")

p_load("vroom", "glue", "scales", "ggpubr", "ggsci", "tidyverse")



# load so2 data
so2_csv.f <- list.files(path = "../tmp-results/so2_15.22_data/",
                        pattern = ".csv", full.names = T)

# Binding CSV Tables with Purrr 
so2_15.22 <- map_df(so2_csv.f, vroom)



# calculate min value
so2_dataset_minvalue <- min(so2_15.22[so2_15.22$value > 0, 4], na.rm = T)



# add extra data
so2_15.22_labeled <- so2_15.22 %>% 
  mutate(semestre = case_when(
    month == "ene" ~ "semestre_1",
    month == "feb" ~ "semestre_1",
    month == "mar" ~ "semestre_1",
    month == "abr" ~ "semestre_1",
    month == "may" ~ "semestre_1",
    month == "jun" ~ "semestre_1",
    month == "jul" ~ "semestre_2",
    month == "ago" ~ "semestre_2",
    month == "sep" ~ "semestre_2",
    month == "oct" ~ "semestre_2",
    month == "nov" ~ "semestre_2",
    month == "dic" ~ "semestre_2"
    )) %>% 
  mutate(value_label = case_when(
    is.na(value) ~ "missing_value",
    value == 0 ~ "error_min_value",
    value > 0 ~ "registered_value"
  )) %>% 
  mutate(modified_so2_values = ifelse(
    value == 0,
    yes = so2_dataset_minvalue, 
    no = value)) %>% 
  mutate(log10_pm2.5_values = log10(modified_so2_values))


