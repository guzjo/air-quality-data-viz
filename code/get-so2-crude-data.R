# cargar paquetes ---------------------------------------------------------

if( !require( "pacman" ) ) {
  install.packages( "pacman" )
}

library("pacman")

p_load("rsinaica", "tidyverse", "glue")



# cargar objetos del script que contiene la funcion de obtencion de datos de SINAICA --------
source(file = "sinaica-function.R")


# Checar los datos de estaciones del estado de Puebla
# para poder establecerlos en la funcion

puebla_stations <- stations_sinaica %>% 
  filter(network_name == "Puebla")

# Obtener los parametros que las EMA recolectan
ninfas_puebla_params <- sinaica_station_params(162)
a.sta_puebla_params <- sinaica_station_params(161)
bine_puebla_params <- sinaica_station_params(163)
velodromo_puebla_params <- sinaica_station_params(165)
utp_puebla_params <- sinaica_station_params(406)

# Obtener la fecha inicial
# y actual de los datos de alguna EMA 
# EMA_puebla_dates <- sinaica_station_dates(162, "Crude")


# Usando la funcion creada en otro script,
# obtener los datos crudos de SO2 de diversos years de 
# Ninfas, A. Sta, BINE, Velodromo, UTP

utp_so2_2015_crude <- get_sinaica_data(days_df = fst.lst_days, 
                                   year = 2015, 
                                   station_id_num = 406,
                                   parameter_chr = "SO2",
                                   data.type_chr = "Crude")


utp_so2_2016_crude <- get_sinaica_data(days_df = fst.lst_days, 
                                   year = 2016, 
                                   station_id_num = 406,
                                   parameter_chr = "SO2",
                                   data.type_chr = "Crude")


utp_so2_2017_crude <- get_sinaica_data(days_df = fst.lst_days, 
                                   year = 2017, 
                                   station_id_num = 406,
                                   parameter_chr = "SO2",
                                   data.type_chr = "Crude")


utp_so2_2018_crude <- get_sinaica_data(days_df = fst.lst_days, 
                                   year = 2018, 
                                   station_id_num = 406,
                                   parameter_chr = "SO2",
                                   data.type_chr = "Crude")


utp_so2_2019_crude <- get_sinaica_data(days_df = fst.lst_days, 
                                   year = 2019, 
                                   station_id_num = 406,
                                   parameter_chr = "SO2",
                                   data.type_chr = "Crude")


utp_so2_2020_crude <- get_sinaica_data(days_df = fst.lst_days, 
                                   year = 2020, 
                                   station_id_num = 406,
                                   parameter_chr = "SO2",
                                   data.type_chr = "Crude")


utp_so2_2021_crude <- get_sinaica_data(days_df = fst.lst_days, 
                       year = 2021, 
                       station_id_num = 406,
                       parameter_chr = "SO2",
                       data.type_chr = "Crude")


utp_so2_2022_crude <- get_sinaica_data(days_df = fst.lst_days, 
                       year = 2022, 
                       station_id_num = 406,
                       parameter_chr = "SO2",
                       data.type_chr = "Crude")


# join all dfs with data
utp_so2_15.22_crude <- bind_rows(utp_so2_2015_crude, 
                                 utp_so2_2016_crude, 
                                 utp_so2_2017_crude,
                                 utp_so2_2018_crude, 
                                 utp_so2_2019_crude,
                                 utp_so2_2020_crude, 
                                 utp_so2_2021_crude, 
                                 utp_so2_2022_crude) %>% 
  arrange(date, month, day, hour) 


# rejoin with all dates from 2015 to 2022
hours_2015.2022 <- seq(ymd_hm("2015-01-01 0:00"),
                       ymd_hm("2022-12-31 23:00"),
                       by = "hour") %>% 
  tibble() %>% 
  rename("date" = 1) %>% 
  mutate(hour = hour(date),
         day = day(date),
         month = month(date, label = TRUE),
         year = year(date),
         date_no.h = as.Date(date)) %>% 
  select(2:6) %>% 
  rename("date" = "date_no.h")

hours_2015.2022$date <- as.character(hours_2015.2022$date)


full_utp_so2.crude_15.22 <- right_join(utp_so2_15.22_crude, 
                                          hours_2015.2022, 
                                         by = c("date", "hour", "day", "month", "year")) %>% 
  arrange(date, hour) %>% 
  fill(station_id, station_name, unit, data, data_type,
       .direction = "up") 


write.csv(x = full_utp_so2.crude_15.22, 
          file = "../tmp-results/full_utp_so2.crude_15.22.csv", 
          row.names = F)  # write csv file
