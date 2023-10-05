# cargar paquetes ---------------------------------------------------------

if( !require( "pacman" ) ) {
  install.packages( "pacman" )
}

library("pacman")

p_load("rsinaica", "tidyverse", "glue")


# definir rango de fechas de datos ----------------------------------------

# obtener todos los dias y meses en formato ymd 
# del 2015 al 2021

dates_2015.2022 <- seq(ymd("2015-01-01"),
                       ymd("2022-12-31"), 
                       by = "1 day") %>% 
  tibble() %>% 
  rename("full_dates" = 1) %>% 
  mutate(days = day(full_dates),
         months = month(full_dates, label = TRUE),
         years = year(full_dates))


# extraer los primeros y ultimos dias del objeto anterior
fst.lst_days <- dates_2015.2022 %>%
  group_by(years, months ) %>%
  summarize(
    primer_dia_mes = first(full_dates),
    ultimo_dia_mes = last(full_dates)) %>% 
  ungroup()



# crear una funcion para obtener los datos de una variable por year -------

get_sinaica_data <- function(days_df, 
                 year, 
                 station_id_num, 
                 parameter_chr,
                 data.type_chr){

  fst.lst.days_df <- days_df
  
  filtered_df <- fst.lst.days_df %>% 
    dplyr::filter(years == {{year}})
  
  
  ene_days <- filtered_df %>% 
    dplyr::filter(months == "ene")
  
  ene_data <- sinaica_station_data(station_id = station_id_num, 
                                   parameter = parameter_chr, 
                                   start_date = as.character(ene_days$primer_dia_mes), 
                                   end_date =  as.character(ene_days$ultimo_dia_mes),
                                   type = data.type_chr)
  
  
  feb_days <- filtered_df %>%
    dplyr::filter(months == "feb")

  feb_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(feb_days$primer_dia_mes), 
                                    end_date =  as.character(feb_days$ultimo_dia_mes),
                                    type = data.type_chr)

    
  mar_days <- filtered_df %>%
    dplyr::filter(months == "mar")
  
  mar_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(mar_days$primer_dia_mes), 
                                    end_date =  as.character(mar_days$ultimo_dia_mes),
                                    type = data.type_chr)
  
  
  abr_days <- filtered_df %>%
    dplyr::filter(months == "abr")
  
  abr_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(abr_days$primer_dia_mes), 
                                    end_date =  as.character(abr_days$ultimo_dia_mes),
                                    type = data.type_chr)

  
  may_days <- filtered_df %>%
    dplyr::filter(months == "may")
  
  may_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(may_days$primer_dia_mes), 
                                    end_date =  as.character(may_days$ultimo_dia_mes),
                                    type = data.type_chr)
  

  jun_days <- filtered_df %>%
    dplyr::filter(months == "jun")
  
  jun_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(jun_days$primer_dia_mes), 
                                    end_date =  as.character(jun_days$ultimo_dia_mes),
                                    type = data.type_chr)
  

  jul_days <- filtered_df %>%
    dplyr::filter(months == "jul")
  
  jul_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(jul_days$primer_dia_mes), 
                                    end_date =  as.character(jul_days$ultimo_dia_mes),
                                    type = data.type_chr)
  

  ago_days <- filtered_df %>%
    dplyr::filter(months == "ago")
  
  ago_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(ago_days$primer_dia_mes), 
                                    end_date =  as.character(ago_days$ultimo_dia_mes),
                                    type = data.type_chr)
  

  sep_days <- filtered_df %>%
    dplyr::filter(months == "sep")
  
  sep_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(sep_days$primer_dia_mes), 
                                    end_date =  as.character(sep_days$ultimo_dia_mes),
                                    type = data.type_chr)
  

  oct_days <- filtered_df %>%
    dplyr::filter(months == "oct")

  oct_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(oct_days$primer_dia_mes), 
                                    end_date =  as.character(oct_days$ultimo_dia_mes),
                                    type = data.type_chr)
  
  
  nov_days <- filtered_df %>%
    dplyr::filter(months == "nov")
  
  nov_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(nov_days$primer_dia_mes), 
                                    end_date =  as.character(nov_days$ultimo_dia_mes),
                                    type = data.type_chr)
  

  dic_days <- filtered_df %>%
    dplyr::filter(months == "dic")
  
  dic_data <- sinaica_station_data(station_id = station_id_num, 
                                    parameter = parameter_chr, 
                                    start_date = as.character(dic_days$primer_dia_mes), 
                                    end_date =  as.character(dic_days$ultimo_dia_mes),
                                    type = data.type_chr)
  
  
  full_df <- bind_rows(ene_data, feb_data, mar_data, abr_data, may_data, jun_data,
                        jul_data, ago_data, sep_data, oct_data, nov_data, dic_data) %>% 
    mutate(data = {{parameter_chr}}) %>% 
    mutate(data_type = {{data.type_chr}}) %>% 
    mutate(day = day(date),
           month = month(date, label = TRUE),
           year = year(date))
  }







# check
# https://stackoverflow.com/questions/51932769/combine-multiple-data-frames-with-similar-non-consecutive-names

