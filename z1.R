library(tidyverse)
library(rnoaa)
station_data = ghcnd_stations()

kurgan = data.frame(id = "KURGAN", latitude = 55.45,  longitude = 65.3333)
kur_around = meteo_nearby_stations(lat_lon_df = kurgan, station_data = station_data,
                                      limit = 2, var = c("PRCP", "TAVG"),
                                      year_min = 1989, year_max = 2014)
kurgan_id = kur_around[["KURGAN"]][["id"]]
all_kurgan_data = meteo_tidy_ghcnd(stationid = kurgan_id, var=c("TAVG"),
                                   date_min = "1989-01-01", date_max = "2014-12-31")

all_kurgan_data = all_kurgan_data %>% mutate(tavg = tavg / 10)
all_kurgan_data = all_kurgan_data %>% filter(tavg > 5)

all_kurgan_data <- all_kurgan_data %>% mutate(year = year(date), month = month(date))
all_kurgan_data <- all_kurgan_data %>% group_by(id, year, month) %>% summarise(sum_month = mean(tavg, na.rm = TRUE))

var_table <- data.frame(
  m = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00),
  bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00),
  di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
)
q = 1600
l = 2.2
e = 25
k = 300

all_kurgan_data <- all_kurgan_data %>% 
  mutate(yj = ((var_table[["afi"]][month] + var_table[["bfi"]][month] * 1.0 * sum_month) * var_table[["di"]][month] * k)/(q * l * (100 - e)))

all_kurgan_data <- all_kurgan_data %>% group_by(id, year) %>% summarise(sum_year = mean(yj, na.rm = TRUE))
all_kurgan_data <- all_kurgan_data %>% group_by(id) %>% summarise(sum_station = mean(sum_year, na.rm = TRUE))
Y <- sum(all_kurgan_data$sum_station)
print(paste(Y, "тонн")) 
