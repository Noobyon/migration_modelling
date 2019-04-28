library(dplyr)
library(readr)
library(tidyr)

##Read and shape migration data from csvs downloaded from infoshare

migration_arrivals_new <- read_csv("data/mig_arrivals_2014-2019.csv")
migration_arrivals_old <- read_csv("data/mig_arrivals_2001-2014.csv")
migration_departures_new <- read_csv("data/mig_departures_2014-2019.csv")
migration_departures_old <- read_csv("data/mig_departures_2001-2014.csv")

migration_arrivals <- bind_rows(migration_arrivals_old,migration_arrivals_new)
migration_departures <- bind_rows(migration_departures_old,migration_departures_new)

migration_data <- bind_cols(migration_arrivals,migration_departures) %>%
                      select(YearMonth, Arrivals, Departures) %>%
                          gather(direction, long_term, Arrivals, Departures)

##Read and shape total border-crossing data from csvs downloaded from infoshare

total_arrivals <- read_csv("data/total_arrivals_2001-2019.csv")
total_departures <- read_csv("data/total_departures_2001-2019.csv")

totals_data <- bind_cols(total_arrivals,total_departures) %>% 
                  select(YearMonth, Arrivals = TotArrivals, Departures=TotDepartures) %>%
                      gather(direction, total, Arrivals, Departures)

##Compute short term border-crossing data and combine with the migration data

border_crossing_data <- bind_cols(migration_data,totals_data) %>%
  mutate(short_term = total - long_term) %>%
  select(YearMonth, direction, long_term, short_term) %>%
  gather(mig_status, count, long_term, short_term) %>%
  separate(col = YearMonth, into = c("Year", "Month"), sep = "M") %>%
  mutate(Month = as.integer(Month),
         Month = month.abb[Month]) %>%
  mutate(time = paste(Month, Year, sep = "-")) %>%
  select(time, direction, mig_status, count)
                                
##Write data to file

write_csv(border_crossing_data, "out/migration_data_2001_2019.csv")
saveRDS(border_crossing_data, "out/migration_data_2001_2019.rds")
                    


