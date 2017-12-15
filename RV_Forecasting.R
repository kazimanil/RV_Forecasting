# Data Input ----
rm(list = ls()); gc(); dev.off() # Clearence
air_reserve <- fread(input = "csv_files/air_reserve.csv", encoding = "UTF-8")
air_store   <- fread(input = "csv_files/air_store_info.csv", encoding = "UTF-8") # 150 tanesi id_relation içerisinde var.
air_visit   <- fread(input = "csv_files/air_visit_data.csv", encoding = "UTF-8")
date        <- fread(input = "csv_files/date_info.csv", encoding = "UTF-8")
hpg_reserve <- fread(input = "csv_files/hpg_reserve.csv", encoding = "UTF-8")
hpg_store   <- fread(input = "csv_files/hpg_store_info.csv", encoding = "UTF-8") # 63 tanesi id_relation içerisinde var.
id_relation <- fread(input = "csv_files/store_id_relation.csv", encoding = "UTF-8")
test        <- fread(input = "sample_submission.csv", encoding = "UTF-8")
submission  <- copy(test)

# Data Manipulation / Mungling ----
air_reserve <- air_reserve[, .(air_store_id,
                               visit_datetime   = as.POSIXct(visit_datetime),
                               visit_date       = as.Date(visit_datetime),
                               visit_hour       = hour(as.POSIXct(visit_datetime)),
                               reserve_datetime = as.POSIXct(reserve_datetime),
                               reserve_date     = as.Date(reserve_datetime),
                               reserve_hour     = hour(as.POSIXct(reserve_datetime)),
                               visitors         = reserve_visitors
                               )]

air_store[, ':='(air_district1 = as.factor(str_split_fixed(air_area_name, " ", n = 3)[,1]),
                 air_district2 = as.factor(str_split_fixed(air_area_name, " ", n = 3)[,2]),
                 air_district3 = as.factor(str_split_fixed(air_area_name, " ", n = 3)[,3])
                 )]

air_visit[, visit_date := as.Date(visit_date)]

date        <- date[, .(date = as.Date(calendar_date), day  = as.factor(day_of_week), holiday_flg)]

hpg_reserve <- hpg_reserve[, .(hpg_store_id,
                               visit_datetime   = as.POSIXct(visit_datetime),
                               visit_date       = as.Date(visit_datetime),
                               visit_hour       = hour(as.POSIXct(visit_datetime)),
                               reserve_datetime = as.POSIXct(reserve_datetime),
                               reserve_date     = as.Date(reserve_datetime),
                               reserve_hour     = hour(as.POSIXct(reserve_datetime)),
                               visitors         = reserve_visitors
                              )]

hpg_store   <- hpg_store[, ':='(hpg_district1 = as.factor(str_split_fixed(hpg_area_name, " ", n = 3)[,1]),
                                hpg_district2 = as.factor(str_split_fixed(hpg_area_name, " ", n = 3)[,2]),
                                hpg_district3 = as.factor(str_split_fixed(hpg_area_name, " ", n = 3)[,3])
                                )]

# Note that, if two groups have different factor levels then these two variables will be useless.
air_store[, air_genre_name := as.factor(air_genre_name)] # May be re-grouped later on. There are small groups and discrepancies with hpg set.
hpg_store[, hpg_genre_name := as.factor(hpg_genre_name)] # May be re-grouped later on. There are small groups and discrepancies with air set.

test        <- test[, .(source = str_split_fixed(id, "_", n = 3)[,1],
                        id     = str_split_fixed(id, "_", n = 3)[,2],
                        date   = str_split_fixed(id, "_", n = 3)[,3],
                        visitors)] # visitors column is full of zeros since there has been no forecast all along.

# Data Manipulation / Merges -----
air_reserve <- merge(air_reserve,
                     date[, .(visit_date = date,
                              visit_day  = day, 
                              visit_holiday = holiday_flg)],
                     by = "visit_date", all.x = T)
air_reserve <- merge(air_reserve,
                     date[, .(reserve_date = date,
                              reserve_day  = day, 
                              reserve_holiday = holiday_flg)],
                     by = "reserve_date", all.x = T)
hpg_reserve <- merge(hpg_reserve,
                     date[, .(visit_date = date,
                              visit_day  = day, 
                              visit_holiday = holiday_flg)],
                     by = "visit_date", all.x = T)
hpg_reserve <- merge(hpg_reserve,
                     date[, .(reserve_date = date,
                              reserve_day  = day, 
                              reserve_holiday = holiday_flg)],
                     by = "reserve_date", all.x = T)
air_visit   <- merge(air_visit,
                     date[, .(visit_date = date,
                              visit_day  = day, 
                              visit_holiday = holiday_flg)],
                     by = "visit_date", all.x = T)
id_relation <- merge(id_relation,
                     air_store,
                     by = "air_store_id")
id_relation <- merge(id_relation,
                     hpg_store,
                     by = "hpg_store_id",
                     all.x = T)
# Data Manipulation / Aggregation ----
air_res_daily <- air_reserve[, .(daily_visitors = sum(visitors))
                             , .(visit_date, visit_day, visit_holiday)]

# EDA / Some Basic Plots ----
ggplot(data = air_res_daily, aes(x = visit_date, y = daily_visitors)) +
  geom_line()

# Save -----
save.image(file = "RV_Forecasting.RData")
