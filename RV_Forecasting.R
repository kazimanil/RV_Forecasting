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
weather     <- fread(input = "japan/weather_data.csv")

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
weather     <- weather[, date := as.Date(date)]

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
air         <- merge(air_reserve,
                     air_store,
                     by = "air_store_id",all.x = T)
air_visit   <- merge(air_visit,
                     air_store,
                     by = "air_store_id",all.x = T)
air[, PreTime := as.numeric(difftime(visit_datetime, reserve_datetime, units = "hours"))]
rm(air_reserve)

# Save -----
save.image(file = "RV_Forecasting.RData")

# Data Manipulation / Aggregation ----
air_daily <- merge( air[, .(daily_visitors = sum(visitors)), .(air_store_id, date = visit_date)],
                    air[, .(daily_reserves = sum(visitors)), .(air_store_id, date = reserve_date)],
                    by = c("air_store_id", "date"), all.x = T, all.y = T)
air_daily[is.na(daily_visitors), daily_visitors := 0]
air_daily[is.na(daily_reserves), daily_reserves := 0]

air[, air_area_name := NULL]
air_visit[, air_area_name := NULL]
air_store[, air_area_name := NULL]
hpg_store[, hpg_area_name := NULL]
hpg_reserve[, hpg_area_name := NULL]

air_visit <- merge(air_visit,
                   air_daily,
                   all.x = T, all.y = T, 
                   by.x = c("air_store_id", "visit_date"), by.y = c("air_store_id", "date"))

# PreTime defines the mean hourly time interval between reservation and visit.
air_store <- merge(air_store,
                   air[, .(PreTime = mean(as.numeric(difftime(visit_datetime, reserve_datetime, units = "hours"
                          )))), .(air_store_id)],
                   by = "air_store_id", all.x = T)
rm(air_daily)

# I will create a training set compromising of lines which has a visit_date before 23-04-2017 
# since it is the start of test / submission set.

# EDA / Some Basic Plots ----
jpeg(filename = "Visit Hour Examination.jpeg", width = 1024, height = 768)
ggplot(data = air[visit_date < "2017-04-23", .(visitors = sum(visitors, na.rm = T)), .(visit_hour)],
       aes(x = visit_hour, y = visitors)) +
  geom_line()
dev.off()

jpeg(filename = "Reservation Hour Examination.jpeg", width = 1024, height = 768)
ggplot(data = air[visit_date < "2017-04-23", .(Reservations = sum(visitors, na.rm = T)), .(reserve_hour)],
       aes(x = reserve_hour, y = Reservations)) +
  geom_line(colour = "red")
dev.off()

jpeg(filename = "Visit Hour - Reservation Hour Examination.jpeg", width = 1024, height = 768)
ggplot(data = air[visit_date < "2017-04-23", .(Reservations = sum(visitors, na.rm = T)), .(reserve_hour)],
       aes(x = reserve_hour, y = Reservations)) + geom_line(colour = "red") +
  geom_line(data = air[visit_date < "2017-04-23", .(visitors = sum(visitors, na.rm = T)), .(visit_hour)],
            aes(x = visit_hour, y = visitors)) +
  xlab("Hour") + ylab("Number of Visitors") + labs(title = "Red for Reservations, Black for Visits")
dev.off()

jpeg(filename = "Reservation - Visit Gap (Aggregated).jpeg", width = 1024, height = 768)
ggplot(data = air_store[!is.na(PreTime), PreTime], aes(x = PreTime)) +
  geom_density() + theme_minimal() +
  labs(title = "Mean time between reservation and visit (Hour)")
dev.off()

# Most of the reservations are to visit the place within 24 hours.
jpeg(filename = "Reservation - Visit Gap.jpeg", width = 1024, height = 768)
ggplot(data = air[PreTime < 720], aes(x = PreTime)) +
  geom_histogram(binwidth = 24) + theme_minimal() +
labs(title = "Time between reservation and visit (Hour)")
dev.off()


# Regression Models ----
sglm <- lm(formula = daily_visitors ~ ., data = air_vis_daily) # simple linear reg model for general demand.

