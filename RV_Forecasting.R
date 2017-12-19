# Data Input ----
rm(list = ls()); gc(); dev.off() # Clearence
air_reserve <- fread(input = "csv_files/air_reserve.csv", encoding = "UTF-8")
air_store   <- fread(input = "csv_files/air_store_info.csv", encoding = "UTF-8") 
air_visit   <- fread(input = "csv_files/air_visit_data.csv", encoding = "UTF-8")
date        <- fread(input = "csv_files/date_info.csv", encoding = "UTF-8")
hpg_reserve <- fread(input = "csv_files/hpg_reserve.csv", encoding = "UTF-8")
hpg_store   <- fread(input = "csv_files/hpg_store_info.csv", encoding = "UTF-8") 
id_relation <- fread(input = "csv_files/store_id_relation.csv", encoding = "UTF-8")
test        <- fread(input = "sample_submission.csv", encoding = "UTF-8")
submission  <- copy(test)
weather     <- fread(input = "weather_data.csv", encoding = "UTF-8")

# Data Manipulation / Mungling ----
air_reserve <- air_reserve[, .(air_store_id,
                               visit_date   = as.Date(str_split_fixed(visit_datetime, pattern = " ", n = 2)[, 1]),
                               visit_hour   = as.numeric(substr(str_split_fixed(visit_datetime, pattern = " ", n = 2)[, 2], 1, 2)),
                               reserve_date = as.Date(str_split_fixed(reserve_datetime, pattern = " ", n = 2)[, 1]),
                               reserve_hour = as.numeric(substr(str_split_fixed(reserve_datetime, pattern = " ", n = 2)[, 2], 1, 2)),
                               visitors     = reserve_visitors
                               )]

air_store[, ':='(air_district1 = str_split_fixed(air_area_name, " ", n = 3)[,1],
                 air_district2 = str_split_fixed(air_area_name, " ", n = 3)[,2],
                 air_district3 = str_split_fixed(air_area_name, " ", n = 3)[,3]
                )]
air_store[, air_area_name := NULL]

air_visit[, visit_date := as.Date(visit_date)]

date        <- date[, .(date = as.Date(calendar_date), day  = as.factor(day_of_week), holiday_flg)]

hpg_reserve <- hpg_reserve[, .(hpg_store_id,
                               visit_date   = as.Date(str_split_fixed(visit_datetime, pattern = " ", n = 2)[, 1]),
                               visit_hour   = as.numeric(substr(str_split_fixed(visit_datetime, pattern = " ", n = 2)[, 2], 1, 2)),
                               reserve_date = as.Date(str_split_fixed(reserve_datetime, pattern = " ", n = 2)[, 1]),
                               reserve_hour = as.numeric(substr(str_split_fixed(reserve_datetime, pattern = " ", n = 2)[, 2], 1, 2)),
                               visitors     = reserve_visitors
                              )]

hpg_store   <- hpg_store[, ':='(hpg_district1 = str_split_fixed(hpg_area_name, " ", n = 3)[,1],
                                hpg_district2 = str_split_fixed(hpg_area_name, " ", n = 3)[,2],
                                hpg_district3 = str_split_fixed(hpg_area_name, " ", n = 3)[,3]
                                )]
hpg_store[, hpg_area_name := NULL]

test        <- test[, .(air_store_id = paste0(str_split_fixed(id, "_", n = 3)[,1], "_", str_split_fixed(id, "_", n = 3)[,2]),
                        date   = as.Date(str_split_fixed(id, "_", n = 3)[,3]),
                        visitors)] # visitors column is full of zeros since there has been no forecast all along.
weather     <- weather[, date := as.Date(date)]

# Adding data from hpg_stores which match with air_stores
air_reserve <- rbind(air_reserve,
                     cbind(merge(hpg_reserve, id_relation, by = "hpg_store_id")[, 7], 
                           merge(hpg_reserve, id_relation, by = "hpg_store_id")[, 2:6]))
hpg_reserve <- hpg_reserve[!hpg_store_id %in% id_relation$hpg_store_id]

# Data Manipulation / Genre Matching ----
air_store   <- merge(air_store,
                     fread(input = "air_genre_specs.csv"),
                     by = "air_genre_name")
hpg_store   <- merge(hpg_store,
                     fread(input = "hpg_genre_specs.csv"),
                     by = "hpg_genre_name")

# Data Manipulation / Daily Reservation Data ----
air_reserve[, PreTime := as.numeric(visit_date - reserve_date) * 24 + visit_hour - reserve_hour]
air_reserve[visit_hour < 8,                      ':='(activity_date = visit_date, period = "night")]
air_reserve[visit_hour >= 8  & visit_hour <= 13, ':='(activity_date = visit_date, period = "noon")]
air_reserve[visit_hour >= 14 & visit_hour <= 16, ':='(activity_date = visit_date, period = "afternoon")]
air_reserve[visit_hour >= 17 & visit_hour <= 21, ':='(activity_date = visit_date, period = "evening")]
air_reserve[visit_hour > 21,                     ':='(activity_date = visit_date + 1, period = "night")]

air_daily   <- dcast.data.table(air_reserve, activity_date + air_store_id ~ period, fun.aggregate = sum, value.var = "visitors")
air_daily[, Total := afternoon + evening + night + noon]
# Data Manipulation / Merges -----
air_visit   <- merge(air_visit, 
                     air_daily, 
                     by.x = c("air_store_id", "visit_date"), by.y = c("air_store_id", "activity_date"), all = T)
air_visit   <- merge(air_visit,
                     date[, .(visit_date = date,
                              visit_day  = day, 
                              visit_holiday = holiday_flg)],
                     by = "visit_date", all.x = T)
air_visit   <- merge(air_visit,
                     air_store[, c(2,1,5,6,7,8,9,10,11,12)],
                     by = "air_store_id", all.x = T)
air_visit   <- merge(air_visit,
                     weather,
                     by.x = c("air_district1", "visit_date"), by.y = c("air_district1", "date"), all.x = T)

# Save -----
save.image(file = "RV_Forecasting.RData")

# Data Manipulation / Creating Test & Train Datasets ----
library("forecast")
library("corrplot")
train       <- air_visit[visit_date < "2017-04-23" & 
                           air_store_id %in% test$air_store_id]
train[, ':='(air_district1  = as.factor(air_district1),
             air_district2  = as.factor(air_district2),
             air_store_id   = as.factor(air_store_id),
             air_genre_name = as.factor(air_genre_name))]

ts <- auto.arima(y = train$visit_date, x = train$visitors, xreg = train[, c(11, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)])
pn <- plm(formula = visitors ~ Total + visit_holiday + rain_amount + Dining + Bar + Party + Cafe + Local + avg_temp + snowfall, 
                data = train, index = c("air_store_id","visit_date"), model = "within")
ls <- step(lm(formula = visitors ~ ., data = train), direction = "backward")
logls <- step(lm(formula = log(visitors) ~ ., data = train), direction = "backward")
save(ts, pn, ls, logls, file = "models.RData")

test        <- merge(test, date, by = "date")
test        <- merge(test,
                     air_store[, c(2,1,5,6,7,8,9,10,11,12)],
                     by = "air_store_id", all.x = T)
test        <- merge(test,
                     weather,
                     by = c("air_district1", "date"), all.x = T)

# EDA / Some Basic Plots ----
corrs    <- Hmisc::rcorr(as.matrix(air_visit[, -c(1,2,3,10,12,13,14)]), type = "spearman")$r
cor_sigs <- Hmisc::rcorr(as.matrix(air_visit[, -c(1,2,3,10,12,13,14)]), type = "spearman")$P

col3 <- colorRampPalette(c("red", "white", "blue"))

dev.off();

jpeg(filename = "Korelasyon Matrisi.jpeg", width = 1024, height = 768)
corrplot(corrs, type = "upper", p.mat = cor_sigs, order = "hclust", addrect = 2,
         sig.level = 0.05, tl.col = "black", col = col3(20), cl.pos = "b", tl.srt = 45, 
         title = "Iliski Matrisi" )
dev.off()

jpeg(filename = "Visit Hour Examination.jpeg", width = 1024, height = 768)
ggplot(data = air_reserve[visit_date < "2017-04-23", .(visitors = sum(visitors, na.rm = T)), .(visit_hour)],
       aes(x = visit_hour, y = visitors)) +
  geom_line() + geom_facets()
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

