# Data Input ----

air_reserve <- fread(input = "air_reserve.csv")
air_store   <- fread(input = "air_store_info.csv") # 150 tanesi id_relation içerisinde var.
air_store   <- air_store[, ':='(air_district1 = str_split_fixed(air_area_name, " ", n = 3)[,1],
                                air_district2 = str_split_fixed(air_area_name, " ", n = 3)[,2],
                                air_district3 = str_split_fixed(air_area_name, " ", n = 3)[,3])]
air_visit   <- fread(input = "air_visit_data.csv")
date        <- fread(input = "date_info.csv")
hpg_reserve <- fread(input = "hpg_reserve.csv")
hpg_store   <- fread(input = "hpg_store_info.csv") # 63 tanesi id_relation içerisinde var.
hpg_store   <- hpg_store[, ':='(hpg_district1 = str_split_fixed(hpg_area_name, " ", n = 3)[,1],
                                hpg_district2 = str_split_fixed(hpg_area_name, " ", n = 3)[,2],
                                hpg_district3 = str_split_fixed(hpg_area_name, " ", n = 3)[,3])]
id_relation <- fread(input = "store_id_relation.csv")
test        <- fread(input = "sample_submission.csv")
submission  <- copy(test)
test        <- test[, ':='(source = str_split_fixed(id, "_", n = 3)[,1],
                           id     = str_split_fixed(id, "_", n = 3)[,2],
                           date   = str_split_fixed(id, "_", n = 3)[,3])]
# Save -----
save.image(file = "RV_Forecasting.RData")
