# Data Input ----

air_reserve <- fread(input = "air_reserve.csv")
air_store   <- fread(input = "air_store_info.csv") # 150 tanesi id_relation içerisinde var.
air_visit   <- fread(input = "air_visit_data.csv")
date        <- fread(input = "date_info.csv")
hpg_reserve <- fread(input = "hpg_reserve.csv")
hpg_store   <- fread(input = "hpg_store_info.csv") # 63 tanesi id_Relation içerisinde var.
id_relation <- fread(input = "store_id_relation.csv")

# Save -----
save.image(file = "RV_Forecasting.RData")
