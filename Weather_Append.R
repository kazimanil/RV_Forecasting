# change this directory with the directory of files.
setwd(dir = "kaggle/weather_data/")  
# change this directory with the directory of files.

rm(list = ls())
filelist <- list.files()
weatherdata <- as.data.table(NULL)
for(i in (1:length(filelist))){
  weatherdata <- rbind(weatherdata,
                       cbind(position = str_split_fixed(filelist[i], "[.]", 2)[[1]],
                             fread(input = paste0(filelist[i]))))
}
# change this directory with the directory you desire for saving.
fwrite(weatherdata, file = "C:/Users/kazimanil/Documents/R_Projects/RV_Forecasting/kaggle/weather_all.csv", )
# change this directory with the directory you desire for saving.
