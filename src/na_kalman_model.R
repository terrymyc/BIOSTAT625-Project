#rm(list=ls())

library(lubridate)
library(dplyr)
library(haven)
library(forecast)
library(imputeTS)


data = read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/minutes_file.sas7bdat")

data = data %>%
  mutate(timeofday = lubridate::hms(timeofday)) %>%
  filter(timeofday >= lubridate::hms('08:00:00.000000') & 
           timeofday <= lubridate::hms('21:00:00.000000'))

####################################### arima with na kalman
arima_impute = function(data) {
  data$count_imputed = na_kalman(data$count, model = "auto.arima", smooth = TRUE, nit = -1)
  return(data)
}
####################################### imputing for each patcid separately


data = data %>%
  group_by(patcid) %>%
  do(arima_impute(.)) %>%
  ungroup() %>% filter(!patcid==0)


data <- data %>%
  mutate(
    count_imputed = case_when(
      count_imputed < 0 ~ 0,
      TRUE ~ count_imputed
    )
  )


#data = as.data.frame(data) 
#data[which(data$count_imputed<0),] = 0
data$sed_min_new = ifelse(data$count_imputed > 0, 0, 1)

data2 = data %>% group_by(patcid) %>% summarise(sed_min_sum = sum(sed_min_new))
write.csv(data2, 'data_imputed_sed_min.csv')
write.csv(data, 'data_aceleremeter_imp_count.csv')
