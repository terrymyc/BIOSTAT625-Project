rm(list=ls())

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

data$flag_imp = ifelse(is.na(data$count), 1, 0)
table(data$flag_imp)


####################################### arima with na kalman for all patcid
data$count_imputed1 = na_kalman(data$count, model = "auto.arima", smooth = TRUE, nit = -1)
data$sed_min_imputed1 = na_kalman(data$sed_min, model = "auto.arima", smooth = TRUE, nit = -1)



####################################### arima with na kalman
arima_impute = function(data) {
  data$count_imputed3 = na_kalman(data$count, model = "auto.arima", smooth = TRUE, nit = -1)
  return(data)
}
####################################### imputing for each patcid separately


data = data %>%
  group_by(patcid) %>%
  do(arima_impute(.)) %>%
  ungroup() %>% filter(!patcid==0)


data <- data %>%
  mutate(
    count_imputed3 = case_when(
      count_imputed3 < 0 ~ 0,
      TRUE ~ count_imputed3
    )
  )

pool=data
data = data %>%
   mutate (count_imp_all = case_when(
          flag_imp == 1 ~ count_imputed1,
          TRUE ~ count
   ), 
    count_impby_group = case_when(
      flag_imp == 1 ~ count_imputed3,
      TRUE ~ count
    ),
     sed_min_all = case_when(
       count_imputed1 > 0 ~ 1,
       TRUE ~ 0
     ),
   sed_min_bygr = case_when(
     count_imputed3 > 0 ~ 1,
     TRUE ~ 0))

data$sed_min_all = ifelse(data$count_imp_all<100, 1, 0)
data$sed_min_bygr = ifelse(data$count_impby_group<100, 1,0)

final_results = data %>% group_by(patcid) %>% summarise(sed_min_all = sum(sed_min_all),
                                                sed_min_bygr = sum(sed_min_bygr),
                                                sed_min_arima_all = mean(count_imp_all),
                                                sed_min_arima = mean(count_impby_group) )

library(openxlsx)
write.xlsx(final_results, 'pool2_imp.xlsx')


