rm(list=ls())

library(lubridate)
library(dplyr)
library(haven)
library(caret)



data = read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/minutes_file.sas7bdat")
data2 = read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/pat_w1w2.sas7bdat")


colnames(data2)[1] = 'patcid'
pool = data %>% inner_join(data2, data, by='patcid')

head(pool)



pool2 = pool %>%
  mutate(timeofday = lubridate::hms(timeofday)) %>%
  filter(timeofday >= lubridate::hms('08:00:00.000000') & 
           timeofday <= lubridate::hms('21:00:00.000000'))

missing.df = pool2 %>% filter (is.na(count))
pool2 = pool2 %>% filter (!is.na(count))

pool2$sample_id = 1:nrow(pool2)
sample.df = pool2 %>% group_by(patcid) %>% sample_frac(0.05) ### use the column names for missing.df

sample.df = sample.df %>% dplyr::select (1, 13, 2:4, 6:7, 3, 17, 23:207)
sample.df = sample.df %>%  select_if(function(col) !any(is.na(col))) ### use the column names for missing.df

missing.df = missing.df %>% dplyr::select (names(sample.df))

preprocess_params1 = preProcess(missing.df[,-c(1:2)], method = c("center", "scale"))
test_data_scaled_missing = predict(preprocess_params1, missing.df)

mode.sedmin = readRDS('C:/Users/kai_kasados/Desktop/UM Fall 2023/625/Final Project/model_sedmin_full.RDS')
predictions = predict(mode.sedmin, newdata = as.matrix(test_data_scaled_missing[,-c(1,2)]))
predicted_classes = ifelse(predictions > 0.5, 'One', 'Zero')
sed_min_imp = predicted_classes



############################# imputed count

pool2 = pool %>%
  mutate(timeofday = lubridate::hms(timeofday)) %>%
  filter(timeofday >= lubridate::hms('08:00:00.000000') & 
           timeofday <= lubridate::hms('21:00:00.000000'))

missing.df = pool2 %>% filter (is.na(count))
pool2 = pool2 %>% filter (!is.na(count))

pool2$sample_id = 1:nrow(pool2)
sample.df = pool2 %>% group_by(patcid) %>% sample_frac(0.05) ### use the column names for missing.df

sample.df = sample.df %>% dplyr::select (1, 8, 2:4, 6:7, 3, 17, 23:207)
sample.df = sample.df %>%  select_if(function(col) !any(is.na(col)))

missing.df = missing.df %>% dplyr::select (names(sample.df))



preprocess_params1 = preProcess(missing.df[,-c(1:2)], method = c("center", "scale"))
test_data_scaled_missing = predict(preprocess_params1, missing.df)

model.count = readRDS('C:/Users/kai_kasados/Desktop/UM Fall 2023/625/Final Project/model_count_full.RDS')
predictions = predict(model.count, newdata = as.matrix(test_data_scaled_missing[,-c(1,2)]))
count_min_imp = predictions







############################################################################# combining the results together
pool3 = missing.df %>% select(patcid, minuteid,
                              hourid, dayid, timeofday, month, dow,
                              count, ped, boutmin, vigboutmin, worn, sed_min,
                              light_min, mod_min, vig_min, weartime) %>% mutate (imp_flag = 1,
                                                                                 count_min_imp = count_min_imp,
                                                                                 sed_min_imp = sed_min_imp)




pool2 = pool2 %>% select(patcid, minuteid,
                         hourid, dayid, timeofday, month, dow,
                         count, ped, boutmin, vigboutmin, worn, sed_min,
                         light_min, mod_min, vig_min, weartime) %>% mutate (imp_flag = 0, 
                                                                            count_min_imp = count,
                                                                            sed_min_imp = sed_min)





df = rbind(pool2, pool3)

df$sed_min_calc = ifelse(df$count_min_imp<100, 1, 0)  
df$sed_min_imp = ifelse(df$sed_min_imp=='1', 1, 0)  




######################################################################################### results
df = df %>% group_by(patcid) %>% summarise(sed_min_ml_count = sum(sed_min_calc),
                                           sed_min_ml = sum(sed_min_imp),
                                              count_imp = mean(count_min_imp),
                                              count_imp_total = sum(count_min_imp))



library(openxlsx)
write.xlsx(df, 'xgboost.df.xlsx')
rm(list=ls())
gc()
