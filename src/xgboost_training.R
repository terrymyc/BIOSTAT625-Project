rm(list=ls())

library(lubridate)
library(hms)
library(dplyr)
library(haven)


data = read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/minutes_file.sas7bdat")
data2 = read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/pat_w1w2.sas7bdat")

colnames(data2)[1] = 'patcid'
pool = data %>% inner_join(data2, data, by='patcid')

head(pool)

###################################################################### selecting the day time
pool2 = pool %>%
  mutate(timeofday = lubridate::hms(timeofday)) %>%
  filter(timeofday >= lubridate::hms('08:00:00.000000') & 
           timeofday <= lubridate::hms('21:00:00.000000'))

##################################################################### complete/missing
missing.df = pool2 %>% filter (is.na(count))
pool2 = pool2 %>% filter (!is.na(count))

rm(list=c('data', 'data2', 'pool')) # no need in these data sets
gc() # clear the memory

set.seed(12)
pool2$sample_id = 1:nrow(pool2)
sample.df = pool2 %>% group_by(patcid) %>% sample_frac(0.7) # test data
opposite_data = anti_join(pool2, sample.df, by = "sample_id") # train data



############################################################# training using count variable
sample.df = sample.df %>% dplyr::select (8, 2:4, 6:7, 3, 17, 23:207)
sample.df = sample.df %>%  select_if(function(col) !any(is.na(col))) ### only covariates with complete cases

opposite_data = opposite_data %>% dplyr::select (8, 2:4, 6:7, 3, 17, 23:207)
opposite_data = opposite_data %>%  select_if(function(col) !any(is.na(col))) ### only covariates with complete cases
#############################################################

sample.df = sample.df[,-1] # no need in patcid

preprocess_params1 = preProcess(sample.df[,-c(1)], method = c("center", "scale")) ### scaling
preprocess_params2 = preProcess(opposite_data[,-c(1)], method = c("center", "scale")) ### scaling



train_data_scaled = predict(preprocess_params1, sample.df) # preprocessing train
test_data_scaled = predict(preprocess_params2, opposite_data) # preprocessing test 


##################################################################################### parameters
params = list(eta = 0.01, gamma = 0.001,  max_depth = 10, colsample_bytree = 0.75, min_child_weight = 0,
              subsample = c(0.5), nthread = 16,  objective = "reg:squarederror")


y = train_data_scaled[,1] # labels
x = as.matrix(train_data_scaled[,-1]) # covariates


library(xgboost)
model_xg = xgboost(data=x, nrounds=10000, label=y$count, params = params) #### nrounds - critical to improve the training
predictions = predict(model_xg, newdata = as.matrix(test_data_scaled[,-1]))
r_squared = caret::R2(predictions, test_data_scaled[,1])
r_squared
## 0.47 - can be different for another iteration of training
saveRDS(model_xg, 'C:/Users/kai_kasados/Desktop/UM Fall 2023/625/Final Project/model_count_full.RDS')
####################################################################################





#################################################################################### training using sed_min variable
set.seed(12)
pool2$sample_id = 1:nrow(pool2)
sample.df = pool2 %>% group_by(patcid) %>% sample_frac(0.7) # test data
opposite_data = anti_join(pool2, sample.df, by = "sample_id") # train data


sample.df = sample.df %>% dplyr::select (13, 2:4, 6:7, 3, 17, 23:207)
sample.df = sample.df %>%  select_if(function(col) !any(is.na(col)))

opposite_data = opposite_data %>% dplyr::select (13, 2:4, 6:7, 3, 17, 23:207)
opposite_data = opposite_data %>%  select_if(function(col) !any(is.na(col))) ### only covariates with complete cases



sample.df = sample.df[,-1] # no need in patcid

preprocess_params1 = preProcess(sample.df[,-c(1)], method = c("center", "scale")) ### scaling
preprocess_params2 = preProcess(opposite_data[,-c(1)], method = c("center", "scale")) ### scaling

train_data_scaled = predict(preprocess_params1, sample.df) # preprocessing train
test_data_scaled = predict(preprocess_params2, opposite_data) # preprocessing test 

test_data_scaled$sed_min = ifelse(test_data_scaled$sed_min == 1, 'One', 'Zero')
train_data_scaled$sed_min = ifelse(train_data_scaled$sed_min == 1, 'One', 'Zero')
#############################################################

##############################################################
params = list(eta = 0.01, gamma = 0.001,  max_depth = 10, colsample_bytree = 0.75, min_child_weight = 0,
              subsample = c(0.5), nthread = 16,  objective = "binary:logistic", eval_metric = "auc" )

train_data_scaled = train_data_scaled[,-1]
y = train_data_scaled[,1]
y$sed_min = ifelse(y$sed_min == 'One', 1, 0)
x = as.matrix(train_data_scaled[,-1])

library(xgboost)
model_xg2 = xgboost(data=x, nrounds=10000, label=y$sed_min, params = params)
predictions = predict(model_xg, newdata = as.matrix(test_data_scaled[,-1]))
y = train_data_scaled[,1]
y$sed_min = ifelse(y$sed_min == 'One', 1, 0)
X = as.matrix(test_data_scaled[,-c(1)])
predictions = predict(model_xg2, newdata = X)
predicted_classes = ifelse(predictions > 0.5, 'One', 'Zero')
confusionMatrix(data = as.factor(predicted_classes), reference = as.factor(test_data_scaled$sed_min))

############################## results from the model training
#Reference
#Prediction    One   Zero
#One  482752 179361
#Zero  71041 179129

#Accuracy : 0.7255          
#95% CI : (0.7246, 0.7264)
#No Information Rate : 0.607           
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.3923          

#Mcnemar's Test P-Value : < 2.2e-16       

#            Sensitivity : 0.8717          
#            Specificity : 0.4997          
#         Pos Pred Value : 0.7291          
#         Neg Pred Value : 0.7160          
#             Prevalence : 0.6070          
#         Detection Rate : 0.5292          
#   Detection Prevalence : 0.7258          
#      Balanced Accuracy : 0.6857          

#       'Positive' Class : One  

saveRDS(model_xg2, 'C:/Users/kai_kasados/Desktop/UM Fall 2023/625/Final Project/model_sedmin_full.RDS')
###############################################################
