# Complete Case Approach
# Linear Interpolation
# Multiple Imputation for activity count with time series features

library(heatmap3)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(haven)
library(lattice)
library(lubridate)
library(caret)
library(zoo)
library(MASS)
library(mi)

###  Data processing ###
colnames(pat_svy)<-tolower(colnames(pat_svy))
survey_select <- pat_svy[,c("patcid", "agegroup", "dem3", "dem11", "dem12educ","dem14", "status6", "povertygroup", "bmicat4all", "tobacco1", "alcohol1", "gpaq23daysedmin", "newrace")]
colnames(survey_select) <- tolower(colnames(survey_select))
survey_select <- survey_select %>%
  mutate_all(~ ifelse(grepl("\\.d$|\\.r$|\\.v$", .), "missing", .)) %>%
  mutate_all(~ ifelse(is.na(.), "missing", .))
# add a column "hour" to indicate the hour of the day
pat$hour <- hour(pat$timeofday)
# keep rows with hour between 8 and 20 from pat_not_worn
pat_daytime <- pat[pat$hour >= 8 & pat$hour < 22, ] # up until 21:59
str(pat_daytime)

pat_daytime$timestamp <-
  as.POSIXct(paste(
    as.Date("2011-01-01") + pat_daytime$dayid - 1,
    pat_daytime$timeofday),
    format = "%Y-%m-%d %H:%M:%S")
pat_daytime_select <-
  pat_daytime[, c("patcid", "timestamp", "count", "sed_min")]

survey_select$patcid <- as.numeric(survey_select$patcid)
merge_df <-
  merge(pat_daytime_select,
        survey_select,
        by = "patcid",
        all.x = TRUE)

merge_df$day <- format(merge_df$timestamp, "%D")
merge_df$hour <- format(merge_df$timestamp, "%H")
merge_df$min  <- format(merge_df$timestamp, "%M")

merge_df$lagged_count <- na.locf(merge_df$count, fromLast=F, na.rm=F)  #lagged variable
merge_df$lagged_count <-ifelse(is.na(merge_df$lagged_count), 0, merge_df$lagged_count )
merge_df$patcid <- as.factor(merge_df$patcid)

merge_df_all <- merge_df[,c("patcid", "count", "lagged_count", "agegroup", "dem3", "dem11", "dem12educ","sed_min",
                           "dem14", "status6", "povertygroup", "bmicat4all", "tobacco1", "alcohol1", "gpaq23daysedmin", "hour", "newrace")]



### Data processing a new df to store aggregated (individual-level) sed_min generated from differnt appraoches ### 
result_df <-data.frame()


# Complete Case Approach
result_complete_cases <- merge_df_all %>%
  group_by(patcid) %>%
  summarise(total_sed_min = sum(sed_min, na.rm = TRUE)) %>%
  summarise(mean_sed_min = mean(total_sed_min, na.rm = TRUE),
            var_sed_min = var(total_sed_min, na.rm = TRUE))
print(result_complete_cases)
# save the reuslts
result_df<-as.data.frame(merge_df_all %>% group_by(patcid) %>% summarise(sed_min_complete = sum(sed_min, na.rm = TRUE)))


# Linear Interpolation
merge_df_all$count_intpl<- na.approx(merge_df_all$count, rule=2)

merge_df_all$sed_min_intpl<-ifelse(merge_df_all$count_intpl<100, 1, 0)

result_intpl <- merge_df_all %>%
  group_by(patcid) %>%
  summarise(total_sed_min = sum(sed_min_intpl, na.rm = TRUE)) %>%
  summarise(mean_sed_min = mean(total_sed_min, na.rm = TRUE),
            var_sed_min = var(total_sed_min, na.rm = TRUE))
print(result_intpl)
# save the reuslts
result_df<-left_join(result_df, as.data.frame(merge_df_all %>% group_by(patcid) %>% summarise(sed_min_intpl = sum(sed_min_intpl, na.rm = TRUE))), by ='patcid')


# Multiple Imputation for activity count with time series features
merge_df_all$count_sqrt<-merge_df_all$count^(1/2)
merge_df_all$lagged_count_sqrt<-merge_df_all$lagged_count^(1/2)
merge_df_all$agegroup<-as.factor(ifelse(merge_df_all$agegroup=='missing', 0, merge_df_all$agegroup))
merge_df_all$dem3<-as.factor(ifelse(merge_df_all$dem3=='missing', 0, merge_df_all$dem3))
merge_df_all$status6<-as.factor(ifelse(merge_df_all$status6=='missing', 0, merge_df_all$status6))
merge_df_all$bmicat4all<-as.factor(ifelse(merge_df_all$bmicat4all=='missing', 0, merge_df_all$bmicat4all))
merge_df_all$hour<-as.factor(ifelse(merge_df_all$hour=='missing', 0, merge_df_all$hour))
merge_df_all$newrace<-as.factor(ifelse(merge_df_all$newrace=='missing', 0, merge_df_all$newrace))
merge_df_all$povertygroup<-as.factor(ifelse(merge_df_all$povertygroup=='missing', 0, merge_df_all$povertygroup))

set.seed(123)
selected_columns<-merge_df_all[,c("count_sqrt", "lagged_count_sqrt" , "agegroup", "dem3", "newrace", "povertygroup", "status6", "bmicat4all", "hour")]
#selected_columns<-selected_columns%>% slice(seq(1, nrow(selected_columns), by = 100)) #comment out when running.
mdf <- missing_data.frame(selected_columns) # warnings about missingness patterns
# Display information about missing data patterns
show(mdf)
# change things
mdf <- change(mdf, y = "count_sqrt", what = "transformation", to = "identity") #non-negative continuous
imp<-mi(selected_columns,n.iter=10, n.chains=5)

imputed_all<-NULL
for (i in 1:5){
  temp_df <-complete(imp, 5)[[i]]%>%
    mutate(count = count_sqrt^2, lagged_count = lagged_count_sqrt^2,mult = i) 
  imputed_all<-rbind(imputed_all, temp_df)
}
obs_mean<-mean(imputed_all[imputed_all$missing_count_sqrt==F,]$count)
obs_var<-var(imputed_all[imputed_all$missing_count_sqrt==F,]$count)
imp_mean<-mean(imputed_all[imputed_all$missing_count_sqrt==T,]$count)
imp_var<-var(imputed_all[imputed_all$missing_count_sqrt==T,]$count)

cat("Observed activity count: mean", obs_mean, "variance ", obs_var, "\n")
cat("Imputed activity count: mean", imp_mean, "variance ", imp_var, "\n")

imputed_sample<-imputed_all[seq(1, nrow(imputed_all), by=100),,drop=F]
# plot the imputed and observed activity count
ggplot(imputed_sample, aes(x = seq_along(count), y = count, color = ifelse(missing_count_sqrt == FALSE, 'Observed', 'Imputed'))) +
  geom_point() +
  scale_color_manual(values = c('Observed' = 'slategray1', 'Imputed' = 'orange')) +
  labs(x = "Index of Minute Observations", y = "Activity Count", color = "Data") +
  theme(text = element_text(size = 16))
ggsave("BIOSTAT625-Project/results/figures/count_obs_imp.png", width = 9, height = 5, dpi = 600)

# Determine whether a minute is sedentary minute
imputed_all$mult<-rep(1:5, each=3997273)
combined<-data.frame(patcid=merge_df_all$patcid)
combined$count_1<-imputed_all[imputed_all$mult==1,]$count
combined$count_2<-imputed_all[imputed_all$mult==2,]$count
combined$count_3<-imputed_all[imputed_all$mult==3,]$count
combined$count_4<-imputed_all[imputed_all$mult==4,]$count
combined$count_5<-imputed_all[imputed_all$mult==5,]$count
combined$avg_count<-rowMeans(combined[,2:6])
merge_df_all$sed_min_multmi<-ifelse(combined$avg_count<100, 1, 0)

result_multmi <- merge_df_all %>%
  group_by(patcid) %>%
  summarise(total_sed_min = sum(sed_min_multmi, na.rm = TRUE)) %>%
  summarise(mean_sed_min = mean(total_sed_min, na.rm = TRUE),
            var_sed_min = var(total_sed_min, na.rm = TRUE))
print(result_multmi)

result_df<-left_join(result_df, as.data.frame(merge_df_all %>% group_by(patcid) %>% summarise(sed_min_multmi= sum(sed_min_multmi, na.rm = TRUE))), by ='patcid')