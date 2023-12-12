library(dplyr)
library(ggplot2)
library(gridExtra)
library(haven)
library(lattice)

# ------------------------------------------------------------
# Read in data
# ------------------------------------------------------------
pat <- read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/minutes_file.sas7bdat")
pat_svy <- read_sas("https://www.nyc.gov/assets/doh/downloads/sas/episrv/pat_w1w2.sas7bdat")

# ------------------------------------------------------------
# Preprocess sensor data
# ------------------------------------------------------------

# add a column "hour" to indicate the hour of the day
pat$hour <- NA

# extract data with hourid = 1, 25, 49, 73, 97, 121, 145
pat$hour[pat$hourid %in% c(6, 30, 54, 78, 102, 126, 150)] <- 8
pat$hour[pat$hourid %in% c(7, 31, 55, 79, 103, 127, 151)] <- 9
pat$hour[pat$hourid %in% c(8, 32, 56, 80, 104, 128, 152)] <- 10
pat$hour[pat$hourid %in% c(9, 33, 57, 81, 105, 129, 153)] <- 11
pat$hour[pat$hourid %in% c(10, 34, 58, 82, 106, 130, 154)] <- 12
pat$hour[pat$hourid %in% c(11, 35, 59, 83, 107, 131, 155)] <- 13
pat$hour[pat$hourid %in% c(12, 36, 60, 84, 108, 132, 156)] <- 14
pat$hour[pat$hourid %in% c(13, 37, 61, 85, 109, 133, 157)] <- 15
pat$hour[pat$hourid %in% c(14, 38, 62, 86, 110, 134, 158)] <- 16
pat$hour[pat$hourid %in% c(15, 39, 63, 87, 111, 135, 159)] <- 17
pat$hour[pat$hourid %in% c(16, 40, 64, 88, 112, 136, 160)] <- 18
pat$hour[pat$hourid %in% c(17, 41, 65, 89, 113, 137, 161)] <- 19
pat$hour[pat$hourid %in% c(18, 42, 66, 90, 114, 138, 162)] <- 20
pat$hour[pat$hourid %in% c(19, 43, 67, 91, 115, 139, 163)] <- 21

# select rows and columns
pat_count <- pat[!is.na(pat$hour),
                 c("patcid", "minuteid", "month", "dow", "hour", "count")]
pat_binary <- pat[!is.na(pat$hour),
                  c("patcid", "minuteid", "month", "dow", "hour", "sed_min")]

# create a lagged variable "count_lag" to indicate the count of the previous minute
pat_count <- pat_count %>%
  group_by(patcid) %>%
  mutate(count_lag = lag(count, 1))

# create a lagged variable "sed_min_lag" to indicate the sedentary minutes of the previous minute
pat_binary <- pat_binary %>%
  group_by(patcid) %>%
  mutate(sed_min_lag = lag(sed_min, 1))

summary(pat_count)
summary(pat_binary)

# ------------------------------------------------------------
# Preprocess self-report data
# ------------------------------------------------------------
colnames(pat_svy)[1] <- "patcid"

# filter out participants having accelerometer data
id <- unique(pat$patcid)
pat_svy <- pat_svy[pat_svy$patcid %in% id, ]

# sed_min exceeds 840 considered as invalid
pat_svy$gpaqsedall[pat_svy$gpaqsedall > 840] <- NA

# select demographic variables
demo_var <- c("dem11", "dem12", "povertygroup", "newrace")
pat_svy_demo <- pat_svy[, c("patcid", demo_var)]
summary(pat_svy_demo)

# ------------------------------------------------------------
# Merge demographic variables and sensor data
# ------------------------------------------------------------

pat_count <- pat_count %>%
  left_join(pat_svy_demo, by = "patcid")
summary(pat_count)

pat_binary <- pat_binary %>%
  left_join(pat_svy_demo, by = "patcid")
summary(pat_binary)

pat_count$patcid <- as.integer(pat_count$patcid)
pat_binary$patcid <- as.integer(pat_binary$patcid)

# ------------------------------------------------------------
# Inspect missing patterns
# ------------------------------------------------------------

# how many minutes are missing per person
pat_count %>%
  group_by(patcid) %>%
  summarise(n_missing_min = sum(is.na(count))) %>%
  # ggplot to show the distribution with a line indicating the mean
  ggplot(aes(x = n_missing_min)) +
  geom_histogram(binwidth = 30) +
  geom_vline(aes(xintercept = mean(n_missing_min)), color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "Missing minutes between 8 a.m. and 10 p.m. over 7 days",
       y = "Number of participants") +
  theme(text = element_text(size = 16))
ggsave("BIOSTAT625-Project/results/figures/missing_min_pp.png", width = 9, height = 5, dpi = 600)

# density plot to show distributions of sed_min and gpaqsedall
pat_binary %>%
  # filter out id without valid sed_min
  filter(patcid %in% id) %>%
  group_by(patcid) %>%
  summarise(sed_min = sum(as.numeric(sed_min), na.rm = TRUE)/7) %>%
  # cbind with pat_svy to get gpaqsedall
  left_join(pat_svy[, c("patcid", "gpaqsedall")], by = "patcid") %>%
  ggplot(aes(x = sed_min)) +
  geom_density(aes(fill = "Sensor"), alpha = 0.2) +
  geom_density(aes(x = gpaqsedall, fill = "Self-report"), alpha = 0.2) +
  labs(x = "Daytime sedentary minutes on an average day",
       y = "Density") +
  theme(text = element_text(size = 16)) +
  scale_fill_manual(values = c("Sensor" = "red", "Self-report" = "blue"),
                    name = "Data source")
ggsave("BIOSTAT625-Project/results/figures/sed_min_density_observed.png", width = 9, height = 5, dpi = 600)

plt.minuteid <- histogram(~ minuteid | is.na(count), data = pat_count,
                          xlab = "Time Stamp") # minuteid as a proxy of timestamp
plt.dow <- histogram(~ dow | is.na(count), data = pat_count,
                     xlab = "Day of Week") # more missing on Sat and Sun
plt.hour <- histogram(~ hour | is.na(count), data = pat_count,
                      xlab = "Hour of Day") # more missing in the morning and evening
missing_pattern <- grid.arrange(plt.minuteid, plt.dow, plt.hour, ncol = 3)
ggsave("BIOSTAT625-Project/results/figures/missing_pattern.png", missing_pattern, width = 12, height = 5, dpi = 600)

plt.dem11 <- histogram(~ dem11 | is.na(count), data = pat_count,
                       xlab = "Marriage Status") # more missing if single
plt.dem12 <- histogram(~ dem12 | is.na(count), data = pat_count,
                       xlab = "Level of Education") # more missing senior high school/college student
plt.inc <- histogram(~ povertygroup | is.na(count), data = pat_count,
                     xlab = "Level of Income") # more missing if poor
plt.race <- histogram(~ newrace | is.na(count), data = pat_count,
                      xlab = "Race") # more missing if black
