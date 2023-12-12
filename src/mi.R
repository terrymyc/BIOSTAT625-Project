library(mice)
library(tidyr)
source("BIOSTAT625-Project/src/descriptive.R")

# code dem11, dem12, povertygroup, newrace as factor
pat_count$month <- as.factor(pat_count$month)
pat_count$dow <- as.factor(pat_count$dow)
pat_count$dem11 <- as.factor(pat_count$dem11)
pat_count$dem12 <- as.factor(pat_count$dem12)
pat_count$povertygroup <- as.factor(pat_count$povertygroup)
pat_count$newrace <- as.factor(pat_count$newrace)


pat_binary$month <- as.factor(pat_binary$month)
pat_binary$dow <- as.factor(pat_binary$dow)
pat_binary$dem11 <- as.factor(pat_binary$dem11)
pat_binary$dem12 <- as.factor(pat_binary$dem12)
pat_binary$povertygroup <- as.factor(pat_binary$povertygroup)
pat_binary$newrace <- as.factor(pat_binary$newrace)

# inspect intraclass correlation (ICC)
#ICC1(aov(count ~ as.factor(patcid), data = pat_count)) # 0.04438785

# ------------------------------------------------------------
# Multiple imputation using minute-level data only
# ------------------------------------------------------------
pat_binary$sed_min <- as.factor(pat_binary$sed_min)
pat_binary$sed_min_lag <- as.factor(pat_binary$sed_min_lag)


set.seed(123)
imp_count <- futuremice(pat_count[, c(2, 4:6)], n.core = 5)

set.seed(123)
imp_binary <- futuremice(pat_binary[, c(2, 4:6)], n.core = 5)

# inspect convergence
plot(imp_count)
plot(imp_binary)

densityplot(imp_count)

# extract imputed data
imp_count_wide <- complete(imp_count, "broad") %>%
  select(4, 8, 12, 16, 20) %>%
  # transform all columns to sed_min, 1 if count = 0-99, 0 if count >= 100
  mutate_all(list(~ ifelse(. < 100, 1, 0))) %>%
  # rename columns as imp1_count, imp2_count, etc.
  rename_all(list(~ paste0("imp", 1:5, "_count"))) %>%
  # add back patcid
  cbind(pat_count[, 1], .) %>%
  group_by(patcid) %>%
  summarise_at(vars(starts_with("imp")), sum) %>%
  # create a new column as the average of imputed sedentary minutes and discard the five columns
  mutate(sed_min_count = rowMeans(select(., starts_with("imp")))) %>%
  select(-starts_with("imp")) %>%
  # divide sed_min_count by 7 to get the average sedentary minutes per day
  mutate(sed_min_count = sed_min_count / 7)
summary(imp_count_wide)

imp_binary_wide <- complete(imp_binary, "broad") %>%
  select(4, 8, 12, 16, 20) %>%
  mutate_all(list(~ as.integer(.) - 1)) %>%
  # rename columns as imp1_binary, imp2_binary, etc.
  rename_all(list(~ paste0("imp", 1:5, "_binary"))) %>%
  # add back patcid
  cbind(pat_binary[, 1], .) %>%
  group_by(patcid) %>%
  summarise_at(vars(starts_with("imp")), sum) %>%
  # create a new column as the average of imputed sedentary minutes and discard the five columns
  mutate(sed_min_binary = rowMeans(select(., starts_with("imp")))) %>%
  select(-starts_with("imp")) %>%
  # divide sed_min_count by 7 to get the average sedentary minutes per day
  mutate(sed_min_binary = sed_min_binary / 7)
summary(imp_binary_wide)

# merge imp_count_wide and imp_binary_wide
result_df <- cbind(imp_count_wide, imp_binary_wide[, 2])

# density plot to show distributions of sed_min and gpaqsedall
pat_binary %>%
  group_by(patcid) %>%
  summarise(sed_min = sum(as.numeric(sed_min) - 1, na.rm = TRUE)/7) %>%
  left_join(pat_svy[, c("patcid", "gpaqsedall")], by = "patcid") %>%
  left_join(result_df, by = "patcid") %>%
  ggplot(aes(x = sed_min)) +
  geom_density(aes(fill = "Sensor"), alpha = 0.2) +
  geom_density(aes(x = sed_min_count, fill = "MI count"), alpha = 0.2) +
  geom_density(aes(x = sed_min_binary, fill = "MI binary"), alpha = 0.2) +
  geom_density(aes(x = gpaqsedall, fill = "Self-report"), alpha = 0.2) +
  labs(x = "Daytime sedentary minutes on an average day",
       y = "Density") +
  theme(text = element_text(size = 16)) +
  scale_fill_manual(values = c("Sensor" = "red",
                               "MI count" = "yellow",
                               "MI binary" = "green",
                               "Self-report" = "blue"),
                    name = "Data source")

ggsave("BIOSTAT625-Project/results/figures/sed_min_density_imputed.png", width = 9, height = 5, dpi = 600)
