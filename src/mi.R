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
  rename_all(list(~ paste0("imp", 1:5, "_count")))

imp_binary_wide <- complete(imp_binary, "broad") %>%
  select(4, 8, 12, 16, 20) %>%
  # rename columns as imp1_binary, imp2_binary, etc.
  rename_all(list(~ paste0("imp", 1:5, "_binary")))

# combine imputed data
result_df <- cbind(imp_count_wide, imp_binary_wide)

# add back patcid
result_df <- cbind(pat_count[, 1], result_df)

