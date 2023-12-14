load("BIOSTAT625-Project/src/result_df.RData")

# 1. Open jpeg file
png("BIOSTAT625-Project/results/figures/corrplot.png",
    width = 5,
    height = 5,
    units = "in",
    res = 600)
# 2. Create the plot
corrplot(cor(result_df[, -1]), order = "hclust", 
         tl.col = "black", tl.srt = 45, type = "upper")
# 3. Close the file
dev.off()
