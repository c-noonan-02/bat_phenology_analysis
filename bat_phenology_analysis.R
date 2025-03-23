# clear environment
rm(list = ls())

# import relevant packages
library(metafor)

# import data sets
extracted_data <- read.csv("./data/extracted_data.csv", sep = ",", header = TRUE)
head(extracted_data)
bibliography_data <- read.csv("./data/bibliography_data.csv", sep = ",", header = TRUE)
head(bibliography_data)

# combine the two, to form the final dataset
metaanalysis_data <- merge(bibliography_data, extracted_data, by = "paper_ID")
View(metaanalysis_data)

# calculate the difference between the means
metaanalysis_data$difference_means <- metaanalysis_data$light_treatment_mean - metaanalysis_data$dark_treatment_mean
View(metaanalysis_data)

# calculate the associated SE
metaanalysis_data$difference_SE <- sqrt((metaanalysis_data$light_se)^2 + (metaanalysis_data$dark_se)^2)
View(metaanalysis_data)

# visualise the data
plot(metaanalysis_data$difference_means, (1 / metaanalysis_data$difference_SE), xlab = "Mean difference in time of first activity (minutes after sunset)", ylab = "Precision (1/SE)")

# calculate SE
metaanalysis_data$difference_variance <- metaanalysis_data$difference_SE^2

# build meta-analysis model of mean difference
meta_model <- rma(yi = difference_means, vi = difference_variance, data = metaanalysis_data)
meta_model

# plot the model
funnel(meta_model)
funnel(meta_model, back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")
# trying to add labels to the plot
# text(meta_model$yi, metaanalysis_data$difference_SE + 0.05, labels = metaanalysis_data$paper_ID, pos = 3, cex = 0.7)

forest(meta_model, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", order = "obs")
