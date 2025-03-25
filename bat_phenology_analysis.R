# clear environment
rm(list = ls())

# import relevant packages
library(metafor)

# import data sets
metaanalysis_data <- read.csv("./data/extracted_data.csv", sep = ",", header = TRUE)
head(metaanalysis_data)
bibliography_data <- read.csv("./data/bibliography_data.csv", sep = ",", header = TRUE)
head(bibliography_data)

# combine the two, to form the final dataset
# metaanalysis_data <- merge(bibliography_data, extracted_data, by = "paper_ID")
# View(metaanalysis_data)

# place papers in alphabetical order, as this is more logical
bibliography_data <- bibliography_data[order(bibliography_data$author), ]
metaanalysis_data <- metaanalysis_data[match(bibliography_data$paper_ID, metaanalysis_data$paper_ID), ]


# calculate the difference between the means
metaanalysis_data$difference_means <- metaanalysis_data$light_treatment_mean - metaanalysis_data$dark_treatment_mean
View(metaanalysis_data)

# calculate the associated SE
metaanalysis_data$difference_SE <- sqrt((metaanalysis_data$light_se)^2 + (metaanalysis_data$dark_se)^2)
View(metaanalysis_data)

# visualise the data
plot(metaanalysis_data$difference_means, (1 / metaanalysis_data$difference_SE), xlab = "Mean difference in time of first activity (minutes after sunset)", ylab = "Precision (1/SE)")

# calculate variance
metaanalysis_data$difference_variance <- metaanalysis_data$difference_SE^2

# build meta-analysis model of mean difference
meta_model <- rma(yi = difference_means, sei = difference_SE, data = metaanalysis_data)
meta_model

# plot the model - funnel plot
funnel(meta_model)
funnel(meta_model, label = "all", legend = list(cex = 0.9), back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")

# add labels explaining which points are associated with which papers
study_labels <- c("4 = Zou et al. (2024)", "3 = Stone et al. (2009)", "2 = Mariton et al. (2022)", "1 = Lou et al. (2021)", "Studies:")
xpos <- 38
ypos <- 13
rect(xpos, ypos - 7.4, xpos +34.4, ypos, col = "white", border = "black")
for (i in seq_along(study_labels)) {
  if (study_labels[i] == "Studies:") {
    text(xpos + 1.5, ypos - (i * 1.28), labels = study_labels[i], adj = 0, cex = 0.9, font = 2)
  } else {
    text(xpos + 1.5, ypos - (i * 1.28), labels = study_labels[i], adj = 0, cex = 0.9)
  }}

# plot the model - forest plot
forest(meta_model, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", order = "obs")
