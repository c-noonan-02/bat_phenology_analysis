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

