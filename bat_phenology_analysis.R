#### Set-up ####

# clear environment
rm(list = ls())

# import relevant packages
library(metafor)

#### 1. Meta-analysis on mean time after sunset of onset of activity ####


##### Set-up #####
# import data sets
metaanalysis_data <- read.csv("./timing_data/extracted_data.csv", sep = ",", header = TRUE)
head(metaanalysis_data)
bibliography_data <- read.csv("./timing_data/bibliography_data.csv", sep = ",", header = TRUE)
head(bibliography_data)

# place papers in alphabetical order, as this is more logical
bibliography_data <- bibliography_data[order(bibliography_data$author), ]
metaanalysis_data <- metaanalysis_data[match(bibliography_data$paper_ID, metaanalysis_data$paper_ID), ]


##### Calculate Effect Sizes #####

# calculate sd as this is needed to use the escalc() function
metaanalysis_data$light_sd <- metaanalysis_data$light_se * sqrt(metaanalysis_data$light_n)
metaanalysis_data$dark_sd  <- metaanalysis_data$dark_se  * sqrt(metaanalysis_data$dark_n)

# calculate using package
metaanalysis_data <- escalc(measure = "MD",
                                   m1i = light_treatment_mean, sd1i = light_sd, n1i = light_n,
                                   m2i = dark_treatment_mean, sd2i = dark_sd, n2i = dark_n,
                                   data = metaanalysis_data)
View(metaanalysis_data)

# visualise the data
plot(metaanalysis_data$yi, metaanalysis_data$vi, xlab = "Mean difference in time of first activity (minutes after sunset)", ylab = "Variance")

##### Meta-Analysis #####

# build meta-analysis model of mean difference
timing_meta_model <- rma(yi, vi, data = metaanalysis_data)
timing_meta_model
summary(timing_meta_model)

# plot the model - funnel plot
funnel(timing_meta_model)
funnel(timing_meta_model, label = "all", legend = list(cex = 0.9), yaxis = "sei", back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")

# add labels explaining which points are associated with which papers
study_labels <- c("3 = Zou et al. (2024)", "2 = Stone et al. (2009)", "1 = Lou et al. (2021)", "Studies:")
xpos <- 38.7
ypos <- 13
rect(xpos, ypos - 6, xpos +34.1, ypos, col = "white", border = "black")
for (i in seq_along(study_labels)) {
  if (study_labels[i] == "Studies:") {
    text(xpos + 1.5, ypos - (i * 1.28), labels = study_labels[i], adj = 0, cex = 0.9, font = 2)
  } else {
    text(xpos + 1.5, ypos - (i * 1.28), labels = study_labels[i], adj = 0, cex = 0.7)
  }}

# plot the model - forest plot
forest(timing_meta_model, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", order = "obs", col = "pink", border = "black", colout = "#FF3399")


#### 2. Meta-analysis on mean activity levels ####

# clear environment
rm(list = ls())

# import relevant packages
library(metafor)


##### By species #####

###### Set-up ######
# import data sets
bat_passes_data <- read.csv("./activity_data/bat_passes_data.csv", sep = ",", header = TRUE)
head(bat_passes_data)
bibliography_data <- read.csv("./activity_data/activity_bibliography.csv", sep = ",", header = TRUE)
head(bibliography_data)

# # place papers in alphabetical order, as this is more logical
# bibliography_data <- bibliography_data[order(bibliography_data$author), ]
# bat_passes_data <- bat_passes_data[match(bibliography_data$paper_ID, bat_passes_data$paper_ID), ]


###### Calculate Effect Sizes ######

# calculate sd as this is needed to use the escalc() function
bat_passes_data$light_sd <- bat_passes_data$light_se * sqrt(bat_passes_data$light_n)
bat_passes_data$dark_sd  <- bat_passes_data$dark_se  * sqrt(bat_passes_data$dark_n)

# calculate using package
bat_passes_data <- escalc(measure = "MD",
                            m1i = light_treatment_mean, sd1i = light_sd, n1i = light_n,
                            m2i = dark_treatment_mean, sd2i = dark_sd, n2i = dark_n,
                            data = bat_passes_data)
View(bat_passes_data)


###### Meta-Analysis ######

# build meta-analysis model of mean difference
activity_meta_model <- rma.mv(yi, vi, mods = ~ 1, random = list(~1|species, ~1|paper_ID), data = bat_passes_data)
activity_meta_model
summary(activity_meta_model)

# plot the model - funnel plot
funnel(activity_meta_model)
funnel(activity_meta_model, label = FALSE, legend = list(cex = 0.9), back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")

# plot the model - forest plot
forest(activity_meta_model, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", order = "obs", col = "pink", border = "black", colout = "#FF3399")

# change label from study to estimate, colour by species, etc.


##### Over-all #####


###### Set-up ######
# import data sets
all_bat_passes_data <- read.csv("./activity_data/all_bat_passes_data.csv", sep = ",", header = TRUE)
head(all_bat_passes_data)
bibliography_data <- read.csv("./activity_data/activity_bibliography.csv", sep = ",", header = TRUE)
head(bibliography_data)


###### Calculate Effect Sizes ######

# calculate sd as this is needed to use the escalc() function
all_bat_passes_data$light_sd <- all_bat_passes_data$light_se * sqrt(all_bat_passes_data$light_n)
all_bat_passes_data$dark_sd  <- all_bat_passes_data$dark_se  * sqrt(all_bat_passes_data$dark_n)

# calculate using package
all_bat_passes_data <- escalc(measure = "MD",
                          m1i = light_treatment_mean, sd1i = light_sd, n1i = light_n,
                          m2i = dark_treatment_mean, sd2i = dark_sd, n2i = dark_n,
                          data = all_bat_passes_data)
View(all_bat_passes_data)


###### Meta-Analysis ######

# build meta-analysis model of mean difference
activity_meta_model <- rma(yi, vi, data = all_bat_passes_data)
activity_meta_model
summary(activity_meta_model)

# plot the model - funnel plot
funnel(activity_meta_model)
funnel(activity_meta_model, label = FALSE, legend = list(cex = 0.9), back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")

# plot the model - forest plot
forest(activity_meta_model, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", order = "obs", col = "pink", border = "black", colout = "#FF3399")


#### 3. Assessment of Literature ####

# Here I will visualise the information I have on papers - i.e. the number of papers that will have the data but chose not to consider timing, those which do not report data appropriately, etc.


#### Export all figures ####

# will add code here to save the figures
