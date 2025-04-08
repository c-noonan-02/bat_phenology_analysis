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

##### Set-up #####
# import data sets
literature_data <- read.csv("./general_data/literature_overview.csv", sep = ",", header = TRUE)
head(literature_data)

timing_literature <- data.frame(table(literature_data$timing_decisions))
colnames(timing_literature) <- c("paper_status", "count")
head(timing_literature)

activity_literature <- data.frame(table(literature_data$activity_decisions))
colnames(activity_literature) <- c("paper_status", "count")
head(activity_literature)

# load required packages
library(ggplot2)
library(stringr)

###### Visualise data ######

# re-organise dataframes for clearer visualisation

# define your custom order
custom_order <- c("included", "poor_reporting", "diff_treatment", "not_timing", "irrelevant", "inaccessible")
# apply the custom order to the dataframe
timing_literature$paper_status <- factor(timing_literature$paper_status, levels = custom_order)

# define your custom order
custom_order <- c("included", "poor_reporting", "diff_measure", "diff_treatment", "not_activity_lvl", "irrelevant", "inaccessible")
# apply the custom order to the dataframe
activity_literature$paper_status <- factor(activity_literature$paper_status, levels = custom_order)

# visualise the status of the literature

# save customised colours and tick labels to use in plot
timing_colours <- c("included" = "seagreen", "poor_reporting" = "maroon", "diff_treatment" = "maroon", "not_timing" = "maroon", "irrelevant" = "grey", "inaccessible" = "grey")
timing_labels <- c("included" = "Included", "poor_reporting" = "Unclear reporting", "diff_treatment" = "Different predictor", "not_timing" = "Different behaviour", "irrelevant" = "Irrelevant papers", "inaccessible" = "Inaccessible papers")
# plot literature overview for timing meta-analysis
ggplot(timing_literature, aes(x = paper_status, y = count, fill = paper_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = timing_colours) +
  theme_minimal() +
  labs(x = "Paper Decision", y = "Number of Papers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(timing_labels, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5), legend.position = "none") +
  geom_text(aes(label = count), vjust = -0.5)

# save customised colours and tick labels to use in plot
activity_colours <- c("included" = "seagreen", "poor_reporting" = "maroon","diff_measure" = "maroon", "diff_treatment" = "maroon", "not_activity_lvl" = "maroon", "irrelevant" = "grey", "inaccessible" = "grey")
activity_labels <- c("included" = "Included", "poor_reporting" = "Unclear reporting", "diff_measure" = "Different measure", "diff_treatment" = "Different predictor", "not_activity_lvl" = "Different behaviour", "irrelevant" = "Irrelevant papers", "inaccessible" = "Inaccessible papers")
# plot literature overview for activity levels meta-analysis
ggplot(activity_literature, aes(x = paper_status, y = count, fill = paper_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = activity_colours) +
  theme_minimal() +
  labs(x = "Paper Decision", y = "Number of Papers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(activity_labels, width = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5), legend.position = "none") +
  geom_text(aes(label = count), vjust = -0.5)


#### 4. Publication Bias ####

# Here I might test publication bias if it seems relevant from my plots


#### Export all figures ####

# will add code here to save the figures
