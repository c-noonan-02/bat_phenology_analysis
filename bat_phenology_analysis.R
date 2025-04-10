#### Set-up ####

# clear environment
rm(list = ls())

# import relevant packages
library(metafor)

#### 1. Meta-analysis on mean time after sunset of onset of activity ####


##### Set-up #####
# import data sets
timing_data <- read.csv("./timing_data/extracted_data.csv", sep = ",", header = TRUE)
head(timing_data)
bibliography_data <- read.csv("./timing_data/bibliography_data.csv", sep = ",", header = TRUE)
head(bibliography_data)

# place papers in alphabetical order, as this is more logical
bibliography_data <- bibliography_data[order(bibliography_data$author), ]
timing_data <- timing_data[match(bibliography_data$paper_ID, timing_data$paper_ID), ]


##### Calculate Effect Sizes #####

# calculate sd as this is needed to use the escalc() function
timing_data$light_sd <- timing_data$light_se * sqrt(timing_data$light_n)
timing_data$dark_sd  <- timing_data$dark_se  * sqrt(timing_data$dark_n)

# calculate using package
timing_data <- escalc(measure = "MD",
                                   m1i = light_treatment_mean, sd1i = light_sd, n1i = light_n,
                                   m2i = dark_treatment_mean, sd2i = dark_sd, n2i = dark_n,
                                   data = timing_data)
View(timing_data)


##### Meta-Analysis #####

# build meta-analysis model of mean difference (Model 1)
timing_meta_model <- rma(yi, vi, data = timing_data)
timing_meta_model
summary(timing_meta_model)

# plot the model - funnel plot
funnel(timing_meta_model)
png("./figures/timing_funnel.png", width = 460, height = 350)
funnel(timing_meta_model, label = "all", legend = list(cex = 0.9), yaxis = "sei", back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")

# add labels explaining which points are associated with which papers
study_labels <- c("3 = JTRLFNEZ", "2 = 6IHYJZ5N", "1 = XLE9CETS", "Studies:")
xpos <- 44
ypos <- 13
rect(xpos, ypos - 6, xpos +28.8, ypos, col = "white", border = "black")
for (i in seq_along(study_labels)) {
  if (study_labels[i] == "Studies:") {
    text(xpos + 1.5, ypos - (i * 1.28), labels = study_labels[i], adj = 0, cex = 0.9, font = 2)
  } else {
    text(xpos + 1.5, ypos - (i * 1.28), labels = study_labels[i], adj = 0, cex = 0.8)
  }}
dev.off()

# plot the model - forest plot
png("./figures/timing_forest.png", width = 600, height = 350)
forest(timing_meta_model, slab = timing_data$paper_ID, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, bg = "white", order = "obs", col = "pink", border = "black", colout = "#FF3399", header = c("Study ID"))
dev.off()


#### 2. Meta-analysis on mean activity levels ####

# import relevant packages
library(metafor)
library(stringr)


##### Over-all #####


###### Set-up ######
# import data sets
species_activity_data <- read.csv("./activity_data/bat_passes_data.csv", sep = ",", header = TRUE)
head(species_activity_data)
bibliography_data <- read.csv("./activity_data/activity_bibliography.csv", sep = ",", header = TRUE)
head(bibliography_data)


###### Calculate Effect Sizes ######

# calculate sd as this is needed to use the escalc() function
species_activity_data$light_sd <- species_activity_data$light_se * sqrt(species_activity_data$light_n)
species_activity_data$dark_sd  <- species_activity_data$dark_se  * sqrt(species_activity_data$dark_n)

# calculate using package
species_activity_data <- escalc(measure = "MD",
                          m1i = light_treatment_mean, sd1i = light_sd, n1i = light_n,
                          m2i = dark_treatment_mean, sd2i = dark_sd, n2i = dark_n,
                          data = species_activity_data)
View(species_activity_data)


###### Meta-Analysis ######

# build meta-analysis model of mean difference (Model 2)
activity_meta_model <- rma(yi, vi, data = species_activity_data)
activity_meta_model
summary(activity_meta_model)

# plot the model - funnel plot
funnel(activity_meta_model)
png("./figures/activity_funnel_overall.png", width = 460, height = 350)
funnel(activity_meta_model, label = "all", legend = list(cex = 0.9), back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")

# add study labels to the plot
study_labels <- c("3 = 66FLKFKZ", "2 = HYJXU4SH", "1 = XLE9CETS", "Studies:")
xpos <- 3
ypos <- 48
rect(xpos, ypos - 26, xpos + 125, ypos, col = "white", border = "black")
for (i in seq_along(study_labels)) {
  if (study_labels[i] == "Studies:") {
    text(xpos + 3, ypos - (i * 5.3), labels = study_labels[i], adj = 0, cex = 0.9, font = 2)
  } else {
    text(xpos + 3, ypos - (i * 5.3), labels = study_labels[i], adj = 0, cex = 0.8)
  }}
dev.off()

# plot the model - forest plot
png("./figures/activity_forest_overall.png", width = 600, height = 350)
forest(activity_meta_model, slab = species_activity_data$paper_ID, cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, bg = "white", order = "obs", col = "pink", border = "black", colout = "#FF3399", header = c("Study ID"))
dev.off()


##### By species #####

###### Set-up ######
# import data sets
activity_data <- read.csv("./activity_data/bat_passes_data.csv", sep = ",", header = TRUE)
head(activity_data)
bibliography_data <- read.csv("./activity_data/activity_bibliography.csv", sep = ",", header = TRUE)
head(bibliography_data)

# # place papers in alphabetical order, as this is more logical
# bibliography_data <- bibliography_data[order(bibliography_data$author), ]
# activity_data <- activity_data[match(bibliography_data$paper_ID, activity_data$paper_ID), ]


###### Calculate Effect Sizes ######

# calculate sd as this is needed to use the escalc() function
activity_data$light_sd <- activity_data$light_se * sqrt(activity_data$light_n)
activity_data$dark_sd  <- activity_data$dark_se  * sqrt(activity_data$dark_n)

# calculate using package
activity_data <- escalc(measure = "MD",
                          m1i = light_treatment_mean, sd1i = light_sd, n1i = light_n,
                          m2i = dark_treatment_mean, sd2i = dark_sd, n2i = dark_n,
                          data = activity_data)
View(activity_data)
activity_data[1,2] <- "Rhinolophus_hipposideros"
View(activity_data)


###### Meta-Analysis ######

# build meta-analysis model of mean difference (Model 3a, complex model)
activity_meta_model1 <- rma.mv(yi, vi, mods = ~ 1, random = list(~1|species, ~1|paper_ID), data = activity_data)
activity_meta_model1
summary(activity_meta_model1)
# build meta-analysis model of mean difference (Model 3b, drop species as random effect)
activity_meta_model2 <- rma.mv(yi, vi, mods = ~ 1, random = ~1|paper_ID, data = activity_data)
summary(activity_meta_model2)
# build meta-analysis model of mean difference (Model 3c, simple model)
activity_meta_model3 <- rma(yi, vi, mods = ~ 1, data = activity_data)
summary(activity_meta_model3)

# plot the model - funnel plot
funnel(activity_meta_model1)

png("./figures/activity_funnel_species.png", width = 460, height = 350)
funnel(activity_meta_model1, label = FALSE, legend = list(cex = 0.9), back = "white", shade = "grey80", hlines = "grey90", lty = 2, lwd = 2, pch = 16, col = "#FF3399")
dev.off()

# plot the model - forest plot
# Use gsub to replace full genus names in dataframe with abbreviated versions, for use in the plot
activity_data$species <- gsub("([A-Za-z])([a-z]+)_([a-z]+)", "\\1. \\3", activity_data$species)
# assign colours for plot
species_colours <- c("R. hipposideros" = "#1abc9c", "M. spp" = "#e74c3c", "P. pygmaeus" = "#3498db", "P. pipistrellus" = "#f39c12", "R. ferrumequinum" = "#9b59b6", "C. gouldii" = "#2ecc71", "O. ridei" = "#16a085", "V. vulturnus" = "lightblue")
paper_colours <- c("4XJZH9V7" = "#1abc9c", "66FLKFKZ" = "#e74c3c", "6IHYJZ5N" = "#3498db", "HYJXU4SH" = "#f39c12", "NSX6JM2N" = "#9b59b6", "XLE9CETS" = "#2ecc71")
activity_data$species_colours <- species_colours[activity_data$species]
activity_data$paper_colours <- paper_colours[activity_data$paper_ID]


png("./figures/activity_forest_species.png", width = 1200, height = 700)
forest(activity_meta_model1,
       slab = activity_data$paper_ID,
       ilab = paste(str_pad(activity_data$species, width = max(nchar(activity_data$species)))),
       ilab.pos = 2,
       cex.lab = 1, cex.axis = 1,
       cex = 2,
       addfit = TRUE,
       bg = "white",
       order = activity_data$species,
       col = "pink", border = "black", colout = activity_data$species_colours,
       #col = "pink", border = "black", colout = "#FF3399",
       header = c("Study ID"))
text(-550, 16.4, "Species", pos = 3, cex = 2, font = 2)
dev.off()


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
custom_order <- c("included", "poor_reporting", "diff_measure", "diff_treatment", "not_activitylvl", "irrelevant", "inaccessible")
# apply the custom order to the dataframe
activity_literature$paper_status <- factor(activity_literature$paper_status, levels = custom_order)

# visualise the status of the literature

# save customised colours and tick labels to use in plot
timing_colours <- c("included" = "seagreen", "poor_reporting" = "maroon", "diff_treatment" = "maroon", "not_timing" = "maroon", "irrelevant" = "grey", "inaccessible" = "grey")
timing_labels <- c("included" = "Included", "poor_reporting" = "Unclear reporting", "diff_treatment" = "Different predictor", "not_timing" = "Different behaviour", "irrelevant" = "Irrelevant papers", "inaccessible" = "Inaccessible papers")
# plot literature overview for timing meta-analysis
timing_literature_plot <-
  ggplot(timing_literature, aes(x = paper_status, y = count, fill = paper_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = timing_colours) +
  theme_minimal() +
  labs(x = "Paper Decision", y = "Number of Papers") +
  scale_x_discrete(labels = function(x) str_wrap(timing_labels, width = 10)) +
  theme(axis.text.x = element_text(hjust = 0.5, size = 10),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = count), vjust = -0.5, size = 5)
timing_literature_plot

# save customised colours and tick labels to use in plot
activity_colours <- c("included" = "seagreen", "poor_reporting" = "maroon","diff_measure" = "maroon", "diff_treatment" = "maroon", "not_activitylvl" = "maroon", "irrelevant" = "grey", "inaccessible" = "grey")
activity_labels <- c("included" = "Included", "poor_reporting" = "Unclear reporting", "diff_measure" = "Different measure", "diff_treatment" = "Different predictor", "not_activitylvl" = "Different behaviour", "irrelevant" = "Irrelevant papers", "inaccessible" = "Inaccessible papers")
# plot literature overview for activity levels meta-analysis
activity_literature_plot <-
  ggplot(activity_literature, aes(x = paper_status, y = count, fill = paper_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = activity_colours) +
  theme_minimal() +
  labs(x = "Paper Decision", y = "Number of Papers") +
  scale_x_discrete(labels = function(x) str_wrap(activity_labels, width = 10)) +
  theme(axis.text.x = element_text(hjust = 0.5, size = 10),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = count), vjust = -0.5, size = 5)
activity_literature_plot

# save both plots to files
ggsave("./figures/activity_literature.png", plot = activity_literature_plot, width = 10, height = 6, dpi = 300)
ggsave("./figures/timing_literature.png", plot = timing_literature_plot, width = 10, height = 6, dpi = 300)


#### 4. Publication Bias ####

# load required packages
library(clubSandwich)

# Here I might test publication bias if it seems relevant from my plots

# test of publication bias for Model 1
regtest(x = timing_meta_model, model = "rma", predictor = "sei", ret.fit = FALSE)
# test of publication bias for Model 2
regtest(x = activity_meta_model, model = "rma", predictor = "sei", ret.fit = FALSE)
# test of publication bias for Model 3(b, as simplified model is necessary for analysis)
coef_test(activity_meta_model2, vcov = "CR2", cluster = activity_data$paper_ID)




