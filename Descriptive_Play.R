########## STAR DESCRIPTIVE PLAY #####

## 

##### Packages and Setting working directory: #####

## PACKAGES:
if(!require("pacman")) {install.packages("pacman"); require("pacman")} ## for plotting correlation plots

p_load("ggcorrplot","corrplot","smacof","colorspace","stringr", "ggplot2","gridExtra","reshape2","tidyr","plyr","haven",
       "rstudioapi","patchwork", "Rmisc","ggcorrplot", "dplyr")

## Setting working directory:
current_path <- getActiveDocumentContext()$path    ## gets path for current R script
setwd(dirname(current_path)) ## sets working directory to folder where current script is located

##### Data #####

## loading CSV:
MetaAnalysisROI <- read.csv("Data/MetaAnalysisROIs_EmotionProcessing_MeanActivity.csv")
names(MetaAnalysisROI)
GroupNetworks <- read.csv("Data/GroupBased_Networks_EmotionProcessing_MeanActivity.csv")
View(GroupNetworks)
IndividualNetworks <- read.csv("Data/Individual_Networks_EmotionProcessing_MeanActivity.csv")
View(IndividualNetworks)

## Cleaning up the Dataframes: 

## MetaAnalysisROI: 
MetaAnalysisROI$Session <- as.numeric(sub("session", "", MetaAnalysisROI$Session, ignore.case = TRUE))

colnames(MetaAnalysisROI)[4] <- "Mean_Activity"

label_map <- c(
"Amygdala_bl_Harvard-Oxford_thr50_binarized" = "Amygdala",
"5mm_4_47_7" = "mPFC",
"5mm_42_25_3" = "r_IFG",
"5mm_-42_25_3" = "l_IFG",
"5mm_48_17_29" = "r_MFG",
"5mm_-42_13_27" = "l_MFG",
"5mm_-2_8_59" = "SFG",
"5mm_53_-50_4" = "r_MTG",
"5mm_38_-55_-20" = "r_Fusiform",
"5mm_-40_-55_-22" = "l_Fusiform",
"5mm_44_4_0" = "r_Insula",
"5mm_-42_4_-1" = "l_Insula",
"5mm_-36_-19_48" = "l_PrimaryMotor",
"5mm_38_-18_45" = "r_PrimaryMotor",
"5mm_-52_-19_7" = "l_Auditory",
"5mm_50_-21_7" = "R_Auditory"
)

MetaAnalysisROI <- MetaAnalysisROI %>%
  mutate(ROI = ifelse(ROI %in% names(label_map), label_map[as.character(ROI)], ROI))

unique(MetaAnalysisROI$ROI)

## 


## Plotting the data: 

# Create the plot

## Amygdala Plot: 

## MetaAnalysisROI: 

ROI_plot <- ggplot(MetaAnalysisROI, aes(x = ROI, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(SubjectID)), width = 0.2, height = 0, show.legend = FALSE) + 
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Mean Activity by ROI",x = "ROI",y = "Mean Activity (Faces > Shapes)", color = "Subject") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ROI_plot

# GroupNetwork:
GroupNetworks$Session <- as.numeric(sub("session", "", GroupNetworks$Session, ignore.case = TRUE))
colnames(GroupNetworks)[c(3,4)] <- c("Network", "Mean_Activity")
unique(GroupNetworks$Network)

GroupNetwork_plot <- ggplot(GroupNetworks, aes(x = Network, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(SubjectID)), width = 0.2, height = 0, show.legend = FALSE) + 
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Mean ROI Activity by ROI", x = "ROI", y = "Mean ROI Activity", color = "Subject") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

GroupNetwork_plot

# IndividualNetwork:
IndividualNetworks$Session <- as.numeric(sub("session", "", IndividualNetworks$Session, ignore.case = TRUE))
colnames(IndividualNetworks)[c(3,4)] <- c("Network", "Mean_Activity")

IndividualNetwork_plot <- ggplot(IndividualNetworks, aes(x = Network, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(SubjectID)), width = 0.2, height = 0, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Mean Activity by Network", x = "Network", y = "Mean Activity", color = "Subject") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

IndividualNetwork_plot

## Varying data point transparency by session: 

IndividualNetwork_Session_plot <- ggplot(IndividualNetworks, aes(x = Network, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(SubjectID), alpha = Session), width = 0.2, height = 0, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Mean Activity by Network", x = "Network", y = "Mean Activity (Faces > Shapes)", color = "Subject", alpha = "Session") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

IndividualNetwork_Session_plot
