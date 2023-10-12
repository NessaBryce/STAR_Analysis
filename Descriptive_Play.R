# EMOTION TASK ACTIVITY -- VISUALIZATION  #####

## 

## -------- LOADING PACKAGES:

if(!require("pacman")) {install.packages("pacman"); require("pacman")} ## for plotting correlation plots
p_load("ggcorrplot","corrplot","smacof","colorspace","stringr", "ggplot2","gridExtra","reshape2","tidyr","plyr","haven",
       "rstudioapi","patchwork", "Rmisc","ggcorrplot", "dplyr")

## -------- SETTING WORKING DIRECTORY:

current_path <- getActiveDocumentContext()$path    ## gets path for current R script
setwd(dirname(current_path)) ## sets working directory to folder where current script is located

## -------- DATA PREPARATION : ####

## Loading Data:
MetaAnalysisROI <- read.csv("Data/MetaAnalysisROIs_EmotionProcessing_MeanActivity.csv")
GroupNetworks <- read.csv("Data/GroupBased_Networks_EmotionProcessing_MeanActivity.csv")
IndividualNetworks <- read.csv("Data/Individual_Networks_EmotionProcessing_MeanActivity.csv")

### -------- Meta Analysis Based ROIs :#### 

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

## Creating Amygdala specific df: 

amygdala_df <- MetaAnalysisROI %>%
  filter(grepl("Amygdala", ROI, ignore.case = TRUE))

### --------  Group Networks (Yeo,2011): ####

GroupNetworks$Session <- as.numeric(sub("session", "", GroupNetworks$Session, ignore.case = TRUE))
colnames(GroupNetworks)[c(3,4)] <- c("Network", "Mean_Activity")
unique(GroupNetworks$Network)

names(GroupNetworks)
network_mapping <- c(
  "17Networks_1" = "Vis-1",
  "17Networks_2" = "Vis-2",
  "17Networks_3" = "MOT-1",
  "17Networks_4" = "MOT-2",
  "17Networks_5" = "DAN-2",
  "17Networks_6" = "DAN-1",
  "17Networks_7" = "VAN-1",
  "17Networks_8" = "FP-1",
  "17Networks_9" = "LIM-1",
  "17Networks_10" = "LIM-2",
  "17Networks_11" = "FP-2",
  "17Networks_12" = "FP-3",
  "17Networks_13" = "FP-4",
  "17Networks_14" = "MOT-3",
  "17Networks_15" = "DMN-1",
  "17Networks_16" = "DMN-2",
  "17Networks_17" = "DMN-3"
)

# Use mutate to replace the values in the Network column
GroupNetworks <- GroupNetworks %>%
  mutate(Network = recode(Network, !!!network_mapping))

### --------  Individual Networks: (Kong, 2018) #### 

IndividualNetworks$Session <- as.numeric(sub("session", "", IndividualNetworks$Session, ignore.case = TRUE))
colnames(IndividualNetworks)[c(3,4)] <- c("Subject_Network", "Mean_Activity")

## Color palette to match netowrk colors from dlabel file in connectome work bench:
Individual_colors <- c(
  "VIS-P" = rgb(170, 70, 125, maxColorValue = 255),
  "CG-OP" = rgb(184, 89, 251, maxColorValue = 255),
  "DN-B" = rgb(205, 61, 77, maxColorValue = 255),
  "SMOT-B" = rgb(27, 179, 242, maxColorValue = 255),
  "AUD" = rgb(231, 215, 165, maxColorValue = 255),
  "PM-PPr" = rgb(66, 231, 206, maxColorValue = 255),
  "dATN-B" = rgb(98, 206, 61, maxColorValue = 255),
  "SMOT-A" = rgb(73, 145, 175, maxColorValue = 255),
  "LANG" = rgb(11, 47, 255, maxColorValue = 255),
  "FPN-B" = rgb(228, 228, 0, maxColorValue = 255),
  "FPN-A" = rgb(240, 147, 33, maxColorValue = 255),
  "dATN-A" = rgb(10, 112, 33, maxColorValue = 255),
  "VIS-C" = rgb(119, 17, 133, maxColorValue = 255), 
  "SAL_PMN" = "#febceb", #hex code
  "DN-A" = rgb(100, 49, 73, maxColorValue = 255)
)

range(IndividualNetworks$Mean_Activity)

## Creating an additional variable for network labels that do not include the subject ID:

IndividualNetworks <- IndividualNetworks %>%
  mutate(Network = str_replace(Subject_Network, "^[0-9]+_", ""))

## ---------- DATA VISUALIZATION: #####

## SESSION plotting function:

create_plot <- function(roi, df, column_name, ROI_color) {
  
  roi_df <- df %>%
    dplyr::filter(grepl(roi, .data[[column_name]], ignore.case = TRUE)) # Filter the data frame based on the ROI
  
  mean_activity <- mean(roi_df$Mean_Activity, na.rm = TRUE) # Calculate the mean activity

  roi_plot <- ggplot(roi_df, aes(x = Session, y = Mean_Activity, group = SubjectID)) +
    geom_line(color = ROI_color, alpha = 0.2) +  # Set color aesthetic for lines
    geom_point(color = ROI_color) +  # Set color argument for points
    geom_hline(yintercept = mean_activity, linetype = "dotted", color = ROI_color) +  # Add dashed line at mean activity
    geom_hline(yintercept = 0, linetype = "dotted", color = "#4c4c4c") +  # Add black dashed horizontal line at y = 0
    annotate("text", x = 11.5, y = mean_activity, label = paste0("Mean(", round(mean_activity, 2),")"), color = "black", size = 3, vjust = 1.2) +
    labs(title = paste(roi, "activity across sessions"), x = "Session", y = paste("Activity (Faces > Shapes)", sep = ""), color = "Subject") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#efefef"),
          panel.border = element_rect(fill = NA, color = "black")) +
    ylim(-3, 6) +
    scale_x_continuous(breaks = 1:12, labels = as.character(1:12))
   
  return(roi_plot)
}

### ---------- Amygdala Activity: #####

Amgydala_plot <- create_plot("Amygdala", MetaAnalysisROI, "ROI", "red")
Amgydala_plot

### ---------- Meta Analysis Based ROI Activity:  #####

ROI_plot <- ggplot(MetaAnalysisROI, aes(x = ROI, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(SubjectID)), width = 0.2, height = 0, show.legend = TRUE, alpha = 0.5) + 
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#4c4c4c") +  
  labs(title = "Activity by ROI",x = "ROI",y = "Activity (Faces > Shapes)", color = "Subject") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#efefef"),
        panel.border = element_rect(fill = NA, color = "black")) +
  ylim(-4, 6) 

ROI_plot

# It looks like the r_Fusiform exhibits notable activity -- lets plot it by session: 

r_Fusiform_plot <- create_plot("r_Fusiform", MetaAnalysisROI)
r_Fusiform_plot

### ---------- Group Based Network Activity:  #####

GroupNetwork_plot <- ggplot(GroupNetworks, aes(x = Network, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(SubjectID)), width = 0.2, height = 0, show.legend = TRUE, alpha = 0.5) + 
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#4c4c4c") + 
  labs(title = "Activity by ROI",x = "ROI",y = "Activity (Faces > Shapes)", color = "Subject") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#efefef"),
        panel.border = element_rect(fill = NA, color = "black")) +
  ylim(-4, 6) 

GroupNetwork_plot

VIS_2_plot <- create_plot("VIS-2", GroupNetworks, "Network","#771185")
VIS_2_plot

VIS_C_plot

### ---------- Individualized Network Activity:  #####

IndividualNetwork_plot <- ggplot(IndividualNetworks, aes(x = Network,y = Mean_Activity)) +
  geom_jitter(aes(color = factor(Network)), width = 0.2, height = 0, show.legend = TRUE, alpha = 0.5) +
  #scale_shape_manual(values = c(19,25)) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#4c4c4c") +  # Add black dashed horizontal line at y = 0
  scale_color_manual(values = Individual_colors) +  # Use your custom color palette
  labs(title = "Activity by Network", x = "Network", y = "Activity (Face > Shapes)", color = "Network") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#efefef"),
        panel.border = element_rect(fill = NA, color = "black")) +
  ylim(-4, 6) 

IndividualNetwork_plot

mid_point <- (length(unique(IndividualNetworks$Subject_Network)) + 1) / 2

IndividualNetwork_plot_by_Sub <- ggplot(IndividualNetworks, aes(x = Subject_Network, y = Mean_Activity)) +
  geom_jitter(aes(color = factor(Network)), width = 0.2, height = 0, show.legend = TRUE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Activity by Network", x = "Network", y = "Activity (Faces > Shapes)", color = "Subject") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#4c4c4c") +  # Add black dashed horizontal line at y = 0
  geom_vline(xintercept = mid_point, color = "Black", size = 0.3) +
  scale_color_manual(values = Individual_colors) +  # Use your custom color palette
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#f1f2f2"),
        panel.border = element_rect(fill = NA, color = "black")) +
  ylim(-4, 6) 

IndividualNetwork_plot_by_Sub

## Plotting Networks by Session: 

VIS_C_plot <- create_plot("VIS-C", IndividualNetworks, "Network","#771185")
VIS_C_plot

dATN_B_plot <- create_plot("dATN-B", IndividualNetworks, "Network","#62ce3d")
dATN_B_plot

VIS_P_plot <- create_plot("VIS-P", IndividualNetworks, "Network","#aa467d")
VIS_P_plot

PM_PPr_plot <- create_plot("PM-PPr", IndividualNetworks, "Network","#42e7ce")
PM_PPr_plot

SMOT_A_plot <- create_plot("SMOT-A", IndividualNetworks, "Network","#4991af")
SMOT_A_plot

SMOT_B_plot <- create_plot("SMOT-B", IndividualNetworks, "Network","#1bb3f2")
SMOT_B_plot

AUD_plot <- create_plot("AUD", IndividualNetworks, "Network","#e7d7a5")
AUD_plot

dATN_A_plot <- create_plot("dATN-A", IndividualNetworks, "Network","#0a7021")
dATN_A_plot



CG_OP_plot <- create_plot("CG-OP", IndividualNetworks, "Network","#b859fb")
CG_OP_plot

SAL_PMN_plot <- create_plot("SAL_PMN", IndividualNetworks, "Network","#febceb")
SAL_PMN_plot

FPN_A_plot <- create_plot("FPN-A", IndividualNetworks, "Network","#f09321")
FPN_A_plot

FPN_B_plot <- create_plot("FPN-B", IndividualNetworks, "Network","#e4e400")
FPN_B_plot

DN_A_plot <- create_plot("DN-A", IndividualNetworks, "Network","#643149")
DN_A_plot

DN_B_plot <- create_plot("DN-B", IndividualNetworks, "Network","#cd3d4d")
DN_B_plot

LANG_plot <- create_plot("LANG", IndividualNetworks, "Network","#0b2fff")
LANG_plot




















