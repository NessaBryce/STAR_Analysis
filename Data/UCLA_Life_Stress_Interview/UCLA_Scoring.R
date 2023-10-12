### SCORING UCLA Life stress interview --- scored by: Nessa Bryce (nessa.bryce@gmail.com)
 
## -------- Install packages & Load functions: --------

if(!require("pacman")) {install.packages("pacman"); require("pacman")} 
p_load("rstudioapi", "readr","haven","matrixStats")
#source('score_questionnaire.R') 

## Setting working directory: 

## SETTING WORKING DIRECTORY: 
root <- find_rstudio_root_file()
survey_dir <- "/UCLA_Life_Stress_Interview"
survey_wd <- paste0(root,survey_dir)
setwd(survey_wd)

## -------- Loading data: --------

my_data <- read.csv('UCLA_RawData.csv')

## -------- Cleaning the data set ----------

names(my_data)

## Relabeling the variables:
col_names <- c("num", #ucla_episode_X # Number of the episode
               "Month", #ucla_episode_Xa # Month that the episode occurred in
               "Domain", #ucla_episode_Xb # Domain of the episode (see below for list of possible domains)
               "Rej_Ostra", #ucla_episode_Xb_peervo # Peer rejection or ostracism 
               "New_ongo", #ucla_episode_Xc # Is the episode new or ongoing?
               "Impact", #ucla_episode_Xe # Level of impact (range 1-5 with steps of 0.5)
               "Indep_Depend", #ucla_episode_Xf # Was the episode dependent on the subject? (Independant(0))
               "Resolved", #ucla_episode_Xg # Was the episode resolved during the period?
               "Date", #ucla_episode_Xh # date that the episode occurred
               "Exact_Est") #ucla_episode_Xi # if date above is an exact date or an estimate
repeated_colname <- rep(col_names, times = 15)

## Up to 15 episodes can be recorded using the REDcap form:
num_Epi <- 1:15
repeated_num <- rep(num_Epi, each = 10) 
repeated_epi <- rep("Epi", 150) 
col_names_updated <- paste0(repeated_epi,"_",repeated_num,"_",repeated_colname)
col_names_updated
length(col_names_updated)

## Code that double checks the names line up properly!
length(grep("ucla_ep",names(my_data)))

My_df <- as.data.frame(matrix(NA, nrow = 150, ncol = 2))
My_df$V1 <- col_names_updated
My_df[1:149,2] <- names(my_data[grep("ucla_ep", names(my_data))])

names(my_data)

col_index <- which(colnames(my_data) == "ucla_ml_episode_2g") ##for some reason column has _ml_ in the name... 
colnames(my_data)[col_index] <- "ucla_episode_2g"
col_index <- which(colnames(my_data) == "ucla_epsiode_6") ## episode is spelt incorrectly 
colnames(my_data)[col_index] <- "ucla_episode_6"

length(grep("ucla_ep",names(my_data)))

# Updating the column names:
colnames(my_data)[grep("ucla_epi", names(my_data))] <- col_names_updated
names(my_data)

## Factorizing Domain column (so that categories are obvious): 

## There are 9 possible domains: 
Domains <- c("Academic", 
             "Behavioral", 
             "Peer/Romantic", 
             "Family", 
             "Household/Extended Family",
             "Neighborhood",
             "Health", 
             "Finances",
             "Discrimination", 
             "Legal", 
             "Exposure to Violence")

# Factorize the Domain columns:
my_data[,grep("Domain", names(my_data))] <- lapply(my_data[,grep("Domain", names(my_data))], factor) 
# Relabeling the levels of the Domain Columns: 
my_data[,grep("Domain", names(my_data))] <- lapply(my_data[, grepl("Domain", names(my_data))], function(x) {levels(x) <- Domains; x})

## Factorize all col with categorical 


## -------- SCORING CHRONIC STRESS -------- 

names(my_data) ## columns 9-21 are the chronic stressors: 

# "ucla_academic"            
# "ucla_school_behaviour"   
# "ucla_peer_romantic"       
# "ucla_peer_victimization" 
# "ucla_peer_ostracism"
# "ucla_family_relationship"
# "ucla_extended_family"
# "ucla_neighborhood"       
# "ucla_health"
# "ucla_finacial"           
# "ucla_discrimination"
# "ucla_legal"              
# "ucla_exposure_violence" 

## Subsetting Chronic data for clarity: 
chronic_data <- my_data[,9:21]

# Highest chronic stress severity rating: 
chronic_highest_impact_score  <- apply(chronic_data, 1, max, na.rm = TRUE) ## all rows that only have NAs will be -Inf
chronic_highest_impact_score[which(chronic_highest_impact_score == -Inf)] <- 0 ## turning all -Infs to 0 

# Average chronic stress severity rating: 
chronic_ave_impact_score  <- apply(chronic_data, 1, mean, na.rm = TRUE) ## all rows that only have NAs will be -Inf
chronic_ave_impact_score[is.nan(chronic_ave_impact_score)] <- 0 ## turning all NaNs to 0 

# Total chronic stress severity rating: 
Total_Chronic_Stress  <- apply(chronic_data, 1, sum, na.rm = TRUE) ## all rows that only have NAs will be -Inf
Total_Chronic_Stress[is.nan(Total_Chronic_Stress)] <- 0 ## turning all NaNs to 0 


## -------- SCORING EPISODIC STRESSORS -------- 

## Total Scores: 

## All of the Episodic scoring is dependent on the impact column.
## Summary scores include: Total number of episodic stressors, highest impact, average impact and total impact

## Grabbing only episodic impact column: 
col_of_interest_impact <- grepl("Impact", colnames(my_data)) 
episodic_impact <- my_data[,col_of_interest_impact]

# Total number of episodic stressors in past month: 
Number_episodes <- rowSums(!is.na(episodic_impact)) ## this counts how many entries in each row are not NAs (ie. have data in them) 

# Highest episodic stress rating across all episodes in past month:
Highest_impact_score  <- apply(episodic_impact, 1, max, na.rm = TRUE) ## all rows that only have NAs will be -Inf
Highest_impact_score[which(Highest_impact_score == -Inf)] <- 0 ## turning all -Infs to 0 
  
# Average episodic stress severity rating across all domains: 
Ave_impact_score  <- apply(episodic_impact, 1, mean, na.rm = TRUE) ## all rows that only have NAs will be -Inf
Ave_impact_score[is.nan(Ave_impact_score)] <- 0 ## turning all NaNs to 0 

# Total episodic stress: 
Total_episodic_stress <- apply(episodic_impact, 1, sum, na.rm = TRUE) ## all rows that only have NAs will be -Inf
Total_episodic_stress[is.nan(Total_episodic_stress)] <- 0 ## turning all NaNs to 0 

## -------- EXPORTING TOTAL CHRONIC/EPISODIC STRESS DATA -------- 

# Compiling the Chronic and Episodic Scores: 
UCLA_Scores <- as.data.frame(cbind(my_data[,1:2], ## this is the subjectID and month data 
                                   chronic_highest_impact_score, 
                                   chronic_ave_impact_score,
                                   Total_Chronic_Stress,
                                   Number_episodes,
                                   Highest_impact_score,
                                   Ave_impact_score,
                                   Total_episodic_stress))
colnames(UCLA_Scores) <- c("SubID", "Month","Chronic_highest_impact",
                           "Chronic_average_impact",
                           "Total_chronic_stress",
                           "Number_episodes", 
                           "Episodic_highest_impact", 
                           "Episodic_average_impact",
                           "Total_episodic_stress")

# Removing everything after the "_" from the Month column
UCLA_Scores$Month <- gsub("_(.*)", "", UCLA_Scores$Month)

write.csv(UCLA_Scores, "UCLA_Chronic_Episodic_Total_scores.csv")

## -------- SCORING EPISODIC STRESSORS BY DOMAIN -------- 

col_of_interest <- grep("(Domain|Impact)", colnames(my_data), value = TRUE) 
episodic_impact_domain <- my_data[,col_of_interest]

row <- episodic_impact_domain[214,]

# Extracting unique factor levels across the three category columns
unique_categories <- unique(unlist(episodic_impact_domain[grep("Domain", colnames(episodic_impact_domain), value = TRUE)]))

## TOTAL episodic stress by domain:

# Function to compute aggregated scores for each row
aggregate_scores <- function(row, score_type = "sum", df = episodic_impact_domain) {
  categories <- as.character(row[grep("Domain", colnames(df), value = TRUE)])
  scores <- as.numeric(row[grep("Impact", colnames(df), value = TRUE)])
  
  if (score_type == "sum"){
    aggregated <- tapply(scores, categories, sum) # Aggregate scores by category
  } else if(score_type == "count" ) {
    aggregated <- table(categories)
  } else {
    stop("Invalid score type selected")
  }
  # Ensuring consistency with unique categories
  result <- setNames(rep(NA, length(unique_categories)), unique_categories)
  result[names(aggregated)] <- aggregated
  
  return(result)
}

# Apply the function to each row and bind them together
Total_by_domain <- as.data.frame(t(apply(episodic_impact_domain, 1, aggregate_scores, score_type = "sum")))
colnames(Total_by_domain) <- paste(names(Total_by_domain),"_TotalSeverity")

# Apply the function to each row and bind them together
count_by_domain <- as.data.frame(t(apply(episodic_impact_domain, 1, aggregate_scores,score_type = "count")))
colnames(count_by_domain) <- paste(names(count_by_domain),"_NumEpisodes")

Episodics_Severity <- cbind(count_by_domain,Total_by_domain)
Episodics_Severity <- Episodic_Severity_by_domain[, sort(names(Episodic_Severity_by_domain))]
Episodics_Severity <- Episodic_Severity_by_domain[, !grepl("NA", names(Episodic_Severity_by_domain))]

## Adding the SubID and Session columns back in: 
SubID_Session <- UCLA_Scores[,c("SubID","Month")]

Episodic_severity_by_domain <- cbind(SubID_Session, 
                                     UCLA_Scores$Number_episodes,
                                     UCLA_Scores$Total_episodic_stress,
                                     Episodics_Severity)
names(Episodic_severity_by_domain)[3:4] <- c("Total_Num_Episodes", "Total_Severity")

write.csv(Episodic_severity_by_domain, "UCLA_Episodic_Stress_by_Domain.csv")




