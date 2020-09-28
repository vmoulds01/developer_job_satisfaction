install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyverse')
install.packages('data.table')
install.packages('mice')
install.packages('Amelia')
install.packages('mlbench')
install.packages('hablar')
install.packages('maps')
install.packages('viridis')
install.packages('GGally')
install.packages('mlr3viz')
install.packages('mlr3learners')
install.packages('mlr3filters')
install.packages('praznik')
install.packages('FSelectorRcpp')
install.packages('mlr3')
install.packages('e1071')
install.packages('ggpubr')
install.packages('finalfit')
install.packages('reshape2')
install.packages('stringr')
install.packages('entropy')
install.packages('rbin')

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(mice)
library(Amelia)
library(mlbench)
library(hablar)
require(maps)
require(viridis)
library(GGally)
library(mlr3viz)
library(mlr3learners)
library(mlr3filters)
library(praznik)
library(FSelectorRcpp)
library(mlr3)
library(e1071)
library(ggpubr)
library(finalfit)
library(reshape2)
library(stringr)
library(entropy)
library(rbin)

############################################### DATA CLEANING FUNCTIONS #########################################################################

#################################### Functions for cleaning column names ########################################################################

clean_2020_column_names <- function(input_df){
  old_col_names <- c("CompFreq", "CompTotal", "ConvertedComp", "NEWCollabToolsDesireNextYear",  "NEWCollabToolsWorkedWith", 
                     "NEWDevOps", "NEWDevOpsImpt", "NEWEdImpt", "NEWJobHunt", "NEWJobHuntResearch", "NEWLearn", "NEWOffTopic", 
                     "NEWOnboardGood", "NEWOtherComms", "NEWOvertime", "NEWPurchaseResearch", "NEWPurpleLink", "NEWSOSites",                  
                     "NEWStuck")
  new_col_names <- c("PayFrequency", "Salary", "SalaryUSD", "CollabToolsDesire",  "CollabToolsWorkedWith", 
                     "DevOps", "DevOpsImpt", "EdImpt", "JobHunt", "JobHuntResearch", "Learn", "OffTopic", 
                     "OnboardGood", "OtherComms", "Overtime", "PurchaseResearch", "PurpleLink", "SOSites",                  
                     "Stuck")
  setnames(input_df, old = old_col_names, new = new_col_names)
  return(input_df)
}

clean_2019_column_names <- function(input_df){
  old_col_names <- c('CompFreq', 'CompTotal', 'ConvertedComp')
  new_col_names <- c('PayFrequency', 'Salary', 'SalaryUSD')
  setnames(input_df, old = old_col_names, new = new_col_names)
  return(input_df)
}

clean_2018_column_names <- function(input_df){
  old_col_names <- c('SalaryType', 'ConvertedSalary', 'JobSatisfaction', 'RaceEthnicity', 'JobSearchStatus', 
                     'FrameworkDesireNextYear', 'FrameworkWorkedWith', 'YearsCodingProf', 'Hobby', 
                     'YearsCoding', 'FormalEducation', 'UpdateCV')
  new_col_names <- c('PayFrequency', 'SalaryUSD', 'JobSat', 'Ethnicity', 'JobSeek', 
                     'WebframeDesireNextYear', 'WebframeWorkedWith', 'YearsCodePro', 'Hobbyist', 
                     'YearsCode', 'EdLevel', 'ResumeUpdate')
  setnames(input_df, old = old_col_names, new = new_col_names)
  return(input_df)
}

clean_2017_column_names <- function(input_df){
  old_col_names <- c('JobSatisfaction', 'Race', 'JobSeekingStatus', 
                     'WantWorkFramework', 'HaveWorkedFramework', 
                     'WantWorkLanguage', 'HaveWorkedLanguage', 
                     'WantWorkDatabase', 'HaveWorkedDatabase', 
                     'WantWorkPlatform', 'HaveWorkedPlatform')
  new_col_names <- c('JobSat', 'Ethnicity', 'JobSeek', 
                     'WebframeDesireNextYear', 'WebframeWorkedWith', 
                     'LanguageDesireNextYear', 'LanguageWorkedWith',
                     'DatabaseDesireNextYear', 'DatabaseWorkedWith',
                     'PlatformDesireNextYear', 'PlatformWorkedWith')
  setnames(input_df, old = old_col_names, new = new_col_names)
  return(input_df)
}

################################ Functions for removing columns with duplicated information to other columns #######################################

remove_dup_cols_2020 <- function(input_df){
  dup_cols_to_drop <- c('Salary', 'CurrencyDesc', 'CurrencySymbol', 'Age1stCode')
  data_keep_cols <- input_df[, !(names(input_df) %in% dup_cols_to_drop)]
  return(data_keep_cols)
}

remove_dup_cols_2019 <- function(input_df){
  dup_cols_to_drop <- c('Salary', 'CurrencyDesc', 'CurrencySymbol', 'Age1stCode', 'CareerSat')
  data_keep_cols <- input_df[, !(names(input_df) %in% dup_cols_to_drop)]
  return(data_keep_cols)
}

remove_dup_cols_2018 <- function(input_df){
  dup_cols_to_drop <- c('Salary', 'Currency', 'CurrencySymbol', 'CareerSatisfaction')
  data_keep_cols <- input_df[, !(names(input_df) %in% dup_cols_to_drop)]
  return(data_keep_cols)
}

remove_dup_cols_2017 <- function(input_df){
  dup_cols_to_drop <- c('Currency')
  data_keep_cols <- input_df[, !(names(input_df) %in% dup_cols_to_drop)]
  return(data_keep_cols)
}

################################ Functions to remove ID columns and columns pertaining to stackoverflow survey #######################################

remove_stackoverflow_id_cols_2020 <- function(input_df){
  cols_to_drop <- c("Respondent", "SOAccount", "SOComm", "SOPartFreq", "SOVisitFreq", "SurveyEase", "SurveyLength", 
                    "SOSites", "OffTopic")
  data_keep_cols <- input_df[, !(names(input_df) %in% cols_to_drop)]
  return(data_keep_cols)
}

remove_stackoverflow_id_cols_2019 <- function(input_df){
  cols_to_drop <- c("Respondent", "SOVisit1st", "SOVisitFreq", "SOVisitTo", "SOFindAnswer", "SOTimeSaved", "SOHowMuchTime", 
                    "SOAccount", "SOPartFreq", "SOJobs", "EntTeams", "SOComm", "WelcomeChange", "SONewContent", 
                    "SurveyLength", "SurveyEase" )
  data_keep_cols <- input_df[, !(names(input_df) %in% cols_to_drop)]
  return(data_keep_cols)
}

remove_stackoverflow_id_cols_2018 <- function(input_df){
  cols_to_drop <- c("Respondent", "AdBlocker", "AdBlockerDisable", "AdBlockerReasons", "AdsAgreeDisagree1", 
                    "AdsAgreeDisagree2", "AdsAgreeDisagree3", "AdsActions", "AdsPriorities1", "AdsPriorities2", 
                    "AdsPriorities3", "AdsPriorities4", "AdsPriorities5", "AdsPriorities6", "AdsPriorities7", 
                    "StackOverflowRecommend", "StackOverflowVisit", "StackOverflowHasAccount",   
                    "StackOverflowParticipate", "StackOverflowJobs", "StackOverflowDevStory", "StackOverflowJobsRecommend", 
                    "StackOverflowConsiderMember", "MilitaryUS", "SurveyTooLong", "SurveyEasy", "HypotheticalTools1", 
                    "HypotheticalTools2", "HypotheticalTools3", "HypotheticalTools4", "HypotheticalTools5")
  data_keep_cols <- input_df[, !(names(input_df) %in% cols_to_drop)]
  return(data_keep_cols)
}

remove_stackoverflow_id_cols_2017 <- function(input_df){
  cols_to_drop <- c("Respondent", "PronounceGIF", "StackOverflowDescribes" ,"StackOverflowSatisfaction", 
                    "StackOverflowDevices", "StackOverflowFoundAnswer", "StackOverflowCopiedCode", "StackOverflowJobListing",          
                    "StackOverflowCompanyPage", "StackOverflowJobSearch", "StackOverflowNewQuestion",        
                    "StackOverflowAnswer", "StackOverflowMetaChat", "StackOverflowAdsRelevant",        
                    "StackOverflowAdsDistracting", "StackOverflowModeration", "StackOverflowCommunity",         
                    "StackOverflowHelpful", "StackOverflowBetter", "StackOverflowWhatDo",             
                    "StackOverflowMakeMoney", "SurveyLong", "QuestionsInteresting", "QuestionsConfusing", "InterestedAnswers")
  data_keep_cols <- input_df[, !(names(input_df) %in% cols_to_drop)]
  return(data_keep_cols)
}

############################################## Functions to handle NA Data ########################################################################

remove_job_sat_na <- function(input_df){
  na_removed_df <- input_df %>% drop_na(JobSat) 
  return(na_removed_df)
}

na_feature_count <- function(input_df){
  print(colSums(is.na(input_df)))
}


############################################## Functions to assess factor levels ##################################################################


assess_factors_for_feature <- function(input_df, feature_col_name){
  factor_levels <- input_df %>% 
    group_by(input_df[,feature_col_name]) %>%
    summarise(no_rows = length(input_df[,feature_col_name])) 
  factor_levels_ordered <- arrange(factor_levels, desc(no_rows))
  return(factor_levels_ordered)
}

find_cols_factors_over_200_levels <- function(data_all){
  var_class <- split(names(data_all), sapply(data_all, function(x) paste(class(x), collapse = " ")))
  t <- apply(data_all %>% select(var_class$factor), 2, function(x) length(unique(x)) > 200)
  data_all <- data_all %>% select(c(names(t)[t]))
  return(names(data_all))
}

find_factors_with_over_100_counts <- function(input_df, feature_col_name){
  factors_with_over_100_counts <- input_df %>%
    mutate(feature_col_name = fct_lump(feature_col_name, n = 100)) %>%
    count(feature_col_name)
  return(factors_with_over_100_counts)
} 


remove_levels_from_factor_col <- function(input_df, feature_col_name, factor_level_counts){
  # Need to write this function 
  # Want to count factor levels for column and then remove factors with counts less than some value
  # Drop response level as it shouldn't be in the data
  factor_levels_to_drop = c()
  input_df <- input_df[input_df$feature_col_name!=factor_levels_to_drop,]
  levels(droplevels(input_df$feature_col_name))
}


############################################## Functions to assess variable importance ##################################################################

regression_correlation_feature_importance <- function(input_df, target){
  filter = FilterCorrelation$new()
  data_na_removed_df <- input_df %>% drop_na(target)
  data_task <- data_na_removed_df %>% mutate_if(is.factor, as.numeric)
  task = TaskRegr$new(id = 'devsurvey', backend = data_task, target = target)
  filter$calculate(task)
  corr_score <- as.data.table(filter)
  corr_score_ordered <- corr_score[order(-rank(score))][]
  return(corr_score_ordered)
}

information_gain_feature_importance_no_factor_mutation <- function(input_df, target){
  filter = FilterInformationGain$new()
  data_task <- input_df 
  task = TaskClassif$new(id = 'devsurvey', backend = data_task, target = target)
  filter$calculate(task)
  corr_score <- as.data.table(filter)
  corr_score_ordered <- corr_score[order(-rank(score))][]
  return(corr_score_ordered)
}

information_gain_feature_importance <- function(input_df, target){
  filter = FilterInformationGain$new()
  data_task <- input_df %>% mutate_if(is.character, as.factor)
  task = TaskClassif$new(id = 'devsurvey', backend = data_task, target = target)
  filter$calculate(task)
  corr_score <- as.data.table(filter)
  corr_score_ordered <- corr_score[order(-rank(score))][]
  return(corr_score_ordered)
}

information_gain_ratio_feature_importance <- function(input_df){
  data_task <- input_df %>% mutate_if(is.character, as.factor)
  task = TaskClassif$new(id = 'devsurvey', backend = data_task, target = "JobSat")
  filterGR = flt("information_gain")
  filterGR$param_set$values = list("type" = "gainratio")
  filterGR$calculate(task)
  corr_score <- as.data.table(filterGR)
  corr_score_ordered <- corr_score[order(-rank(score))][]
  return(corr_score_ordered)
}

information_gain_ratio_target <- function(input_df, target){
  data_task <- input_df %>% mutate_if(is.character, as.factor)
  task = TaskClassif$new(id = 'devsurvey', backend = data_task, target = target)
  filterGR = flt("information_gain")
  filterGR$param_set$values = list("type" = "gainratio")
  filterGR$calculate(task)
  corr_score <- as.data.table(filterGR)
  corr_score_ordered <- corr_score[order(-rank(score))][]
  return(corr_score_ordered)
}

performance_measure_feature_importance <- function(input_df){
  data_task <- input_df 
  task = TaskClassif$new(id = 'devsurvey', backend = data_task, target = "JobSat")
  learner = mlr3::lrn("classif.rpart")
  filter = flt("performance", learner = learner)
  filter$calculate(task)
  performance_score <- as.data.table(filter)
  return(performance_score)
}

####################################################### END OF FUNCTIONS ########################################################################


###################################################### CLEANING DATA CODE #######################################################################

###################################################### Clean up 2020 data #######################################################################

data_file_2020 = 'datasets/raw/2020_Survey_Results.csv'
raw_data_2020 <- read_csv(data_file_2020)


## Initial Cleaning
data_2020_clean_col_names <- clean_2020_column_names(raw_data_2020)
data_2020_dup_cols_rm <- remove_dup_cols_2020(data_2020_clean_col_names)
data_2020_clean_cols <- remove_stackoverflow_id_cols_2020(data_2020_dup_cols_rm)

data_2020_jobsat_na_dropped <- remove_job_sat_na(data_2020_clean_cols)

rm(data_2020_clean_col_names, data_2020_dup_cols_rm, data_2020_clean_cols)

## Check missing data 
na_feature_count(data_2020_jobsat_na_dropped)
missmap(data_2020_jobsat_na_dropped, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="Initial Missing Data Map")

################################## 2020 - Missing Data Trends Analysis ########################################################

## For rows with lots of missing data see if there is a trend with features that were filled in
data_lots_missing <- data_2020_jobsat_na_dropped[rowSums(is.na(data_2020_jobsat_na_dropped)) >= 10,]
#missmap(data_lots_missing, col=c("black", "grey"), legend=FALSE)
data_lots_missing_completed_cols <- data_lots_missing[, c('MainBranch', 'Hobbyist', 'Country', 'JobSat')]
data_lots_missing_completed_cols <- data_lots_missing_completed_cols %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Very satisfied'),
                            "Yes","No")) %>% 
  mutate(IsIndia = ifelse(Country %in% c('India'), "Yes", "No" ))
data_lots_missing_completed_cols <- data_lots_missing_completed_cols %>% mutate_if(is.character, as.factor)
summary(data_lots_missing_completed_cols)

## Look at rows with small number of missing data 
data_low_missing <- data_2020_jobsat_na_dropped[rowSums(is.na(data_2020_jobsat_na_dropped)) < 10,]
#missmap(data_low_missing, col=c("black", "grey"), legend=FALSE)
data_low_missing_cols <- data_low_missing[, c('MainBranch', 'Hobbyist', 'Country', 'JobSat')]
data_low_missing_cols <- data_low_missing_cols %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Very satisfied'),
                            "Yes","No")) %>% 
  mutate(IsIndia = ifelse(Country %in% c('India'), "Yes", "No" ))
data_low_missing_cols <- data_low_missing_cols %>% mutate_if(is.character, as.factor)
summary(data_low_missing_cols)

## Assess if there is a difference in 4 data columns for 

## Use chi2 to assess proportion developer versus non-developer between missing data sets
## Null Hypothesis - The proportion of developers is the same in the 2 groups 
## Alternate Hypothesis - The proportion of developers is lower in the group with more missing data 
total_rows_lots_missing <- nrow(data_lots_missing_completed_cols)
total_rows_low_missing <- nrow(data_low_missing_cols)
number_devs_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$MainBranch == 'I am a developer by profession', ])
number_devs_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$MainBranch == 'I am a developer by profession', ])
main_proportion_low_missing_data <- number_devs_low_missing_data / total_rows_low_missing
main_proportion_lots_missing_data <- number_devs_lots_missing_data / total_rows_lots_missing
print(main_proportion_low_missing_data)
print(main_proportion_lots_missing_data)

developer_proportion_chi2_result <- prop.test(x = c(number_devs_lots_missing_data, number_devs_low_missing_data), 
                                              n = c(total_rows_lots_missing, total_rows_low_missing), alternative = "less")
print(developer_proportion_chi2_result)

## p-value < 2.2e-16 which is lower than 0.05 so can accept alternative hypothesis 

## Use chi2 to assess proportion hobbyist verus not a hobbyist between different missing data sets
## Null Hypothesis - The proportion of hobbyists is the same in the 2 groups 
## Alternate Hypothesis - The proportion of hobbyists is lower in the group with more missing data
number_hobbyists_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Hobbyist == 'Yes', ])
number_hobbyists_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Hobbyist == 'Yes', ])
hobbyists_proportion_low_missing_data <- number_hobbyists_low_missing_data / total_rows_low_missing
hobbyists_proportion_lots_missing_data <- number_hobbyists_lots_missing_data / total_rows_lots_missing
print(hobbyists_proportion_low_missing_data)
print(hobbyists_proportion_lots_missing_data)

hobbyists_proportion_chi2_result <- prop.test(x = c(number_hobbyists_lots_missing_data, number_hobbyists_low_missing_data), 
                                              n = c(total_rows_lots_missing, total_rows_low_missing), alternative = "less")
print(hobbyists_proportion_chi2_result)

# p-value is 1.434e-11 which is lower than 0.05 so can accept alternative hypothesis

## Use chi2 to assess proportion from India between different missing data sets
## Null Hypothesis - The proportion of people from India is the same in the 2 groups 
## Alternate Hypothesis - The proportion of people from India is higher in the group with more missing data
number_india_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Country == 'India', ])
number_india_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Country == 'India', ])
india_proportion_low_missing_data <- number_india_low_missing_data / total_rows_low_missing
india_proportion_lots_missing_data <- number_india_lots_missing_data / total_rows_lots_missing
print(india_proportion_low_missing_data)
print(india_proportion_lots_missing_data)

india_proportion_chi2_result <- prop.test(x = c(number_india_lots_missing_data, number_india_low_missing_data), 
                                              n = c(total_rows_lots_missing, total_rows_low_missing), alternative = "greater")
print(india_proportion_chi2_result)

# p-value < 2.2e-16 which is lower than 0.05 so can accept alternative hypothesis

## Use chi2 to assess proportion statisfied between different missing data sets
## Null Hypothesis - The proportion of people satisfied is the same in the 2 groups 
## Alternate Hypothesis - The proportion of people satisfied is lower in the group with more missing data
number_satisfied_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Satisfied == 'Yes', ])
number_satisfied_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Satisfied == 'Yes', ])
sat_proportion_low_missing_data <- number_satisfied_low_missing_data / total_rows_low_missing
sat_proportion_lots_missing_data <- number_satisfied_lots_missing_data / total_rows_lots_missing
print(sat_proportion_low_missing_data)
print(sat_proportion_lots_missing_data)

satisfied_proportion_chi2_result <- prop.test(x = c(number_satisfied_lots_missing_data, number_satisfied_low_missing_data), 
                                              n = c(total_rows_lots_missing, total_rows_low_missing), alternative = "less")
print(satisfied_proportion_chi2_result)

# p-value = 2.785e-05 which is lower than 0.05 so can accept alternative hypothesis

## Produce plots showing the differences in the proportions between missing > 20% of values compared to less 20% values
Missing <- c(">= 20% Missing", "< 20% missing")
India <- c(india_proportion_lots_missing_data, india_proportion_low_missing_data)
Satisfied <- c(sat_proportion_lots_missing_data, sat_proportion_low_missing_data)
Hobbyist <- c(hobbyists_proportion_lots_missing_data, hobbyists_proportion_low_missing_data)
Developer <- c(main_proportion_lots_missing_data, main_proportion_low_missing_data)

combined_df <- data.frame(Missing, India, Satisfied, Hobbyist, Developer)
head(combined_df)

ggplot(data = combined_df %>% gather(Variable, Proportion, -Missing), 
       aes(x = Variable, y = Proportion, fill = Missing)) + 
  geom_bar(stat = 'identity', position = 'dodge') 


########################################## 2020 Missing Data Trends Completed ##################################################

## Check information gain for untouched features
initial_feature_selection <- information_gain_feature_importance(data_2020_jobsat_na_dropped, "JobSat")
print(initial_feature_selection)

######################################### 2020 Binning Features ###############################################################

## Put the following numeric features into bins - Age, SalaryUSD, Workhours, YearsCode & YearsCodePro 
## Bin Age according to 2018 bins
age_bins <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">65")
bin_cut_points <- c(0, 18, 25, 35, 45, 55, 65, Inf)
data_2020_jobsat_na_dropped$AgeBinned <- cut(data_2020_jobsat_na_dropped$Age, breaks = bin_cut_points, labels = age_bins, right = FALSE)
head(data_2020_jobsat_na_dropped[, c('Age', 'AgeBinned')])

## Checking feature importance after binning 
temp_df <- data_2020_jobsat_na_dropped[, !(names(data_2020_jobsat_na_dropped) %in% c('Age'))]
str(temp_df$AgeBinned)
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

## Bin SalaryUSD
hist(data_2020_jobsat_na_dropped$SalaryUSD)
bin_salary <- rbin_quantiles(data_2020_jobsat_na_dropped, JobSat, SalaryUSD, 20)
print(bin_salary)
salary_bins <- c("<5028", "5028-9228", "9228-13620", "13620-18996", "18996-24648", "24648-30384", "30384-36444", 
                 "36444-42036", "42036-47996", "47996-54049", "54049-60000", "60000-66716", "66716-75000", "75000-84000", 
                 "84000-95000", "95000-110000", "110000-129588", "129588-160000", "160000-275000", ">275000")
salary_bin_cut_points <- c(0, 5028, 9228, 13620, 18996, 24648, 30384, 36444, 42036, 47996, 54049, 
                           60000, 66716, 75000, 84000, 95000, 110000, 129588, 160000, 275000, Inf)
data_2020_jobsat_na_dropped$SalaryUSDBinned <- cut(data_2020_jobsat_na_dropped$SalaryUSD, breaks = salary_bin_cut_points, 
                                                   labels = salary_bins, right = FALSE)
head(data_2020_jobsat_na_dropped[, c('SalaryUSD', 'SalaryUSDBinned')], 10)

## Checking feature importance after binning 
temp_df <- data_2020_jobsat_na_dropped[, !(names(data_2020_jobsat_na_dropped) %in% c('Age', 'SalaryUSD'))]
str(temp_df$AgeBinned)
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)


## Bin WorkWeekHrs
hist(data_2020_jobsat_na_dropped$WorkWeekHrs)
bin_work_week_hrs <- rbin_quantiles(data_2020_jobsat_na_dropped, JobSat, WorkWeekHrs, 5)
print(bin_work_week_hrs)
work_week_bins <- c("<37.5", "37.5-40", "40-45", ">=45")
work_week_cut_points <- c(0, 37.5, 40, 45, Inf)
data_2020_jobsat_na_dropped$WorkWeekHrsBinned <- cut(data_2020_jobsat_na_dropped$WorkWeekHrs, breaks = work_week_cut_points, 
                                                   labels = work_week_bins, right = FALSE)
head(data_2020_jobsat_na_dropped[, c('WorkWeekHrs', 'WorkWeekHrsBinned')], 10)

## Checking impact on feature importance
temp_df <- data_2020_jobsat_na_dropped[, !(names(data_2020_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'WorkWeekHrs'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)


## Bin YearsCode To Match 2018 bins
unique(data_2020_jobsat_na_dropped$YearsCode)
data_2020_jobsat_na_dropped$YearsCode <- gsub("Less than 1 year", "0", data_2020_jobsat_na_dropped$YearsCode)
data_2020_jobsat_na_dropped$YearsCode <- gsub("More than 50 years", "60", data_2020_jobsat_na_dropped$YearsCode)
data_2020_jobsat_na_dropped$YearsCode <- as.numeric(data_2020_jobsat_na_dropped$YearsCode)
str(data_2020_jobsat_na_dropped$YearsCode)
hist(data_2020_jobsat_na_dropped$YearsCode)
years_code_bins <- c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24-26", "27-29", ">30")
years_code_cut_points <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, Inf)
data_2020_jobsat_na_dropped$YearsCodeBinned <- cut(data_2020_jobsat_na_dropped$YearsCode, breaks = years_code_cut_points, 
                                                     labels = years_code_bins, right = FALSE)
head(data_2020_jobsat_na_dropped[, c('YearsCode', 'YearsCodeBinned')], 10)

## Checking impact on feature importance
temp_df <- data_2020_jobsat_na_dropped[, !(names(data_2020_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'WorkWeekHrs', 'YearsCode'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

## Bin YearsCodePro according to 2018 bins
unique(data_2020_jobsat_na_dropped$YearsCodePro)
data_2020_jobsat_na_dropped$YearsCodePro <- gsub("Less than 1 year", "0", data_2020_jobsat_na_dropped$YearsCodePro)
data_2020_jobsat_na_dropped$YearsCodePro <- gsub("More than 50 years", "60", data_2020_jobsat_na_dropped$YearsCodePro)
data_2020_jobsat_na_dropped$YearsCodePro <- as.numeric(data_2020_jobsat_na_dropped$YearsCodePro)
str(data_2020_jobsat_na_dropped$YearsCodePro)
hist(data_2020_jobsat_na_dropped$YearsCodePro)
years_code_pro_bins <- c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24-26", "27-29", ">30")
years_code_pro_cut_points <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, Inf)
data_2020_jobsat_na_dropped$YearsCodeProBinned <- cut(data_2020_jobsat_na_dropped$YearsCodePro, breaks = years_code_pro_cut_points, 
                                                   labels = years_code_pro_bins, right = FALSE)
head(data_2020_jobsat_na_dropped[, c('YearsCodePro', 'YearsCodeProBinned')], 10)

## Checking impact on feature importance
temp_df <- data_2020_jobsat_na_dropped[, !(names(data_2020_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'WorkWeekHrs', 'YearsCode', 'YearsCodePro'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

#### NOTE: WorkWeekHrs, YearsCode and YearsCodePro information gain seems to decrease slightly with new bins ####

data_2020_binned <- data_2020_jobsat_na_dropped[, !(names(data_2020_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'WorkWeekHrs', 'YearsCode', 'YearsCodePro'))]  %>% mutate_if(is.factor, as.character)
str(data_2020_binned)

######################################### 2020 Binning Features Completed #######################################################

######################################### 2020 Missing Data Cleaning ############################################################

## Handling missing data by adding a new category to represent missing
data_2020_binned_missing_replaced <- data_2020_binned %>% mutate_if(is.character, ~replace(., is.na(.), "Missing"))
str(data_2020_binned_missing_replaced)
missmap(data_2020_binned_missing_replaced, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="Final Missing Data Map")
na_feature_count(data_2020_binned_missing_replaced)

temp_feature_selection <- information_gain_feature_importance(data_2020_binned_missing_replaced, "JobSat")
print(temp_feature_selection)

######################################### 2020 Missing Data Cleaned ############################################################

######################################### 2020 Factor Level Reduction ##########################################################


## Reduce variables with high number of factor levels i.e. > 200
data_2020_high_factor_levels <- data_2020_binned_missing_replaced %>% mutate_if(is.character, as.factor)
data_2020_initial_clean <- data_2020_binned_missing_replaced %>% mutate_if(is.character, as.factor)
str(data_2020_initial_clean)
cols_over_200_factor_levels <- find_cols_factors_over_200_levels(data_2020_initial_clean)
print(cols_over_200_factor_levels)


## Tidy DatabaseWorkedWith to reduce factor levels to under 200
summary(data_2020_initial_clean$DatabaseWorkedWith)
data_2020_initial_clean$DatabaseWorkedWithNew <- fct_lump_min(data_2020_initial_clean$DatabaseWorkedWith, min = 20)
str(data_2020_initial_clean$DatabaseWorkedWithNew)

## Tidy DatabaseDesireNextYear to reduce factor levels to ~ 200
summary(data_2020_initial_clean$DatabaseDesireNextYear)
data_2020_initial_clean$DatabaseDesireNextYearNew <- fct_lump_min(data_2020_initial_clean$DatabaseDesireNextYear, min = 20)
str(data_2020_initial_clean$DatabaseDesireNextYearNew)

## Tidy DevType to reduce factor levels to ~ 200
summary(data_2020_initial_clean$DevType)
data_2020_initial_clean$DevTypeNew <- fct_lump_min(data_2020_initial_clean$DevType, min = 20)
str(data_2020_initial_clean$DevTypeNew)

## Tidy LanguageDesireNextYear to reduce factor levels to ~ 200
summary(data_2020_initial_clean$LanguageDesireNextYear)
data_2020_initial_clean$LanguageDesireNextYearNew <- fct_lump_min(data_2020_initial_clean$LanguageDesireNextYear, min = 20)
str(data_2020_initial_clean$LanguageDesireNextYearNew)

## Tidy LanguageWorkedWith to reduce factor levels to ~ 200
summary(data_2020_initial_clean$LanguageWorkedWith)
data_2020_initial_clean$LanguageWorkedWithNew <- fct_lump_min(data_2020_initial_clean$LanguageWorkedWith, min = 20)
str(data_2020_initial_clean$LanguageWorkedWithNew)

## Tidy MiscTechDesireNextYear to reduce factor levels to ~ 200
summary(data_2020_initial_clean$MiscTechDesireNextYear)
data_2020_initial_clean$MiscTechDesireNextYearNew <- fct_lump_min(data_2020_initial_clean$MiscTechDesireNextYear, min = 20)
str(data_2020_initial_clean$MiscTechDesireNextYearNew)

## Tidy MiscTechWorkedWith to reduce factor levels to ~ 200
summary(data_2020_initial_clean$MiscTechWorkedWith)
data_2020_initial_clean$MiscTechWorkedWithNew <- fct_lump_min(data_2020_initial_clean$MiscTechWorkedWith, min = 10)
str(data_2020_initial_clean$MiscTechWorkedWithNew)

## Tidy CollabToolsDesire to reduce factor levels to ~ 200
summary(data_2020_initial_clean$CollabToolsDesire)
data_2020_initial_clean$CollabToolsDesireNew <- fct_lump_min(data_2020_initial_clean$CollabToolsDesire, min = 20)
str(data_2020_initial_clean$CollabToolsDesireNew)

## Tidy CollabToolsWorkedWith to reduce factor levels to ~ 200
summary(data_2020_initial_clean$CollabToolsWorkedWith)
data_2020_initial_clean$CollabToolsWorkedWithNew <- fct_lump_min(data_2020_initial_clean$CollabToolsWorkedWith, min = 20)
str(data_2020_initial_clean$CollabToolsWorkedWithNew)

## Tidy Stuck to reduce factor levels to ~ 200
summary(data_2020_initial_clean$Stuck)
data_2020_initial_clean$StuckNew <- fct_lump_min(data_2020_initial_clean$Stuck, min = 10)
str(data_2020_initial_clean$StuckNew)

## Tidy PlatformDesireNextYear to reduce factor levels to ~ 200
summary(data_2020_initial_clean$PlatformDesireNextYear)
data_2020_initial_clean$PlatformDesireNextYearNew <- fct_lump_min(data_2020_initial_clean$PlatformDesireNextYear, min = 20)
str(data_2020_initial_clean$PlatformDesireNextYearNew)

## Tidy PlatformWorkedWith to reduce factor levels to ~ 200
summary(data_2020_initial_clean$PlatformWorkedWith)
data_2020_initial_clean$PlatformWorkedWithNew <- fct_lump_min(data_2020_initial_clean$PlatformWorkedWith, min = 20)
str(data_2020_initial_clean$PlatformWorkedWithNew)

## Tidy WebframeDesireNextYear to reduce factor levels to ~ 200
summary(data_2020_initial_clean$WebframeDesireNextYear)
data_2020_initial_clean$WebframeDesireNextYearNew <- fct_lump_min(data_2020_initial_clean$WebframeDesireNextYear, min = 20)
str(data_2020_initial_clean$WebframeDesireNextYearNew)

## Tidy WebframeWorkedWith to reduce factor levels to ~ 200
summary(data_2020_initial_clean$WebframeWorkedWith)
data_2020_initial_clean$WebframeWorkedWithNew <- fct_lump_min(data_2020_initial_clean$WebframeWorkedWith, min = 20)
str(data_2020_initial_clean$WebframeWorkedWithNew)

data_2020_low_factor_levels <- data_2020_initial_clean[, !(names(data_2020_initial_clean) %in% c('DatabaseDesireNextYear', 'DatabaseWorkedWith', 'DevType', 
                                                                             'JobFactorsNew', 'LanguageDesireNextYear', 'LanguageWorkedWith', 
                                                                             'MiscTechDesireNextYear', 'MiscTechWorkedWith', 
                                                                             'CollabToolsDesire', 'MCollabToolsDesireNew', 
                                                                             'CollabToolsWorkedWith', 'JobHunt', 'Stuck', 
                                                                             'PlatformDesireNextYear', 'PlatformWorkedWith', 
                                                                             'WebframeDesireNextYear', 'WebframeWorkedWith'))]
low_factor_level_feature_selection <- information_gain_feature_importance_no_factor_mutation(data_2020_low_factor_levels, "JobSat")
print(low_factor_level_feature_selection)

low_factor_level_feature_selection_gain_ratio <- information_gain_ratio_feature_importance(data_2020_low_factor_levels)
print(low_factor_level_feature_selection_gain_ratio)

# This takes too long to run in r due to number of categorical variables with high factor levels
#low_factor_level_feature_selection_performance <- performance_measure_feature_importance(data_2020_low_factor_levels)
#print(low_factor_level_feature_selection_performance)

high_factor_level_feature_selection <- information_gain_feature_importance_no_factor_mutation(data_2020_high_factor_levels, "JobSat")
print(high_factor_level_feature_selection)

high_factor_level_feature_selection_gain_ratio <- information_gain_ratio_feature_importance(data_2020_high_factor_levels)
print(high_factor_level_feature_selection_gain_ratio)

## Save off cleaned data 
write.csv(data_2020_low_factor_levels, file = "datasets/interim/2020_clean_data_factors_reduced.csv", row.names = FALSE)
write.csv(data_2020_high_factor_levels, file = "datasets/interim/2020_clean_data_pre_factor_reduction.csv", row.names = FALSE)

rm(corr_lang_score, corr_lang_score_ordered, corr_score, corr_score_ordered, data_2020_binned, data_2020_binned_missing_replaced, 
   data_2020_clean_cols, data_2020_initial_clean, data_2020_jobsat_na_dropped, data_2020_vars_low_missing_data, 
   data_2020_high_factor_levels, data_2020_low_factor_levels, high_factor_level_feature_selection, 
   high_factor_level_feature_selection_gain_ratio, initial_feature_selection, raw_data_2020, temp_feature_selection)

############################################################################################################################

#################################### CLEAN 2019 DATA #######################################################################

data_file_2019 = 'datasets/raw/2019_Survey_Results.csv'
raw_data_2019 <- read_csv(data_file_2019)

## Initial Cleaning
data_2019_clean_col_names <- clean_2019_column_names(raw_data_2019)
data_2019_dup_cols_rm <- remove_dup_cols_2019(data_2019_clean_col_names)
data_2019_clean_cols <- remove_stackoverflow_id_cols_2019(data_2019_dup_cols_rm)

data_2019_jobsat_na_dropped <- remove_job_sat_na(data_2019_clean_cols)

rm(data_2019_clean_col_names, data_2019_dup_cols_rm, data_2019_clean_cols)

## Check missing data 
na_feature_count(data_2019_jobsat_na_dropped)
missmap(data_2019_jobsat_na_dropped, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="2019 - Initial Missing Data Map")

################################## 2019 - Missing Data Trends Analysis ########################################################

## For rows with lots of missing data see if there is a trend with features that were filled in
data_lots_missing <- data_2019_jobsat_na_dropped[rowSums(is.na(data_2019_jobsat_na_dropped)) >= 10,]
#missmap(data_lots_missing, col=c("black", "grey"), legend=FALSE)
data_lots_missing_completed_cols <- data_lots_missing[, c('MainBranch', 'Hobbyist', 'Country', 'JobSat', 'OpenSourcer', 'JobSeek')]
data_lots_missing_completed_cols <- data_lots_missing_completed_cols %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Very satisfied'),
                            "Yes","No")) 
data_lots_missing_completed_cols <- data_lots_missing_completed_cols %>% mutate_if(is.character, as.factor)
summary(data_lots_missing_completed_cols)

## Look at rows with small number of missing data 
data_low_missing <- data_2019_jobsat_na_dropped[rowSums(is.na(data_2019_jobsat_na_dropped)) < 10,]
missmap(data_low_missing, col=c("black", "grey"), legend=FALSE)
data_low_missing_cols <- data_low_missing[, c('MainBranch', 'Hobbyist', 'Country', 'JobSat', 'OpenSourcer', 'JobSeek')]
data_low_missing_cols <- data_low_missing_cols %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Very satisfied'),
                            "Yes","No")) 
data_low_missing_cols <- data_low_missing_cols %>% mutate_if(is.character, as.factor)
summary(data_low_missing_cols)


data_lots_missing_completed_cols$Missing <- ">20% missing data"
data_low_missing_cols$Missing <- "<20% missing data"
combined_df <- rbind(data_lots_missing_completed_cols, data_low_missing_cols)
tail(combined_df)

## Chi2 test to see if proportion between main branch data is same or different 
## Null hypothesis is that they are the same
total_rows_lots_missing <- nrow(data_lots_missing_completed_cols)
total_rows_low_missing <- nrow(data_low_missing_cols)
number_devs_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$MainBranch == 'I am a developer by profession', ])
number_devs_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$MainBranch == 'I am a developer by profession', ])
main_proportion_low_missing_data <- number_devs_low_missing_data / total_rows_low_missing
main_proportion_lots_missing_data <- number_devs_lots_missing_data / total_rows_lots_missing
print(main_proportion_low_missing_data)
print(main_proportion_lots_missing_data)

with(combined_df,table(Missing,MainBranch))
with(combined_df, chisq.test(Missing,MainBranch))

## Chi2 Results:
##    X-squared = 693.94, df = 1, p-value < 2.2e-16

## Chi2 test to see if proportion between hobbyist data is same or different 
## Null hypothesis is that they are the same
number_hobby_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Hobbyist == 'Yes', ])
number_hobby_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Hobbyist == 'Yes', ])
hobbyists_proportion_low_missing_data <- number_hobby_low_missing_data / total_rows_low_missing
hobbyists_proportion_lots_missing_data <- number_hobby_lots_missing_data / total_rows_lots_missing
print(hobbyists_proportion_low_missing_data)
print(hobbyists_proportion_lots_missing_data)

with(combined_df,table(Missing,Hobbyist))
with(combined_df, chisq.test(Missing,Hobbyist))

## Chi2 Results:
##    X-squared = 158.17, df = 1, p-value < 2.2e-16

## Chi2 test to see if proportion between satisfied data is same or different 
## Null hypothesis is that they are the same
number_sat_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Satisfied == 'Yes', ])
number_sat_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Satisfied == 'Yes', ])
sat_proportion_low_missing_data <- number_sat_low_missing_data / total_rows_low_missing
sat_proportion_lots_missing_data <- number_sat_lots_missing_data / total_rows_lots_missing
print(sat_proportion_low_missing_data)
print(sat_proportion_lots_missing_data)

with(combined_df,table(Missing,Satisfied))
with(combined_df, chisq.test(Missing,Satisfied))

## Chi2 Results:
##    X-squared = 47.582, df = 1, p-value = 5.274e-12

## Chi2 test to see if proportion between country data is same or different 
## Null hypothesis is that they are the same

with(combined_df,table(Missing,Country))
with(combined_df, chisq.test(Missing,Country))

## Chi2 Results:
##    X-squared = 1219, df = 169, p-value < 2.2e-16 though get warning it mightn't be right

## Chi2 test to see if proportion between open sources data is same or different 
## Null hypothesis is that they are the same
opensourcer_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$OpenSourcer == 'Once a month or more often', ])
opensourcer_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$OpenSourcer == 'Once a month or more often', ])
opensourcer_proportion_low_missing_data <- opensourcer_low_missing_data / total_rows_low_missing
opensourcer_proportion_lots_missing_data <- opensourcer_lots_missing_data / total_rows_lots_missing
print(opensourcer_proportion_low_missing_data)
print(opensourcer_proportion_lots_missing_data)

with(combined_df,table(Missing,OpenSourcer))
with(combined_df, chisq.test(Missing,OpenSourcer))

## Chi2 Results:
##    X-squared = 97.373, df = 3, p-value < 2.2e-16

## Chi2 test to see if proportion between job seek data is same or different 
## Null hypothesis is that they are the same
jobseeker_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$JobSeek == 'I am actively looking for a job', ])
jobseeker_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$JobSeek == 'I am actively looking for a job', ])
jobseeker_proportion_low_missing_data <- jobseeker_low_missing_data / total_rows_low_missing
jobseeker_proportion_lots_missing_data <- jobseeker_lots_missing_data / total_rows_lots_missing
print(jobseeker_proportion_low_missing_data)
print(jobseeker_proportion_lots_missing_data)

with(combined_df,table(Missing,JobSeek))
with(combined_df, chisq.test(Missing,JobSeek))

## Chi2 Results:
##    X-squared = 169.46, df = 2, p-value < 2.2e-16

## Produce plots showing the differences in the proportions between missing > 20% of values compared to less 20% values
Missing <- c(">= 20% Missing", "< 20% missing")
Satisfied <- c(sat_proportion_lots_missing_data, sat_proportion_low_missing_data)
Hobbyist <- c(hobbyists_proportion_lots_missing_data, hobbyists_proportion_low_missing_data)
Developer <- c(main_proportion_lots_missing_data, main_proportion_low_missing_data)
OpenSourcer <- c(opensourcer_proportion_lots_missing_data, opensourcer_proportion_low_missing_data)
JobSeeker <- c(jobseeker_proportion_lots_missing_data, jobseeker_proportion_low_missing_data)

plot_df <- data.frame(Missing, Satisfied, Hobbyist, Developer, OpenSourcer, JobSeeker)
head(plot_df)

ggplot(data = plot_df %>% gather(Variable, Proportion, -Missing), 
       aes(x = Variable, y = Proportion, fill = Missing)) + 
  geom_bar(stat = 'identity', position = 'dodge') 


########################################## 2019 Missing Data Trends Completed ##################################################

## Check information gain for untouched features
initial_feature_selection <- information_gain_feature_importance(data_2019_jobsat_na_dropped, "JobSat")
print(initial_feature_selection)

## Check initial gain ratio for untouched features 
initial_feature_gain_ratio <- information_gain_ratio_feature_importance(data_2019_jobsat_na_dropped)
print(initial_feature_gain_ratio)

######################################### 2019 Binning Features ###############################################################

summary(data_2019_jobsat_na_dropped)


## Put the following numeric features into bins - Age, CodeRevHrs, WorkWeekHrs, SalaryUSD, YearsCode, YearsCodePro
## Bin Age according to 2018 bins
hist(data_2019_jobsat_na_dropped$Age)
age_bins <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">65")
bin_cut_points <- c(0, 18, 25, 35, 45, 55, 65, Inf)
data_2019_jobsat_na_dropped$AgeBinned <- cut(data_2019_jobsat_na_dropped$Age, breaks = bin_cut_points, labels = age_bins, right = FALSE)
head(data_2019_jobsat_na_dropped[, c('Age', 'AgeBinned')])

## Checking feature importance after binning 
temp_df <- data_2019_jobsat_na_dropped[, !(names(data_2019_jobsat_na_dropped) %in% c('Age'))]
str(temp_df$AgeBinned)
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

# Bin CodeRevHrs
hist(data_2019_jobsat_na_dropped$CodeRevHrs)
bins <- rbin_quantiles(data_2019_jobsat_na_dropped, JobSat, CodeRevHrs, 8)
print(bins)
code_rev_hrs_bins <- c("<2", "2-3", "3-4", "4-5", "5-6", "6-10", ">10")
code_rev_hrs_cut_points <- c(0, 2, 3, 4, 5, 6, 10, Inf)
data_2019_jobsat_na_dropped$CodeRevHrsBinned <- cut(data_2019_jobsat_na_dropped$CodeRevHrs, breaks = code_rev_hrs_cut_points, labels = code_rev_hrs_bins, right = FALSE)
head(data_2019_jobsat_na_dropped[, c('CodeRevHrs', 'CodeRevHrsBinned')])

## Bin SalaryUSD
hist(data_2019_jobsat_na_dropped$SalaryUSD)
bin_salary <- rbin_quantiles(data_2019_jobsat_na_dropped, JobSat, SalaryUSD, 20)
print(bin_salary)
salary_bins <- c("<5028", "5028-9228", "9228-13620", "13620-18996", "18996-24648", "24648-30384", "30384-36444", 
                 "36444-42036", "42036-47996", "47996-54049", "54049-60000", "60000-66716", "66716-75000", "75000-84000", 
                 "84000-95000", "95000-110000", "110000-129588", "129588-160000", "160000-275000", ">275000")
salary_bin_cut_points <- c(0, 5028, 9228, 13620, 18996, 24648, 30384, 36444, 42036, 47996, 54049, 
                           60000, 66716, 75000, 84000, 95000, 110000, 129588, 160000, 275000, Inf)
data_2019_jobsat_na_dropped$SalaryUSDBinned <- cut(data_2019_jobsat_na_dropped$SalaryUSD, breaks = salary_bin_cut_points, 
                                                   labels = salary_bins, right = FALSE)
head(data_2019_jobsat_na_dropped[, c('SalaryUSD', 'SalaryUSDBinned')], 10)

## Checking feature importance after binning 
temp_df <- data_2019_jobsat_na_dropped[, !(names(data_2019_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'CodeRevHrs'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)


## Bin WorkWeekHrs
hist(data_2019_jobsat_na_dropped$WorkWeekHrs)
bin_work_week_hrs <- rbin_quantiles(data_2019_jobsat_na_dropped, JobSat, WorkWeekHrs, 5)
print(bin_work_week_hrs)
work_week_bins <- c("<37.5", "37.5-40", "40-45", ">=45")
work_week_cut_points <- c(0, 37.5, 40, 45, Inf)
data_2019_jobsat_na_dropped$WorkWeekHrsBinned <- cut(data_2019_jobsat_na_dropped$WorkWeekHrs, breaks = work_week_cut_points, 
                                                     labels = work_week_bins, right = FALSE)
head(data_2019_jobsat_na_dropped[, c('WorkWeekHrs', 'WorkWeekHrsBinned')], 10)

## Checking impact on feature importance
temp_df <- data_2019_jobsat_na_dropped[, !(names(data_2019_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'CodeRevHrs', 'WorkWeekHrs'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)


## Bin YearsCode according to 2018 bins
unique(data_2019_jobsat_na_dropped$YearsCode)
data_2019_jobsat_na_dropped$YearsCode <- gsub("Less than 1 year", "0", data_2019_jobsat_na_dropped$YearsCode)
data_2019_jobsat_na_dropped$YearsCode <- gsub("More than 50 years", "60", data_2019_jobsat_na_dropped$YearsCode)
data_2019_jobsat_na_dropped$YearsCode <- as.numeric(data_2019_jobsat_na_dropped$YearsCode)
str(data_2019_jobsat_na_dropped$YearsCode)
hist(data_2019_jobsat_na_dropped$YearsCode)
years_code_bins <- c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24-26", "27-29", ">30")
years_code_cut_points <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, Inf)
data_2019_jobsat_na_dropped$YearsCodeBinned <- cut(data_2019_jobsat_na_dropped$YearsCode, breaks = years_code_cut_points, 
                                                   labels = years_code_bins, right = FALSE)
head(data_2019_jobsat_na_dropped[, c('YearsCode', 'YearsCodeBinned')], 10)

## Checking impact on feature importance
temp_df <- data_2019_jobsat_na_dropped[, !(names(data_2019_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'CodeRevHrs', 'WorkWeekHrs', 'YearsCode'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

## Bin YearsCodePro
unique(data_2019_jobsat_na_dropped$YearsCodePro)
data_2019_jobsat_na_dropped$YearsCodePro <- gsub("Less than 1 year", "0", data_2019_jobsat_na_dropped$YearsCodePro)
data_2019_jobsat_na_dropped$YearsCodePro <- gsub("More than 50 years", "60", data_2019_jobsat_na_dropped$YearsCodePro)
data_2019_jobsat_na_dropped$YearsCodePro <- as.numeric(data_2019_jobsat_na_dropped$YearsCodePro)
str(data_2019_jobsat_na_dropped$YearsCodePro)
hist(data_2019_jobsat_na_dropped$YearsCodePro)
years_code_pro_bins <- c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24-26", "27-29", ">30")
years_code_pro_cut_points <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, Inf)
data_2019_jobsat_na_dropped$YearsCodeProBinned <- cut(data_2019_jobsat_na_dropped$YearsCodePro, breaks = years_code_pro_cut_points, 
                                                      labels = years_code_pro_bins, right = FALSE)
head(data_2019_jobsat_na_dropped[, c('YearsCodePro', 'YearsCodeProBinned')], 10)

## Checking impact on feature importance
temp_df <- data_2019_jobsat_na_dropped[, !(names(data_2019_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'CodeRevHrs', 'WorkWeekHrs', 'YearsCode', 'YearsCodePro'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

#### NOTE: WorkWeekHrs, YearsCode and YearsCodePro information gain seems to decrease slightly with new bins ####

data_2019_binned <- data_2019_jobsat_na_dropped[, !(names(data_2019_jobsat_na_dropped) %in% c('Age', 'SalaryUSD', 'CodeRevHrs', 'WorkWeekHrs', 'YearsCode', 'YearsCodePro'))]  %>% mutate_if(is.factor, as.character)
str(data_2019_binned)

######################################### 2019 Binning Features Completed #######################################################

######################################### 2019 Missing Data Cleaning ############################################################

## Handling missing data by adding a new category to represent missing
data_2019_binned_missing_replaced <- data_2019_binned %>% mutate_if(is.character, ~replace(., is.na(.), "Missing"))
str(data_2019_binned_missing_replaced)
missmap(data_2019_binned_missing_replaced, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="2019 - Final Missing Data Map")
na_feature_count(data_2019_binned_missing_replaced)

temp_feature_selection <- information_gain_feature_importance(data_2019_binned_missing_replaced, "JobSat")
print(temp_feature_selection)

######################################### 2019 Missing Data Cleaned ############################################################

## Reduce variables with high number of factor levels i.e. > 200
data_2019_high_factor_levels <- data_2019_binned_missing_replaced %>% mutate_if(is.character, as.factor)
summary(data_2019_high_factor_levels)
## Save off cleaned data 
write.csv(data_2019_high_factor_levels, file = "datasets/interim/2019_clean_data_pre_factor_reduction.csv", row.names = FALSE)

######################################### 2019 Factor Level Reduction ##########################################################

data_2019_initial_clean <- data_2019_binned_missing_replaced %>% mutate_if(is.character, as.factor)
str(data_2019_initial_clean)
cols_over_200_factor_levels <- find_cols_factors_over_200_levels(data_2019_initial_clean)
print(cols_over_200_factor_levels)

rm(bin_salary, bin_work_week_hrs, bin_years_code, bin_years_code_pro, bins, combined_df, data_2019_binned, data_2019_binned_missing_replaced, 
   data_2019_high_factor_levels, data_2019_initial_clean, data_2019_jobsat_na_dropped, data_lots_missing, data_lots_missing_completed_cols, 
   data_low_missing, data_low_missing_cols, initial_feature_gain_ratio, initial_feature_selection, plot_df, temp_df, 
   temp_feature_selection, raw_data_2019)

############################################## CLEANED 2019 DATA #######################################################################

#############################################################€##########################################################################

############################################## CLEAN 2018 DATA #########################################################################

data_file_2018 = 'datasets/raw/2018_Survey_Results.csv'
raw_data_2018 <- read_csv(data_file_2018)

## Initial Cleaning
data_2018_clean_col_names <- clean_2018_column_names(raw_data_2018)
data_2018_dup_cols_rm <- remove_dup_cols_2018(data_2018_clean_col_names)
data_2018_clean_cols <- remove_stackoverflow_id_cols_2018(data_2018_dup_cols_rm)

data_2018_jobsat_na_dropped <- remove_job_sat_na(data_2018_clean_cols)

rm(data_2018_clean_col_names, data_2018_dup_cols_rm, data_2018_clean_cols)

## Check missing data 
na_feature_count(data_2018_jobsat_na_dropped)
missmap(data_2018_jobsat_na_dropped, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="2018 - Initial Missing Data Map")

############################################## 2018 - Missing Data Trends Analysis ########################################################

## For rows with lots of missing data see if there is a trend with features that were filled in
data_lots_missing <- data_2018_jobsat_na_dropped[rowSums(is.na(data_2018_jobsat_na_dropped)) >= 18,]
#missmap(data_lots_missing, col=c("black", "grey"), legend=FALSE)
data_lots_missing_completed_cols <- data_lots_missing[, c('Hobbyist', 'Country', 'JobSat', 'OpenSource', 'Employment', 'YearsCodePro')]
data_lots_missing_completed_cols <- data_lots_missing_completed_cols %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Moderately satisfied', 'Extremely satisfied'),
                            "Yes","No")) 
data_lots_missing_completed_cols <- data_lots_missing_completed_cols %>% mutate_if(is.character, as.factor)
summary(data_lots_missing_completed_cols)

## Look at rows with small number of missing data 
data_low_missing <- data_2018_jobsat_na_dropped[rowSums(is.na(data_2018_jobsat_na_dropped)) < 18,]
#missmap(data_low_missing, col=c("black", "grey"), legend=FALSE)
data_low_missing_cols <- data_low_missing[, c('Hobbyist', 'Country', 'JobSat', 'OpenSource', 'Employment', 'YearsCodePro')]
data_low_missing_cols <- data_low_missing_cols %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Moderately satisfied', 'Extremely satisfied'),
                            "Yes","No")) 
data_low_missing_cols <- data_low_missing_cols %>% mutate_if(is.character, as.factor)
summary(data_low_missing_cols)


data_lots_missing_completed_cols$Missing <- ">=20% missing data"
data_low_missing_cols$Missing <- "<20% missing data"
combined_df <- rbind(data_lots_missing_completed_cols, data_low_missing_cols)
head(combined_df)


total_rows_lots_missing <- nrow(data_lots_missing_completed_cols)
total_rows_low_missing <- nrow(data_low_missing_cols)

## Chi2 test to see if proportion between hobbyist data is same or different 
## Null hypothesis is that they are the same
number_hobby_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Hobbyist == 'Yes', ])
number_hobby_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Hobbyist == 'Yes', ])
hobbyists_proportion_low_missing_data <- number_hobby_low_missing_data / total_rows_low_missing
hobbyists_proportion_lots_missing_data <- number_hobby_lots_missing_data / total_rows_lots_missing
print(hobbyists_proportion_low_missing_data)
print(hobbyists_proportion_lots_missing_data)

with(combined_df,table(Missing,Hobbyist))
with(combined_df, chisq.test(Missing,Hobbyist))

## Chi2 Results:
##    X-squared = 149.14, df = 1, p-value < 2.2e-16

## Chi2 test to see if proportion between satisfied data is same or different 
## Null hypothesis is that they are the same
number_sat_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Satisfied == 'Yes', ])
number_sat_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Satisfied == 'Yes', ])
sat_proportion_low_missing_data <- number_sat_low_missing_data / total_rows_low_missing
sat_proportion_lots_missing_data <- number_sat_lots_missing_data / total_rows_lots_missing
print(sat_proportion_low_missing_data)
print(sat_proportion_lots_missing_data)

with(combined_df,table(Missing,Satisfied))
with(combined_df, chisq.test(Missing,Satisfied))

## Chi2 Results:
##    X-squared = 120.22, df = 1, p-value < 2.2e-16

## Chi2 test to see if proportion between country data is same or different 
## Null hypothesis is that they are the same

with(combined_df,table(Missing,Country))
with(combined_df, chisq.test(Missing,Country))

## Chi2 Results:
##    X-squared = 1916.8, df = 169, p-value < 2.2e-16 though get warning it mightn't be right

## Chi2 test to see if proportion between open sources data is same or different 
## Null hypothesis is that they are the same
opensourcer_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$OpenSource == 'Yes', ])
opensourcer_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$OpenSource == 'Yes', ])
opensourcer_proportion_low_missing_data <- opensourcer_low_missing_data / total_rows_low_missing
opensourcer_proportion_lots_missing_data <- opensourcer_lots_missing_data / total_rows_lots_missing
print(opensourcer_proportion_low_missing_data)
print(opensourcer_proportion_lots_missing_data)

with(combined_df,table(Missing,OpenSource))
with(combined_df, chisq.test(Missing,OpenSource))

## Chi2 Results:
##    X-squared = 21.744, df = 1, p-value = 3.116e-06

## Chi2 test to see if proportion between Employment data is same or different 
## Null hypothesis is that they are the same
employment_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$Employment == 'Employed full-time', ])
employment_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$Employment == 'Employed full-time', ])
employment_proportion_low_missing_data <- employment_low_missing_data / total_rows_low_missing
employment_proportion_lots_missing_data <- employment_lots_missing_data / total_rows_lots_missing
print(employment_proportion_low_missing_data)
print(employment_proportion_lots_missing_data)

with(combined_df,table(Missing,Employment))
with(combined_df, chisq.test(Missing,Employment))

## Chi2 Results:
##    X-squared = 181.17, df = 2, p-value < 2.2e-16

## Chi2 test to see if proportion between years code pro data is same or different 
## Null hypothesis is that they are the same
summary(data_lots_missing_completed_cols$YearsCodePro)
summary(data_low_missing_cols$YearsCodePro)
yearspro_lots_missing_data <- nrow(data_lots_missing_completed_cols[data_lots_missing_completed_cols$YearsCodePro == '0-2 years', ])
yearspro_low_missing_data <- nrow(data_low_missing_cols[data_low_missing_cols$YearsCodePro == '0-2 years', ])
yearspro_proportion_low_missing_data <- yearspro_low_missing_data / total_rows_low_missing
yearspro_proportion_lots_missing_data <- yearspro_lots_missing_data / total_rows_lots_missing
print(yearspro_proportion_low_missing_data)
print(yearspro_proportion_lots_missing_data)

with(combined_df,table(Missing,YearsCodePro))
with(combined_df, chisq.test(Missing,YearsCodePro))

## Chi2 Results:
##    X-squared = 486.57, df = 10, p-value < 2.2e-16

## Produce plots showing the differences in the proportions between missing > 20% of values compared to less 20% values
Missing <- c(">= 20% Missing", "< 20% missing")
Satisfied <- c(sat_proportion_lots_missing_data, sat_proportion_low_missing_data)
Hobbyist <- c(hobbyists_proportion_lots_missing_data, hobbyists_proportion_low_missing_data)
OpenSourcer <- c(opensourcer_proportion_lots_missing_data, opensourcer_proportion_low_missing_data)
FullTime <- c(employment_proportion_lots_missing_data, employment_proportion_low_missing_data)
Less2YearsProCoder <- c(yearspro_proportion_lots_missing_data, yearspro_proportion_low_missing_data)

plot_df <- data.frame(Missing, Satisfied, Hobbyist, OpenSourcer, FullTime, Less2YearsProCoder)
head(plot_df)

ggplot(data = plot_df %>% gather(Variable, Proportion, -Missing), 
       aes(x = Variable, y = Proportion, fill = Missing)) + 
  geom_bar(stat = 'identity', position = 'dodge') 


##################################################### 2018 Missing Data Trends Completed ##################################################

## Check information gain for untouched features
initial_feature_selection <- information_gain_feature_importance(data_2018_jobsat_na_dropped, "JobSat")
print(initial_feature_selection)

##################################################### 2018 Binning Features ###############################################################

summary(data_2018_jobsat_na_dropped)


## Put the following numeric features into bins - SalaryUSD

## Bin SalaryUSD
hist(data_2018_jobsat_na_dropped$SalaryUSD)
bin_salary <- rbin_quantiles(data_2018_jobsat_na_dropped, JobSat, SalaryUSD, 20)
print(bin_salary)
salary_bins <- c("<5028", "5028-9228", "9228-13620", "13620-18996", "18996-24648", "24648-30384", "30384-36444", 
                 "36444-42036", "42036-47996", "47996-54049", "54049-60000", "60000-66716", "66716-75000", "75000-84000", 
                 "84000-95000", "95000-110000", "110000-129588", "129588-160000", "160000-275000", ">275000")
salary_bin_cut_points <- c(0, 5028, 9228, 13620, 18996, 24648, 30384, 36444, 42036, 47996, 54049, 
                           60000, 66716, 75000, 84000, 95000, 110000, 129588, 160000, 275000, Inf)
data_2018_jobsat_na_dropped$SalaryUSDBinned <- cut(data_2018_jobsat_na_dropped$SalaryUSD, breaks = salary_bin_cut_points, 
                                                   labels = salary_bins, right = FALSE)
head(data_2018_jobsat_na_dropped[, c('SalaryUSD', 'SalaryUSDBinned')], 10)

## Checking feature importance after binning 
temp_df <- data_2018_jobsat_na_dropped[, !(names(data_2018_jobsat_na_dropped) %in% c('SalaryUSD'))]
temp_feature_selection <- information_gain_feature_importance(temp_df, "JobSat")
print(temp_feature_selection)

data_2018_binned <- data_2018_jobsat_na_dropped[, !(names(data_2018_jobsat_na_dropped) %in% c('SalaryUSD'))]  %>% mutate_if(is.factor, as.character)
str(data_2018_binned)

######################################### 2018 Binning Features Completed #######################################################

######################################### 2018 Engineer Job Factors #############################################################

#AssessJob refers to JobFactors question in 2019 and 2020 survey 

data_2018_feature_engineering <- data_2018_binned
data_2018_feature_engineering$JobFactors <- ""

AssessJob1 <- 'Diversity of the company or organization'
AssessJob2 <- 'Office environment or company culture'
AssessJob3 <- 'Languages, frameworks, and other technologies I’d be working with'
AssessJob4 <- 'Industry that I’d be working in'
AssessJob5 <- 'Specific department or team I’d be working on'
AssessJob6 <- 'Financial performance or funding status of the company or organization'
AssessJob7 <- 'How widely used or impactful my work output would be'
AssessJob8 <- 'Opportunities for professional development'
AssessJob9 <- 'Remote work options'
AssessJob10 <- 'Benefits'


engineer_job_factor_function <- function(x){
  new_value <- ""
  if(x$AssessJob1 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Diversity of the company or organization;', sep='')
  } 
  
  if(x$AssessJob2 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Office environment or company culture;', sep='')
  } 
  
  if(x$AssessJob3 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Languages, frameworks, and other technologies I’d be working with;', sep='')
  } 
  
  if(x$AssessJob4 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Industry that I’d be working in;', sep='')
  } 
  
  if(x$AssessJob5 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Specific department or team I’d be working on;', sep='')
  } 
  
  if(x$AssessJob6 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Financial performance or funding status of the company or organization;', sep='')
  } 
  
  if(x$AssessJob7 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'How widely used or impactful my work output would be;', sep='')
  } 
  
  if(x$AssessJob8 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Opportunities for professional development;', sep='')
  } 
  
  if(x$AssessJob9 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Remote work options;', sep='')
  } 
  
  if(x$AssessJob10 %in% c(1, 2, 3)){
    new_value <- paste(new_value, 'Benefits;', sep='')
  } 
  
  if(new_value == ""){
    new_value == "Missing"
  }
  
  return(new_value)
}

#First row should have assessJob 4, 5 and 7 

for (i in 1:nrow(data_2018_feature_engineering)){
  data_2018_feature_engineering[i, "JobFactors"] <- engineer_job_factor_function(data_2018_feature_engineering[i, ])
}

head(data_2018_feature_engineering$JobFactors)
  
head(data_2018_feature_engineering[, (names(data_2018_feature_engineering) %in% c('AssessJob1', 'AssessJob2', 'AssessJob3', 
                                                                                  'AssessJob4', 'AssessJob5','AssessJob6', 'AssessJob7', 
                                                                                  'AssessJob8', 'AssessJob9', 'AssessJob10'))])


cols_to_drop <- c('AssessJob1', 'AssessJob2', 'AssessJob3', 'AssessJob4', 'AssessJob5','AssessJob6', 'AssessJob7', 'AssessJob8', 
                  'AssessJob9', 'AssessJob10', 'AssessBenefits1', 'AssessBenefits2', 'AssessBenefits3', 'AssessBenefits4', 
                  'AssessBenefits5', 'AssessBenefits6', 'AssessBenefits7', 'AssessBenefits8', 'AssessBenefits9', 
                  'AssessBenefits10', 'AssessBenefits11', 'JobContactPriorities1', 'JobContactPriorities2', 
                  'JobContactPriorities3', 'JobContactPriorities4', 'JobContactPriorities5', 'JobEmailPriorities1', 
                  'JobEmailPriorities2', 'JobEmailPriorities3', 'JobEmailPriorities4', 'JobEmailPriorities5', 
                  'JobEmailPriorities6', 'JobEmailPriorities7')

data_2018_feature_engineering <- data_2018_feature_engineering[, !(names(data_2018_feature_engineering) %in% cols_to_drop)]

summary(data_2018_feature_engineering)

######################################### 2018 Missing Data Cleaning ############################################################

## Handling missing data by adding a new category to represent missing
data_2018_missing_replaced <- data_2018_feature_engineering %>% mutate_if(is.character, ~replace(., is.na(.), "Missing"))
str(data_2018_missing_replaced)
missmap(data_2018_missing_replaced, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="2018 - Final Missing Data Map")
na_feature_count(data_2018_missing_replaced)

temp_feature_selection <- information_gain_feature_importance(data_2018_missing_replaced, "JobSat")
print(temp_feature_selection)

######################################### 2018 Missing Data Cleaned ############################################################

## Reduce variables with high number of factor levels i.e. > 200
data_2018_high_factor_levels <- data_2018_missing_replaced %>% mutate_if(is.character, as.factor)
summary(data_2018_high_factor_levels)
## Save off cleaned data 
write.csv(data_2018_high_factor_levels, file = "datasets/interim/2018_clean_data_pre_factor_reduction.csv", row.names = FALSE)

######################################### 2018 Factor Level Reduction ##########################################################

data_2018_initial_clean <- data_2018_missing_replaced %>% mutate_if(is.character, as.factor)
str(data_2018_initial_clean)
cols_over_200_factor_levels <- find_cols_factors_over_200_levels(data_2018_initial_clean)
print(cols_over_200_factor_levels)



#################################### CLEANED 2018 DATA #######################################################################

##############################################################################################################################

#################################### CLEAN 2017 DATA #########################################################################

data_file_2017 = 'datasets/raw/2017_Survey_Results.csv'
raw_data_2017 <- read_csv(data_file_2017)

## Initial Cleaning
data_2019_clean_col_names <- clean_2019_column_names(raw_data_2019)
data_2019_dup_cols_rm <- remove_dup_cols_2019(data_2019_clean_col_names)
data_2019_clean_cols <- remove_stackoverflow_id_cols_2019(data_2019_dup_cols_rm)

data_2019_jobsat_na_dropped <- remove_job_sat_na(data_2019_clean_cols)

rm(data_2019_clean_col_names, data_2019_dup_cols_rm, data_2019_clean_cols)

## Check missing data 
na_feature_count(data_2019_jobsat_na_dropped)
missmap(data_2019_jobsat_na_dropped, col=c("black", "grey"), legend=FALSE, margins=c(10, 5), main="2019 - Initial Missing Data Map")

################################## 2017 - Missing Data Trends Analysis ########################################################





