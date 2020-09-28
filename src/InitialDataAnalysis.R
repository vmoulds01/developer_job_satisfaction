install.packages('sjPlot')
install.packages('sjmisc')
install.packages('parameters')

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(sjPlot)
library(sjmisc)
library(parameters)

#### Read in the datasets

data_file_2020 = 'datasets/raw/2020_Survey_Results.csv'
raw_data_2020 <- read_csv(data_file_2020)

data_file_2019 = 'datasets/raw/2019_Survey_Results.csv'
raw_data_2019 <- read_csv(data_file_2019)

data_file_2018 = 'datasets/raw/2018_Survey_Results.csv'
raw_data_2018 <- read_csv(data_file_2018)

data_file_2017 = 'datasets/raw/2017_Survey_Results.csv'
raw_data_2017 <- read_csv(data_file_2017)

data_file_2016 = 'datasets/raw/2016_Survey_Results.csv'
raw_data_2016 <- read_csv(data_file_2016)

data_file_2015 = 'datasets/raw/2015v2_Survey_Results.csv'
raw_data_2015 <- read_csv(data_file_2015)

data_file_2014 = 'datasets/raw/2014_Survey_Results.csv'
raw_data_2014 <- read_csv(data_file_2014)

data_file_2013 = 'datasets/raw/2013_Survey_Results.csv'
raw_data_2013 <- read_csv(data_file_2013)

data_file_2012 = 'datasets/raw/2012_Survey_Results.csv'
raw_data_2012 <- read_csv(data_file_2012)

data_file_2011 = 'datasets/raw/2011_Survey_Results.csv'
raw_data_2011 <- read_csv(data_file_2011)

#### Completed reading the datasets 

#### Checking column names 

colnames(raw_data_2020)  
colnames(raw_data_2019)
colnames(raw_data_2018)
colnames(raw_data_2017)
colnames(raw_data_2016)
colnames(raw_data_2015)
colnames(raw_data_2014)
colnames(raw_data_2013)
colnames(raw_data_2012)
colnames(raw_data_2011)

head(raw_data_2017$DeveloperType)

str(as.factor(raw_data_2020$DevType))

# Job Satisfaction column names are:
#    JobSat - 2019, 2020
#    JobSatisfaction - 2018, 2017
#    job_satisfaction - 2016
#    Job Satisfaction - 2015
#    "What best describes your career / job satisfaction?" - 2012, 2013
#    "Please rate your job/career satisfaction" - 2011

#### Checked column names

#### Extract satisfaction proportion information

data_2020 <- data.frame(apply(raw_data_2020[, 'JobSat'], 2, as.factor))
summary(data_2020$JobSat)
data_2020 <- data_2020 %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Very satisfied'),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(JobSat %in% 
                              c('Slightly dissatisfied', 'Very dissatisfied'),
                            1,0))
satisfied_proportion_2020 <- sum(data_2020$Satisfied) / nrow(na.omit(data_2020))
satisfied_proportion_2020

data_2019 <- data.frame(apply(raw_data_2019[, 'JobSat'], 2, as.factor))
summary(data_2019$JobSat)
data_2019 <- data_2019 %>% 
  mutate(Satisfied = ifelse(JobSat %in% 
                              c('Slightly satisfied', 'Very satisfied'),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(JobSat %in% 
                                c('Slightly dissatisfied', 'Very dissatisfied'),
                              1,0))
satisfied_proportion_2019 <- sum(data_2019$Satisfied) / nrow(na.omit(data_2019))
satisfied_proportion_2019


data_2018 <- data.frame(apply(raw_data_2018[, 'JobSatisfaction'], 2, as.factor))
summary(data_2018$JobSatisfaction)
data_2018 <- data_2018 %>% 
  mutate(Satisfied = ifelse(JobSatisfaction %in% 
                              c('Slightly satisfied', 'Moderately satisfied', 'Extremely satisfied'),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(JobSatisfaction %in% 
                                c('Slightly dissatisfied', 'Moderately dissatisfied', 'Extremely dissatisfied'),
                              1,0))
satisfied_proportion_2018 <- sum(data_2018$Satisfied) / nrow(na.omit(data_2018))
satisfied_proportion_2018


data_2017 <- data.frame(apply(raw_data_2017[, 'JobSatisfaction'], 2, as.factor))
summary(data_2017$JobSatisfaction)
data_2017 <- data_2017 %>% 
  mutate(Satisfied = ifelse(JobSatisfaction %in% 
                              c(7, 8, 9, 10),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(JobSatisfaction %in% 
                                c(0, 1, 2, 3),
                              1,0))
satisfied_proportion_2017 <- sum(data_2017$Satisfied) / nrow(na.omit(data_2017))
satisfied_proportion_2017


data_2016 <- data.frame(apply(raw_data_2016[, 'job_satisfaction'], 2, as.factor))
summary(data_2016$job_satisfaction)
data_2016 <- data_2016 %>% 
  mutate(Satisfied = ifelse(job_satisfaction %in% 
                              c("I love my job", "I'm somewhat satisfied with my job"),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(job_satisfaction %in% 
                                c("I hate my job", "I'm somewhat dissatisfied with my job"),
                              1,0))
satisfied_proportion_2016 <- sum(data_2016$Satisfied) / nrow(na.omit(data_2016))
satisfied_proportion_2016

data_2015 <- data.frame(apply(raw_data_2015[, "Job Satisfaction"], 2, as.factor))
summary(data_2015)
data_2015 <- data_2015 %>% 
  mutate(Satisfied = ifelse(Job.Satisfaction %in% 
                              c("I love my job", "I'm somewhat satisfied with my job"),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(Job.Satisfaction %in% 
                                c("I hate my job", "I'm somewhat dissatisfied with my job"),
                              1,0))
satisfied_proportion_2015 <- sum(data_2015$Satisfied) / nrow(na.omit(data_2015))
satisfied_proportion_2015

data_2013 <- data.frame(apply(raw_data_2013[, "What best describes your career / job satisfaction?"], 2, as.factor))
summary(data_2013)
colnames(data_2013)
data_2013 <- data_2013 %>% 
  mutate(Satisfied = ifelse(What.best.describes.your.career...job.satisfaction. %in% 
                              c("Love my job", "I enjoy going to work"),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(What.best.describes.your.career...job.satisfaction. %in% 
                                c("It's a paycheck", "I'm not happy in my job"),
                              1,0))
satisfied_proportion_2013 <- sum(data_2013$Satisfied) / nrow(na.omit(data_2013))
satisfied_proportion_2013

data_2012 <- data.frame(apply(raw_data_2012[, "What best describes your career / job satisfaction?"], 2, as.factor))
summary(data_2012)
colnames(data_2012)
data_2012 <- data_2012 %>% 
  mutate(Satisfied = ifelse(What.best.describes.your.career...job.satisfaction. %in% 
                              c("Love my job", "I enjoy going to work"),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(What.best.describes.your.career...job.satisfaction. %in% 
                                c("It's a paycheck", "I'm not happy in my job"),
                              1,0))
satisfied_proportion_2012 <- sum(data_2012$Satisfied) / nrow(na.omit(data_2012))
satisfied_proportion_2012


data_2011 <- data.frame(apply(raw_data_2011[, "Please rate your job/career satisfaction"], 2, as.factor))
summary(data_2011)
colnames(data_2011)
data_2011 <- data_2011 %>% 
  mutate(Satisfied = ifelse(Please.rate.your.job.career.satisfaction %in% 
                              c("So happy it hurts", "I enjoy going to work"),
                            1,0)) %>% 
  mutate(Disatisfied = ifelse(Please.rate.your.job.career.satisfaction %in% 
                                c("It pays the bills", "I'm not happy in my job", "FML"),
                              1,0))
satisfied_proportion_2011 <- sum(data_2011$Satisfied) / nrow(na.omit(data_2011))
satisfied_proportion_2011


#### Completed extracting satisfaction proportion information

#### Plot variations in datasets

year <- c(2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011)
counts <- c(nrow(raw_data_2020), nrow(raw_data_2019), nrow(raw_data_2018), nrow(raw_data_2017), 
            nrow(raw_data_2016), nrow(raw_data_2015), nrow(raw_data_2014), nrow(raw_data_2013),
            nrow(raw_data_2012), nrow(raw_data_2011))
questions <- c(ncol(raw_data_2020), ncol(raw_data_2019), ncol(raw_data_2018), ncol(raw_data_2017), 
               ncol(raw_data_2016), ncol(raw_data_2015), ncol(raw_data_2014), ncol(raw_data_2013), 
               ncol(raw_data_2012), ncol(raw_data_2011))
na_counts <-  c(nrow(na.omit(raw_data_2020)), nrow(na.omit(raw_data_2019)), nrow(na.omit(raw_data_2018)), 
                nrow(na.omit(raw_data_2017)), nrow(na.omit(raw_data_2016)), nrow(na.omit(raw_data_2015)), 
                nrow(na.omit(raw_data_2014)), nrow(na.omit(raw_data_2013)), nrow(na.omit(raw_data_2012)), 
                nrow(na.omit(raw_data_2011)))
job_sat_counts <- c(nrow(raw_data_2020 %>% drop_na(JobSat)), 
                    nrow(raw_data_2019 %>% drop_na(JobSat)), 
                    nrow(raw_data_2018 %>% drop_na(JobSatisfaction)), 
                    nrow(raw_data_2017 %>% drop_na(JobSatisfaction)),
                    nrow(raw_data_2016 %>% drop_na(job_satisfaction)), 
                    nrow(raw_data_2015 %>% drop_na("Job Satisfaction")), 
                    0, 
                    nrow(raw_data_2013 %>% drop_na("What best describes your career / job satisfaction?")), 
                    nrow(raw_data_2012 %>% drop_na("What best describes your career / job satisfaction?")), 
                    nrow(raw_data_2011 %>% drop_na("Please rate your job/career satisfaction")))
satisfied_proportion <- c(satisfied_proportion_2020*100, satisfied_proportion_2019*100, satisfied_proportion_2018*100, 
                          satisfied_proportion_2017*100, satisfied_proportion_2016*100, satisfied_proportion_2015*100, 
                          0, satisfied_proportion_2013*100, satisfied_proportion_2012*100, satisfied_proportion_2011*100)

summary_survey_df <- data.frame(year, counts, questions, na_counts, job_sat_counts, satisfied_proportion) 
head(summary_survey_df ,10)

summary_survey_df_no_2014 <- summary_survey_df[c(1, 2, 3, 4, 5, 6, 8, 9, 10), ]
head(summary_survey_df_no_2014, 10)


# Plot survey responses and number of questions over the years
ggplot(summary_survey_df, aes(x=factor(year), y=counts, fill=questions)) +
  geom_bar(stat = 'identity') + 
  scale_fill_gradient2(low='white', mid='lightblue', high='darkblue', space='Lab') + 
  labs(title="Survey Question & Response Variation Over The Years", y="Survey Response Counts", x="Survey Year", fill="Number of Questions")



# Plot satisfied proportion over the years ignoring 2014
ggplot(summary_survey_df_no_2014, aes(x=year, y=job_sat_counts, fill=satisfied_proportion)) +
  geom_bar(stat = 'identity') + 
  geom_line(mapping = aes(x=year, y=counts), size = 1, color = "darkgray", show.legend = TRUE) + 
  scale_fill_gradient2(low='white', mid='yellow', high='darkblue', space='Lab') + 
  labs(title="Job Satisfaction Response Variation Over The Years", caption="Line refers to the total survey response counts while bars are job satisfaction counts", y="Response Counts", x="Survey Year", fill="Proportion Satisfied") + 
  coord_cartesian(xlim=c(2011, 2020), ylim=c(0, 100000))


#### Completed plots of variations in datasets

#### Plot variation in job satisfaction questions over the years 

## Step 1 - Get the data 
## Need to create year column 
## Need to update data so column names match 
## Then need to combine the data vertically 

## Adding year
data_2011$Year = 2011
data_2012$Year = 2012
data_2013$Year = 2013
data_2015$Year = 2015
data_2016$Year = 2016
data_2017$Year = 2017
data_2018$Year = 2018
data_2019$Year = 2019
data_2020$Year = 2020

## Updating column names to match
new_col_names = c('JobSat')
old_col_names_2011 = c('Please.rate.your.job.career.satisfaction')
old_col_names_2012_2013 = c('What.best.describes.your.career...job.satisfaction.')
old_col_names_2015 = c('Job.Satisfaction')
old_col_names_2016 = c('job_satisfaction')
old_col_names_2017_2018 = c('JobSatisfaction')


setnames(data_2011, old = old_col_names_2011, new = new_col_names)
setnames(data_2012, old = old_col_names_2012_2013, new = new_col_names)
setnames(data_2013, old = old_col_names_2012_2013, new = new_col_names)
setnames(data_2015, old = old_col_names_2015, new = new_col_names)
setnames(data_2016, old = old_col_names_2016, new = new_col_names)
setnames(data_2017, old = old_col_names_2017_2018, new = new_col_names)
setnames(data_2018, old = old_col_names_2017_2018, new = new_col_names)

## Then need to combine the data vertically

total_df <- rbind(data_2011, data_2012)
total_df <- rbind(total_df, data_2013)
total_df <- rbind(total_df, data_2015)
total_df <- rbind(total_df, data_2016)
total_df <- rbind(total_df, data_2017)
total_df <- rbind(total_df, data_2018)
total_df <- rbind(total_df, data_2019)
total_df <- rbind(total_df, data_2020)

total_df$JobSatYear <- paste(total_df$Year, total_df$JobSat, sep='-')

## Step 2 - Plot combined data in a reverse bar plot 
ggplot(total_df, aes(fill=Year)) + 
  geom_bar(aes(y = JobSatYear), position = position_stack(reverse = TRUE)) 

df_early_years <- rbind(data_2011, data_2012)
df_early_years <- rbind(df_early_years, data_2013)
df_early_years <- rbind(df_early_years, data_2015)
df_early_years <- rbind(df_early_years, data_2016)

## Create new column that combines JobSat and Year 
df_early_years$JobSatYear <- as.factor(paste(df_early_years$Year, df_early_years$JobSat, sep='-'))

# Drop response level as it shouldn't be in the data
df_early_years <- df_early_years[df_early_years$JobSat!='Response',]
levels(droplevels(df_early_years$JobSatYear))

# Remove na from df 
df_early_years <- na.omit(df_early_years)

# Create new column that holds satisfied, disatisfied and neutral information 
df_early_years <- df_early_years %>% 
  mutate(SatisfactionRating = ifelse(Satisfied %in% 
                              c(1),
                            "Satisfied",NA)) %>% 
  mutate(SatisfactionRating = ifelse(Disatisfied %in% 
                                c(1),
                              "Disatisfied",SatisfactionRating))
df_early_years$SatisfactionRating[is.na(df_early_years$SatisfactionRating)] <- "Other"

head(df_early_years)

## Plot combined data in a reverse bar plot where colour of bars represents year
ggplot(df_early_years, aes(fill=Year)) + 
  geom_bar(aes(y = reorder(JobSatYear, table(JobSatYear)[JobSatYear])), position = position_stack(reverse = TRUE)) + 
  labs(title="Job Satisfaction Detailed Response Between 2011 and 2016", y="Satisfaction Response per year", x="Counts", fill="Year") 
  
## Plot combined data in a reverse bar plot where colour of bars represents satisfaction rating
ggplot(df_early_years, aes(fill=SatisfactionRating)) + 
  geom_bar(aes(y = reorder(JobSatYear, table(JobSatYear)[JobSatYear])), position = position_stack(reverse = TRUE)) + 
  labs(title="Job Satisfaction Detailed Response Between 2011 and 2016", y="Satisfaction Response per year", x="Counts", fill="Satisfaction Category") 



df_later_years <- rbind(data_2017, data_2018)
df_later_years <- rbind(df_later_years, data_2019)
df_later_years <- rbind(df_later_years, data_2020)

## Create new column that combines JobSat and Year 
df_later_years$JobSatYear <- as.factor(paste(df_later_years$Year, df_later_years$JobSat, sep='-'))

# Remove na from df 
df_later_years <- na.omit(df_later_years)

# Create new column that holds satisfied, disatisfied and neutral information 
df_later_years <- df_later_years %>% 
  mutate(SatisfactionRating = ifelse(Satisfied %in% 
                                       c(1),
                                     "Satisfied",NA)) %>% 
  mutate(SatisfactionRating = ifelse(Disatisfied %in% 
                                       c(1),
                                     "Disatisfied",SatisfactionRating))
df_later_years$SatisfactionRating[is.na(df_later_years$SatisfactionRating)] <- "Other"

head(df_later_years)

## Plot data in a reverse bar plot where colour of bars represents year
ggplot(df_later_years, aes(fill=Year)) + 
  geom_bar(aes(y = reorder(JobSatYear, table(JobSatYear)[JobSatYear])), position = position_stack(reverse = TRUE)) + 
  labs(title="Job Satisfaction Detailed Response Between 2017 and 2020", y="Satisfaction Response per year", x="Counts", fill="Year") 

## Plot data in a reverse bar plot where colour of bars represents satisfaction
ggplot(df_later_years, aes(fill=SatisfactionRating)) + 
  geom_bar(aes(y = reorder(JobSatYear, table(JobSatYear)[JobSatYear])), position = position_stack(reverse = TRUE)) + 
  labs(title="Job Satisfaction Detailed Response Between 2017 and 2020", y="Satisfaction Response per year", x="Counts", fill="Satisfaction Category") 


#### Completed plot showing variation in job satisfaction questions over the years


#### Saving dataframes created to csv for later use

write.csv(summary_survey_df, file = "datasets/interim/sat_survey_summary.csv", row.names = FALSE)
write.csv(total_df, file = "datasets/interim/sat_details_all_years.csv", row.names = FALSE)
write.csv(df_early_years, file = "datasets/interim/sat_rating_early_years.csv", row.names = FALSE)
write.csv(df_later_years, file = "datasets/interim/sat_rating_later_years.csv", row.names = FALSE)

#### FURTHER THOUGHTS
#### Is there a significant difference in proportion of satisfied devs over the years?
#### Is there a significant different in proportion of satisfied devs per country?
#### Proportion stats so could use chi2 to look at differences here 