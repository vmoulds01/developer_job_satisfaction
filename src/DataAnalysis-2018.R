library(caret)
library(DMwR)
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
library(kknn)


cleaned_data_file_2018 = 'datasets/interim/2018_clean_data_pre_factor_reduction.csv'
clean_data_2018 <- read_csv(cleaned_data_file_2018)
clean_data_2018 <- clean_data_2018 %>% mutate_if(is.character, as.factor)
summary(clean_data_2018)

initial_information_gain_ratio <- information_gain_ratio_feature_importance(clean_data_2018)
print(initial_information_gain_ratio)

summary(clean_data_2018$JobSat)
data_satisfied <- clean_data_2018 %>% mutate(Satisfied = ifelse(JobSat %in% c('Slightly satisfied', 'Moderately satisfied', 'Extremely satisfied'), "Yes","No"))
data_satisfied <- data_satisfied[, !(names(data_satisfied) %in% c('JobSat'))]

satisfied_information_gain_ratio <- information_gain_ratio_target(data_satisfied, 'Satisfied')
print(satisfied_information_gain_ratio)


############################################################# PLOTS ###################################################################################

####################################################### PLOT JOB SATISFACTION #########################################################################

ggplot(clean_data_2018) + 
  geom_bar(aes(y = reorder(JobSat, table(JobSat)[JobSat])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Job Satisfaction 2018", y="Satisfaction Response", x="Counts") 

######################################################### PLOT SATISFACTION ###########################################################################

ggplot(data_satisfied, aes(x = Satisfied)) + 
  geom_bar(position = position_dodge(width=0.5), fill="lightblue", color="darkblue") + 
  geom_text(aes(label=scales::percent(stat(prop)), group=1), stat='count', vjust = -.5) +
  labs(title="Satisfied Distribution 2018", x="Satisfied", y="Counts") 

###################################################### PLOT JOB SEEK VS SATISFIED ######################################################################

unique(data_satisfied$JobSeek)

data_satisfied$JobSeek <- gsub("I am actively looking for a job", "Actively Looking", data_satisfied$JobSeek)
data_satisfied$JobSeek <- gsub("I am not interested in new job opportunities", "Not Interested", data_satisfied$JobSeek)
data_satisfied$JobSeek <- gsub("I’m not actively looking, but I am open to new opportunities", "Not Actively Looking", data_satisfied$JobSeek)

ggplot(data=data_satisfied, aes(x=reorder(JobSeek, table(JobSeek)[JobSeek]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Job Seek Variance For Satisfied", y="Counts", x="Job Seek", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

level_order <- c('Not Interested', 'Not Actively Looking', 'Actively Looking', 'Missing')


ggplot(data_satisfied, aes(x =factor(JobSeek, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Job Seek Category", y="Satisfied Proportion", x="Job Seek Category", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

###################################################### FREQUENCY RESPONSE PER COUNTRY ###################################################################
summary(data_satisfied$Country)

data_satisfied_country <- data_satisfied %>%
  select(Country, Satisfied) %>%                      # Select the two columns of interest
  rename(region = Country)

# Replace "United States of America" by USA in the region column
data_satisfied_country$region <- gsub("United States", "USA", data_satisfied_country$region)

head(data_satisfied_country)

data_world_summary <- data_satisfied_country %>% group_by(region) %>% summarise(counts = n())

head(data_world_summary)

world_map <- map_data("world")
world_data_frequency <- left_join(data_world_summary, world_map, by = "region")
ggplot(world_data_frequency, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=counts), colour = "blue") + 
  scale_fill_viridis_c(option = "C") + 
  labs(title = "Responses per country", fill="Responses")

###################################################### PLOT COUNTRY VS SATISFIED #######################################################################

data_world_sat_summary <- data_satisfied_country %>% group_by(region, Satisfied) %>% summarise(counts = n())

head(data_world_sat_summary)

data_world_sat_summary <- data_world_sat_summary %>% 
  group_by(region) %>% 
  mutate(SatisfiedProportion = case_when(Satisfied == 'No'  ~ 0, 
                                         Satisfied == 'Yes' ~ counts / sum(counts))) %>%
  ungroup() %>%
  filter(SatisfiedProportion != 0)

head(data_world_sat_summary)

world_map <- map_data("world")
world_data_sat_proportion <- left_join(data_world_sat_summary, world_map, by = "region")
ggplot(world_data_sat_proportion, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=SatisfiedProportion), colour = "blue") + 
  scale_fill_viridis_c(option = "C") + 
  labs(title = "Proportion Satisfied Per Country 2018")

head(data_world_sat_summary[order(data_world_sat_summary$SatisfiedProportion),], 20)
tail(data_world_sat_summary[order(data_world_sat_summary$SatisfiedProportion),], 20)

######################################################## PLOT AGE VS SATISFIED #########################################################################

unique(data_satisfied$Age)

data_satisfied$Age <- gsub("Under 18 years old", "Under 18", data_satisfied$Age)
data_satisfied$Age <- gsub("18 - 24 years old", "18 to 24", data_satisfied$Age)
data_satisfied$Age <- gsub("25 - 34 years old", "25 to 34", data_satisfied$Age)
data_satisfied$Age <- gsub("35 - 44 years old", "35 to 44", data_satisfied$Age)
data_satisfied$Age <- gsub("45 - 54 years old", "45 to 54", data_satisfied$Age)
data_satisfied$Age <- gsub("55 - 64 years old", "55 to 64", data_satisfied$Age)
data_satisfied$Age <- gsub("65 years or older", "Over 65", data_satisfied$Age)

level_order <- c('Under 18', '18 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 64', 'Over 65', 'Missing')

ggplot(data=data_satisfied, aes(x=reorder(Age, table(Age)[Age]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Age Variance Satisfication", y="Counts", x="Age", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x =factor(Age, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Job Seek Category", y="Satisfied Proportion", x="Age", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

###################################################### PLOT GENDER VS SATISFIED #########################################################################

unique(data_satisfied$Gender)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Gender, table(Gender)[Gender])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Gender Distribution 2018", y="Gender Response", x="Counts") 

gender_data_satisfied <- data_satisfied[data_satisfied$Gender %in% c("Male", "Female"), ]

summary(gender_data_satisfied$Gender)

ggplot(data=gender_data_satisfied, aes(x=reorder(Gender, table(Gender)[Gender]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Gender Variance Satisfication", y="Counts", x="Gender", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(gender_data_satisfied, aes(x =reorder(Gender, table(Gender)[Gender]), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Gender", y="Satisfied Proportion", x="Gender", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

###################################################### PLOT SALARY VS SATISFIED #########################################################################

unique(data_satisfied$SalaryUSDBinned)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(SalaryUSDBinned, table(SalaryUSDBinned)[SalaryUSDBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Salary Distribution 2018", y="Salaray Response", x="Counts") 

salary_data_satisfied <- data_satisfied[!(data_satisfied$SalaryUSDBinned %in% c("Missing")), ]
summary(salary_data_satisfied$SalaryUSDBinned)

ggplot(salary_data_satisfied) + 
  geom_bar(aes(y = reorder(SalaryUSDBinned, table(SalaryUSDBinned)[SalaryUSDBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Salary Distribution 2018", y="Salaray Response", x="Counts") 

level_order <- c('<5028', '5028-9228', '9228-13620', '13620-18996', '18996-24648', '24648-30384', '30384-36444', '36444-42036', '42036-47996', 
                 '47996-54049', '54049-60000', '60000-66716', '66716-75000', '75000-84000', '84000-95000', '95000-110000', '110000-129588', 
                 '129588-160000', '160000-275000', '>275000', 'Missing')

ggplot(data=salary_data_satisfied, aes(x=factor(SalaryUSDBinned, level=level_order), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Salary Variance Satisfication", y="Counts", x="Salary USD", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(salary_data_satisfied, aes(x = factor(SalaryUSDBinned, level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Salary USD 2018", y="Satisfied Proportion", x="Salaray USD", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

#################################################### PLOT EMPLOYMENT VS SATISFIED #####################################################################

unique(data_satisfied$Employment)

data_satisfied$Employment <- gsub("Employed full-time", "Full-time", data_satisfied$Employment)
data_satisfied$Employment <- gsub("Employed part-time", "Part-time", data_satisfied$Employment)
data_satisfied$Employment <- gsub("Independent contractor, freelancer, or self-employed", "Independent", data_satisfied$Employment)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Employment, table(Employment)[Employment])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Employment Distribution 2018", y="Employment Response", x="Counts") 

level_order <- c('Full-time', 'Independent', 'Part-time')

ggplot(data=data_satisfied, aes(x=factor(Employment, level=level_order), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Employment Variance Satisfication", y="Counts", x="Employment", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(Employment, level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Employment Type", y="Satisfied Proportion", x="Employment Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

###################################################### PLOT STUDENT VS SATISFIED #####################################################################

summary(data_satisfied$Student)

data_satisfied$Student <- gsub("Yes, full-time", "Full-time", data_satisfied$Student)
data_satisfied$Student <- gsub("Yes, part-time", "Part-time", data_satisfied$Student)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Student, table(Student)[Student])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Student Distribution 2018", y="Student Response", x="Counts") 

ggplot(data=data_satisfied, aes(x=reorder(Student, table(Student)[Student]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Student Variance Satisfication", y="Counts", x="Student", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(Student), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Student Type", y="Satisfied Proportion", x="Student Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

#################################################### PLOT COMPANY SIZE VS SATISFIED #####################################################################

unique(data_satisfied$CompanySize)

data_satisfied$CompanySize <- gsub("Fewer than 10 employees", "<10", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("10 to 19 employees", "10 to 19", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("20 to 99 employees", "20 to 99", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("100 to 499 employees", "100 to 499", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("500 to 999 employees", "500 to 999", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("1,000 to 4,999 employees", "1,000 to 4,999", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("5,000 to 9,999 employees", "5,000 to 9,999", data_satisfied$CompanySize)
data_satisfied$CompanySize <- gsub("10,000 or more employees", ">10,000", data_satisfied$CompanySize)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(CompanySize, table(CompanySize)[CompanySize])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="CompanySize Distribution 2018", y="CompanySize Response", x="Counts") 

level_order <- c('<10', '10 to 19', '20 to 99', '100 to 499', '500 to 999', '1,000 to 4,999', '5,000 to 9,999', '>10,000', 'Missing')

ggplot(data=data_satisfied, aes(x=factor(CompanySize, level=level_order), fill=factor(Satisfied))) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="CompanySize Vs Satisfication", y="Counts", x="CompanySize", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(CompanySize, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Company Size", y="Satisfied Proportion", x="CompanySize", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


##################################################### PLOT DEVTYPE VS SATISFIED #########################################################################

str(data_satisfied$DevType)

data_satisfied_dev_type <- data_satisfied %>% 
  select(Satisfied, DevType) %>%
  mutate(isExecutive = ifelse(str_count(DevType, 'executive') > 0, 1, 0)) %>%
  mutate(isManager = ifelse(str_count(DevType, 'manager') > 0, 1, 0)) %>%
  mutate(isAnalyst = ifelse(str_count(DevType, 'analyst') > 0, 1, 0)) %>%
  mutate(isDataScientist = ifelse(str_count(DevType, 'scientist') > 0, 1, 0)) %>% 
  mutate(isAdmin = ifelse(str_count(DevType, 'administrator') > 0, 1, 0)) %>% 
  mutate(isDesigner = ifelse(str_count(DevType, 'Designer') > 0, 1, 0)) %>% 
  mutate(isDevOps = ifelse(str_count(DevType, 'DevOps') > 0, 1, 0)) %>% 
  mutate(isSales = ifelse(str_count(DevType, 'sales') > 0, 1, 0)) %>% 
  mutate(isOther = ifelse(str_count(DevType, 'None') > 0, 1, 0)) %>% 
  mutate(isDeveloper = ifelse(str_count(DevType, 'developer') > 0, 1, 0)) %>%
  mutate(DeveloperType = case_when(str_count(DevType, 'executive') > 0 ~ "Executive", 
                                  str_count(DevType, 'manager') > 0 ~ "Manager",
                                  str_count(DevType, 'academic') > 0 ~ "Academic/Researcher",
                                  str_count(DevType, 'analyst') > 0 & str_count(DevType, 'developer') == 0 ~ "Data Analyst/Scientist",
                                  str_count(DevType, 'scientist') > 0 ~ "Data Analyst/Scientist",
                                  str_count(DevType, 'administrator') > 0 & str_count(DevType, 'developer') == 0 ~ "Administrator",
                                  str_count(DevType, 'Designer') > 0 & str_count(DevType, 'developer') == 0 ~ "Designer",
                                  str_count(DevType, 'DevOps') > 0 ~ "DevOps",
                                  str_count(DevType, 'sales') > 0 ~ "Sales",
                                  str_count(DevType, 'None') > 0 ~ "Other",
                                  DevType == 'Missing' ~ "Missing",
                                  TRUE ~ "Developer"
                                  ))

head(data_satisfied_dev_type[, c('DevType', 'DeveloperType')], 20)

summary(data_satisfied_dev_type)

data_developer_types <- data_satisfied_dev_type[, !(names(data_satisfied_dev_type) %in% c('DeveloperType', 'DevType', 'Satisfied'))]

data_developer_types <- data.frame(colname = names(data_developer_types), counts = colSums(data_developer_types))

head(data_developer_types)

ggplot(data_developer_types, aes(x=colname, y=counts)) + 
  geom_bar(stat='identity', fill="lightblue", color="darkblue") + 
  labs(title="Developer Type Distribution 2018", x="Developer Types", y="Counts") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_dev_type) + 
  geom_bar(aes(y = reorder(DeveloperType, table(DeveloperType)[DeveloperType])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Developer Type Distribution 2018", y="Developer Type Response", x="Counts") 

ggplot(data_satisfied_dev_type, aes(x = factor(DeveloperType), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Developer Type", y="Satisfied Proportion", x="Developer Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

##################################################### PLOT LANGUAGE VS SATISFIED ########################################################################

unique(data_satisfied$LanguageDesireNextYear)

head(data_satisfied[, c('LanguageWorkedWith', 'LanguageDesireNextYear')], 20)

data_satisfied_lang_type <- data_satisfied %>% 
  select(Satisfied, LanguageWorkedWith, LanguageDesireNextYear) %>% 
  mutate(LanguageWorkedWith = as.character(LanguageWorkedWith)) %>% 
  mutate(LanguageDesireNextYear = as.character(LanguageDesireNextYear)) 

data_satisfied_lang_type$LanguageWorkedWithInDesired <- FALSE
data_satisfied_lang_type$LanguageDesireInWorkedWith <- FALSE
data_satisfied_lang_type$NoLanguagesDesire <- 0
data_satisfied_lang_type$NoLanguagesWorkedWith <- 0


for (row in 1:nrow(data_satisfied_lang_type)){
  LangWorkedWith <- data_satisfied_lang_type[row, 'LanguageWorkedWith']
  LangDesire <- data_satisfied_lang_type[row, 'LanguageDesireNextYear']
  data_satisfied_lang_type[row, 'LanguageWorkedWithInDesired'] <- grepl(LangWorkedWith, LangDesire, fixed=TRUE)
  data_satisfied_lang_type[row, 'LanguageDesireInWorkedWith'] <- grepl(LangDesire, LangWorkedWith, fixed=TRUE)
  data_satisfied_lang_type[row, 'NoLanguagesDesire'] <- str_count(data_satisfied_lang_type[row, 'LanguageDesireNextYear'], ';') + 1
  data_satisfied_lang_type[row, 'NoLanguagesWorkedWith'] <- str_count(data_satisfied_lang_type[row, 'LanguageWorkedWith'], ';') + 1
}

data_satisfied_lang_type$NoNewLanguagesDesire <- data_satisfied_lang_type$NoLanguagesDesire - data_satisfied_lang_type$NoLanguagesWorkedWith

head(data_satisfied_lang_type, 10)

summary(data_satisfied_lang_type)

ggplot(data_satisfied_lang_type, aes(x = factor(LanguageWorkedWithInDesired), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied When Language Working With Is Desired", y="Satisfied Proportion", x="Language Working With In Desired", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_lang_type, aes(x = factor(LanguageDesireInWorkedWith), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied When Language Desired Matches Working With", y="Satisfied Proportion", x="Language Desired Matches Current Work", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_lang_type, aes(x = factor(NoLanguagesDesire), fill = factor(Satisfied))) +
  geom_bar(position=position_stack(reverse = TRUE)) +
  labs(title="Satisfied Vs Number Languages Desire", y="Counts", x="Number Languages Desire", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45)) 

data_satisfied_languages_desire <- data_satisfied %>% 
  select(Satisfied, LanguageDesireNextYear) %>%
  mutate(isAssembly = ifelse(str_count(LanguageDesireNextYear, 'Assembly') > 0, 1, 0)) %>% 
  mutate(isBash = ifelse(str_count(LanguageDesireNextYear, 'Bash') > 0, 1, 0)) %>%
  mutate(isPython = ifelse(str_count(LanguageDesireNextYear, 'Python') > 0, 1, 0)) %>%
  mutate(isJava = ifelse(str_count(LanguageDesireNextYear, 'Java') > 0, 1, 0)) %>%
  mutate(isJavaScript = ifelse(str_count(LanguageDesireNextYear, 'JavaScript') > 0, 1, 0)) %>%
  mutate(isGo = ifelse(str_count(LanguageDesireNextYear, 'Go') > 0, 1, 0)) %>% 
  mutate(isCoffeeScript = ifelse(str_count(LanguageDesireNextYear, 'CoffeeScript') > 0, 1, 0)) %>% 
  mutate(isErlang = ifelse(str_count(LanguageDesireNextYear, 'Erlang') > 0, 1, 0)) %>% 
  mutate(isMatlab = ifelse(str_count(LanguageDesireNextYear, 'Matlab') > 0, 1, 0)) %>% 
  mutate(isHaskell = ifelse(str_count(LanguageDesireNextYear, 'Haskell') > 0, 1, 0)) %>% 
  mutate(isJulia = ifelse(str_count(LanguageDesireNextYear, 'Julia') > 0, 1, 0)) %>% 
  mutate(isKotlin = ifelse(str_count(LanguageDesireNextYear, 'Kotlin') > 0, 1, 0)) %>% 
  mutate(isRust = ifelse(str_count(LanguageDesireNextYear, 'Rust') > 0, 1, 0)) %>% 
  mutate(isSQL = ifelse(str_count(LanguageDesireNextYear, 'SQL') > 0, 1, 0))

data_languages_desire <- data_satisfied_languages_desire[, !(names(data_satisfied_languages_desire) %in% c('LanguageDesireNextYear', 'Satisfied'))]
data_languages_desire <- data.frame(colname = names(data_languages_desire), counts = colSums(data_languages_desire))

head(data_languages_desire)

ggplot(data_languages_desire, aes(x=colname, y=counts)) + 
  geom_bar(stat='identity', fill="lightblue", color="darkblue") + 
  labs(title="Language Desire Distribution 2018", x="Languages Desire", y="Counts") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_languages_desire, aes(x = factor(isBash), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied If Bash Language Desired", y="Satisfied Proportion", x="Bash Language Desire", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################### PLOT YEARS CODE VS SATISFIED ########################################################################

unique(data_satisfied$YearsCode)

data_satisfied$YearsCode <- gsub("0-2 years", "0 to 2", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("3-5 years", "3 to 5", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("6-8 years", "6 to 8", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("9-11 years", "9 to 11", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("12-14 years", "12 to 14", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("15-17 years", "15 to 17", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("18-20 years", "18 to 20", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("21-23 years", "21 to 23", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("24-26 years", "24 to 26", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("27-29 years", "27 to 29", data_satisfied$YearsCode)
data_satisfied$YearsCode <- gsub("30 or more years", ">30", data_satisfied$YearsCode)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(YearsCode, table(YearsCode)[YearsCode])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="YearsCode Distribution 2018", y="YearsCode Response", x="Counts") 

level_order <- c('0 to 2', '3 to 5', '6 to 8', '9 to 11', '12 to 14', '15 to 17', '18 to 20', '21 to 23', 
                 '24 to 26', '27 to 29', '>30', 'Missing')

ggplot(data_satisfied, aes(x = factor(YearsCode, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Years Coding 2018", y="Satisfied Proportion", x="Years Code", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))


################################################# PLOT YEARS CODE PRO VS SATISFIED ######################################################################

unique(data_satisfied$YearsCodePro)

data_satisfied$YearsCodePro <- gsub("0-2 years", "0 to 2", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("3-5 years", "3 to 5", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("6-8 years", "6 to 8", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("9-11 years", "9 to 11", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("12-14 years", "12 to 14", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("15-17 years", "15 to 17", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("18-20 years", "18 to 20", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("21-23 years", "21 to 23", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("24-26 years", "24 to 26", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("27-29 years", "27 to 29", data_satisfied$YearsCodePro)
data_satisfied$YearsCodePro <- gsub("30 or more years", ">30", data_satisfied$YearsCodePro)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(YearsCodePro, table(YearsCodePro)[YearsCodePro])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Years Code Pro Distribution 2018", y="Years Code Pro Response", x="Counts") 

level_order <- c('0 to 2', '3 to 5', '6 to 8', '9 to 11', '12 to 14', '15 to 17', '18 to 20', '21 to 23', 
                 '24 to 26', '27 to 29', '>30', 'Missing')

ggplot(data_satisfied, aes(x = factor(YearsCodePro, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Years Coding Professional", y="Satisfied Proportion", x="Years Code Professional", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


##################################################### PLOT WAKE TIME VS SATISFIED #######################################################################

unique(data_satisfied$WakeTime)

data_satisfied$WakeTime <- gsub("Before 5:00 AM", "< 5AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 5:00 - 6:00 AM", "5AM to 6AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 6:01 - 7:00 AM", "6AM to 7AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 7:01 - 8:00 AM", "7AM to 8AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 8:01 - 9:00 AM", "8AM to 9AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 9:01 - 10:00 AM", "9AM to 10AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 10:01 - 11:00 AM", "10AM to 11AM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("Between 11:01 AM - 12:00 PM", "11AM to 12PM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("After 12:01 PM", ">12PM", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("I work night shifts", "Night Shift", data_satisfied$WakeTime)
data_satisfied$WakeTime <- gsub("I do not have a set schedule", "No Schedule", data_satisfied$WakeTime)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(WakeTime, table(WakeTime)[WakeTime])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Wake-up Time Distribution 2018", y="Wake-up Time Response", x="Counts") 

level_order <- c('< 5AM', '5AM to 6AM', '6AM to 7AM', '7AM to 8AM', '8AM to 9AM', '9AM to 10AM', '10AM to 11AM', '11AM to 12PM', 
                 '>12PM', 'Night Shift', 'No Schedule', 'Missing')

ggplot(data_satisfied, aes(x = factor(WakeTime, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Wake-up Time", y="Satisfied Proportion", x="Wake-up Time", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

################################################# PLOT HOURS COMPUTER VS SATISFIED ######################################################################

unique(data_satisfied$HoursComputer)

data_satisfied$HoursComputer <- gsub("Less than 1 hour", "< 1", data_satisfied$HoursComputer)
data_satisfied$HoursComputer <- gsub("1 - 4 hours", "1 to 4", data_satisfied$HoursComputer)
data_satisfied$HoursComputer <- gsub("5 - 8 hours", "5 to 8", data_satisfied$HoursComputer)
data_satisfied$HoursComputer <- gsub("9 - 12 hours", "9 to 12", data_satisfied$HoursComputer)
data_satisfied$HoursComputer <- gsub("Over 12 hours", "> 12", data_satisfied$HoursComputer)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(HoursComputer, table(HoursComputer)[HoursComputer])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Computer Hours Distribution 2018", y="No. Hours Computer Response", x="Counts") 

level_order <- c('< 1', '1 to 4', '5 to 8', '9 to 12', '> 12', 'Missing')

ggplot(data_satisfied, aes(x = factor(HoursComputer, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Hours At Computer 2018", y="Satisfied Proportion", x="No. Hours at Computer", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))


################################################## PLOT HOURS OUTSIDE VS SATISFIED ######################################################################

unique(data_satisfied$HoursOutside)

data_satisfied$HoursOutside <- gsub("Less than 30 minutes", "< 0.5", data_satisfied$HoursOutside)
data_satisfied$HoursOutside <- gsub("30 - 59 minutes", "0.5 to 1", data_satisfied$HoursOutside)
data_satisfied$HoursOutside <- gsub("1 - 2 hours", "1 to 2", data_satisfied$HoursOutside)
data_satisfied$HoursOutside <- gsub("3 - 4 hours", "3 to 4", data_satisfied$HoursOutside)
data_satisfied$HoursOutside <- gsub("Over 4 hours", "> 4", data_satisfied$HoursOutside)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(HoursOutside, table(HoursOutside)[HoursOutside])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Hours Spent Outside Distribution 2018", y="No. Hours Outside Response", x="Counts") 

level_order <- c('< 0.5', '0.5 to 1', '1 to 2', '3 to 4', '> 4', 'Missing')

ggplot(data_satisfied, aes(x = factor(HoursOutside, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Hours Spent Outside 2018", y="Satisfied Proportion", x="No. Hours Outside", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################### PLOT SKIP MEALS VS SATISFIED ########################################################################

unique(data_satisfied$SkipMeals)

data_satisfied$SkipMeals <- gsub("1 - 2 times per week", "1-2 per week", data_satisfied$SkipMeals)
data_satisfied$SkipMeals <- gsub("3 - 4 times per week", "3-4 per week", data_satisfied$SkipMeals)
data_satisfied$SkipMeals <- gsub("Daily or almost every day", "daily", data_satisfied$SkipMeals)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(SkipMeals, table(SkipMeals)[SkipMeals])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Skipped Meals Distribution 2018", y="Skipping Meals Response", x="Counts") 

level_order <- c('Never', '1-2 per week', '3-4 per week', 'daily', 'Missing')

ggplot(data_satisfied, aes(x = factor(SkipMeals, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Skipped Meals 2018", y="Satisfied Proportion", x="Skipped Meals", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################## PLOT OPEN SOURCER VS SATISFIED ######################################################################

unique(data_satisfied$OpenSource)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(OpenSource, table(OpenSource)[OpenSource])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Open Sourcer Distribution 2018", y="Open Source Contribution", x="Counts") 


ggplot(data_satisfied, aes(x = factor(OpenSource), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Open Source Contribution 2018", y="Satisfied Proportion", x="Open Source Contribution", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################### PLOT HOBBYIST VS SATISFIED ########################################################################

unique(data_satisfied$Hobbyist)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Hobbyist, table(Hobbyist)[Hobbyist])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Hobby Coder Distribution 2018", y="Hobby Coder Response", x="Counts") 


ggplot(data_satisfied, aes(x = factor(Hobbyist), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Coding As A Hobby 2018", y="Satisfied Proportion", x="Hobby Coder", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################## STATISTICAL CHI2 SIGNIFICANCE TESTS #################################################################

###################################################### CHI2 COUNTRY VS SATISFIED #######################################################################

### Hypothesis - There is a difference between country proportion for satisfied = yes vs satisfied = no
### Null Hypothesis - There is NO difference between country proportion for satisfied = yes vs satisfied = no

M <- with(data_satisfied,table(Satisfied,Country))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### Results - X-squared = 1112.2, df = 169, p-value < 2.2e-16
### Results fisher test result 0.0004998

######################################################## CHI2 AGE VS SATISFIED #########################################################################

### Hypothesis - There is a difference between age proportion for satisfied = yes vs satisfied = no
### Null Hypothesis - There is NO difference between age proportion for satisfied = yes vs satisfied = no

M <- with(data_satisfied,table(Satisfied, Age))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)


### X-squared = 70.928, df = 7, p-value = 9.592e-13
### Fisher test results are 0.0004998

####################################################### CHI2 GENDER VS SATISFIED #########################################################################
gender_data_satisfied <- data_satisfied[,c('Gender', 'Satisfied')]
gender_data_satisfied$Gender <- as.character(gender_data_satisfied$Gender)
gender_data_satisfied <- data_satisfied[data_satisfied$Gender %in% c("Male", "Female"), ]
gender_data_satisfied$Gender <- as.factor(as.character(gender_data_satisfied$Gender))
summary(gender_data_satisfied$Gender)

M <- with(gender_data_satisfied,table(Satisfied, Gender))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 1.2295, df = 1, p-value = 0.2675
### Fisher test results are 0.263

###################################################### CHI2 SALARY VS SATISFIED #########################################################################

M <- with(data_satisfied,table(Satisfied, SalaryUSDBinned))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 488.69, df = 20, p-value < 2.2e-16
### Fisher test results are 0.0004998

#################################################### CHI2 EMPLOYMENT VS SATISFIED #####################################################################

M <- with(data_satisfied,table(Satisfied, Employment))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 3.3814, df = 2, p-value = 0.1844
### Fisher test results are 0.1679

###################################################### CHI2 STUDENT VS SATISFIED #####################################################################

M <- with(data_satisfied,table(Satisfied, Student))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 31.116, df = 3, p-value = 8.035e-07
### Fisher test results are 0.0004998

#################################################### CHI2 COMPANY SIZE VS SATISFIED #####################################################################

M <- with(data_satisfied,table(Satisfied, CompanySize))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 32.828, df = 8, p-value = 6.613e-05
### Fisher test results are 0.0004998

##################################################### CHI2 DEVTYPE VS SATISFIED #########################################################################

summary(data_satisfied_dev_type)

M <- with(data_satisfied_dev_type,table(Satisfied, DeveloperType))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 135.29, df = 8, p-value < 2.2e-16
### Fisher test results are 0.0004998

##################################################### CHI2 LANGUAGE DESIRE VS SATISFIED ########################################################################

summary(data_satisfied_languages_desire)

## Assembly Desired

M <- with(data_satisfied_languages_desire,table(Satisfied, isAssembly))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 4.8903, df = 1, p-value = 0.02701
### Fisher test results are 0.02664

M <- with(data_satisfied_languages_desire,table(Satisfied, isBash))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 46.667, df = 1, p-value = 8.412e-12
### Fisher test results are 6.43e-12

M <- with(data_satisfied_languages_desire,table(Satisfied, isPython))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 2.7882, df = 1, p-value = 0.09496
### Fisher test results are 0.09343

M <- with(data_satisfied_languages_desire,table(Satisfied, isJava))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.89055, df = 1, p-value = 0.3453
### Fisher test results are 0.3434

M <- with(data_satisfied_languages_desire,table(Satisfied, isJavaScript))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 5.731, df = 1, p-value = 0.01667
### Fisher test results are 0.01631

M <- with(data_satisfied_languages_desire,table(Satisfied, isGo))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 1.3326, df = 1, p-value = 0.2484
### Fisher test results are 0.244


M <- with(data_satisfied_languages_desire,table(Satisfied, isCoffeeScript))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 1.5415, df = 1, p-value = 0.2144
### Fisher test results are p-value = 0.2102

M <- with(data_satisfied_languages_desire,table(Satisfied, isErlang))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 1.3878, df = 1, p-value = 0.2388
### Fisher test results are p-value = 0.2291

M <- with(data_satisfied_languages_desire,table(Satisfied, isMatlab))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 4.3083, df = 1, p-value = 0.03793
### Fisher test results are p-value = 0.03664

M <- with(data_satisfied_languages_desire,table(Satisfied, isHaskell))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 2.669, df = 1, p-value = 0.1023
### Fisher test results are p-value = 0.09949

M <- with(data_satisfied_languages_desire,table(Satisfied, isJulia))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.35302, df = 1, p-value = 0.5524
### Fisher test results are p-value = 0.522

M <- with(data_satisfied_languages_desire,table(Satisfied, isKotlin))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.50619, df = 1, p-value = 0.4768
### Fisher test results are p-value = 0.4725

M <- with(data_satisfied_languages_desire,table(Satisfied, isRust))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 5.5489, df = 1, p-value = 0.01849
### Fisher test results are p-value = 0.01807

M <- with(data_satisfied_languages_desire,table(Satisfied, isSQL))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 8.6677, df = 1, p-value = 0.003239
### Fisher test results are p-value = 0.003171

################################################### CHI2 YEARS CODE VS SATISFIED ########################################################################

M <- with(data_satisfied,table(Satisfied, YearsCode))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 179.93, df = 11, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################### CHI2 YEARS CODE PRO VS SATISFIED ####################################################################

M <- with(data_satisfied,table(Satisfied, YearsCodePro))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 66.209, df = 10, p-value = 2.378e-10
### Fisher test results are p-value = 0.0004998


##################################################### CHI2 WAKE TIME VS SATISFIED #######################################################################

M <- with(data_satisfied,table(Satisfied, WakeTime))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 66.209, df = 10, p-value = 1.775e-07
### Fisher test results are p-value = 0.0004998

################################################# CHI2 HOURS COMPUTER VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, HoursComputer))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 98.645, df = 5, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################## CHI2 HOURS OUTSIDE VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, HoursOutside))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 90.724, df = 5, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################## CHI2 SKIPPED MEALS VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, SkipMeals))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 125.87, df = 4, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################### CHI2 OPEN SOURCER VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, OpenSource))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 69.036, df = 1, p-value < 2.2e-16
### Fisher test results are p-value < 2.2e-16

################################################## CHI2 HOBBYIST VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, Hobbyist))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 23.406, df = 1, p-value = 1.311e-06
### Fisher test results are p-value = 1.416e-06

###################################################### FEATURE ENGINEERING #############################################################################

###  Variables that appear to contribute to satisfaction are:
###  Country, Age, SalaryUSDBinned, Student, Company Size, DevType, languageDesiredIsBash, YearsCode, YearsCodePro, WakeTime, HoursComputer, SkipMeals
### Need to use devtype to build DeveloperType column
### Need to use LanguageDesireNextYear to build languageDesiredIsBash column

data_satisfied_features <- data_satisfied %>% 
  select(Satisfied, Country, Age, SalaryUSDBinned, Student, CompanySize, DevType, YearsCode, YearsCodePro, 
         WakeTime, HoursComputer, SkipMeals, OpenSource, Hobbyist, LanguageDesireNextYear) %>%
  mutate(languageDesiredIsBash = ifelse(str_count(LanguageDesireNextYear, 'Bash') > 0, "Yes", "No")) %>% 
  mutate(DeveloperType = case_when(str_count(DevType, 'executive') > 0 ~ "Executive", 
                                   str_count(DevType, 'manager') > 0 ~ "Manager",
                                   str_count(DevType, 'academic') > 0 ~ "Academic/Researcher",
                                   str_count(DevType, 'analyst') > 0 & str_count(DevType, 'developer') == 0 ~ "Data Analyst/Scientist",
                                   str_count(DevType, 'scientist') > 0 ~ "Data Analyst/Scientist",
                                   str_count(DevType, 'administrator') > 0 & str_count(DevType, 'developer') == 0 ~ "Administrator",
                                   str_count(DevType, 'Designer') > 0 & str_count(DevType, 'developer') == 0 ~ "Designer",
                                   str_count(DevType, 'DevOps') > 0 ~ "DevOps",
                                   str_count(DevType, 'sales') > 0 ~ "Sales",
                                   str_count(DevType, 'None') > 0 ~ "Other",
                                   DevType == 'Missing' ~ "Missing",
                                   TRUE ~ "Developer" )) 

data_satisfied_features <- data_satisfied_features[, !(names(data_satisfied_features) %in% c('DevType', 'LanguageDesireNextYear'))]

### Need to remove country data where there are low counts as this will impact the models 
str(data_satisfied_features$Country)
data_satisfied_features$Country <- fct_lump_min(data_satisfied_features$Country, min = 20)
str(data_satisfied_features$Country)

summary(data_satisfied_features)

############################################################ MODELS ####################################################################################

data_satisfied_features <- data_satisfied_features %>% mutate_if(is.character, as.factor)

summary(data_satisfied_features)

### Benchmarking Using MLR3 Library 
### Comparing Naive Bayes, Decision Tree, Logistic Regression and KNN models against 2020 data to 
### predict whether a developer is satisfied or not using features selected based on CHI2 being 
### less than 0.01

data_task = TaskClassif$new(id = 'devsurvey2018', backend = data_satisfied_features, target = "Satisfied", positive = "Yes")

learners = c("classif.naive_bayes", "classif.rpart", "classif.log_reg", "classif.kknn")
learners = lapply(learners, lrn,
                  predict_type = "prob", predict_sets = c("train", "test"))

# Create a benchmark design object
design = benchmark_grid(
  tasks = data_task,
  learners = learners,
  resamplings = rsmp("holdout") #Splits data into a training set and a test set 
)

# Execute the benchmark
bmr = benchmark(design)

measures = list(
  msr("classif.acc", id = "accuracy_test"),
  msr("classif.auc", id = "auc_test"),
  msr("classif.precision", id = "precision_test"),
  msr("classif.recall", id = "recall_test")
)
preformance_measures <- bmr$aggregate(measures)
print(preformance_measures)
str(preformance_measures)


performance_measures_df <- data.frame(preformance_measures$learner_id, preformance_measures$accuracy_test, 
                                      preformance_measures$auc_test, preformance_measures$precision_test,
                                      preformance_measures$recall_test)

write.csv(performance_measures_df, file = "model_performance_2018.csv", row.names = FALSE)

### Check if balancing classes improves model

as.data.frame(table(data_satisfied_features$Satisfied))
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced_data <- SMOTE(Satisfied ~., as.data.frame(data_satisfied_features), perc.over = 100, k = 5, perc.under = 200)
as.data.frame(table(balanced_data$Satisfied))

summary(balanced_data)

balanced_data_task = TaskClassif$new(id = 'balancedData2020', backend = balanced_data, target = "Satisfied", positive = "Yes")

# Create a benchmark design object
design = benchmark_grid(
  tasks = balanced_data_task,
  learners = learners,
  resamplings = rsmp("holdout") #Splits data into a training set and a test set 
)

# Execute the benchmark
bmr = benchmark(design)

# Analyse performance of models
preformance_measures <- bmr$aggregate(measures)
print(preformance_measures)
str(preformance_measures)


balanced_data_performance_measures_df <- data.frame(preformance_measures$learner_id, preformance_measures$accuracy_test, 
                                                    preformance_measures$auc_test, preformance_measures$precision_test,
                                                    preformance_measures$recall_test)

write.csv(balanced_data_performance_measures_df, file = "model_performance_balanced_data_2020.csv", row.names = FALSE)
