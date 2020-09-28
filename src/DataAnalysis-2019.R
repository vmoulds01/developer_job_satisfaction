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


cleaned_data_file_2019 = 'datasets/interim/2019_clean_data_pre_factor_reduction.csv'
clean_data_2019 <- read_csv(cleaned_data_file_2019)
clean_data_2019 <- clean_data_2019 %>% mutate_if(is.character, as.factor)
summary(clean_data_2019)

initial_information_gain_ratio <- information_gain_ratio_feature_importance(clean_data_2019)
print(initial_information_gain_ratio)

summary(clean_data_2019$JobSat)
data_satisfied <- clean_data_2019 %>% mutate(Satisfied = ifelse(JobSat %in% c('Slightly satisfied', 'Very satisfied'), "Yes","No"))
data_satisfied <- data_satisfied[, !(names(data_satisfied) %in% c('JobSat'))]

satisfied_information_gain_ratio <- information_gain_ratio_target(data_satisfied, 'Satisfied')
print(satisfied_information_gain_ratio)


############################################################# PLOTS ###################################################################################

####################################################### PLOT JOB SATISFACTION #########################################################################

ggplot(clean_data_2019) + 
  geom_bar(aes(y = reorder(JobSat, table(JobSat)[JobSat])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Job Satisfaction 2019", y="Satisfaction Response", x="Counts") 

######################################################### PLOT SATISFACTION ###########################################################################

ggplot(data_satisfied, aes(x = Satisfied)) + 
  geom_bar(position = position_dodge(width=0.5), fill="lightblue", color="darkblue") + 
  geom_text(aes(label=scales::percent(stat(prop)), group=1), stat='count', vjust = -.5) +
  labs(title="Satisfied Distribution 2019", x="Satisfied", y="Counts") 

###################################################### PLOT JOB SEEK VS SATISFIED ######################################################################

unique(data_satisfied$JobSeek)

data_satisfied$JobSeek <- gsub("I am actively looking for a job", "Actively Looking", data_satisfied$JobSeek)
data_satisfied$JobSeek <- gsub("I am not interested in new job opportunities", "Not Interested", data_satisfied$JobSeek)
data_satisfied$JobSeek <- gsub("I’m not actively looking, but I am open to new opportunities", "Not Actively Looking", data_satisfied$JobSeek)

ggplot(data=data_satisfied, aes(x=reorder(JobSeek, table(JobSeek)[JobSeek]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="2019 Job Seek Variance For Satisfied", y="Counts", x="Job Seek", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

level_order <- c('Not Interested', 'Not Actively Looking', 'Actively Looking', 'Missing')


ggplot(data_satisfied, aes(x =factor(JobSeek, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Job Seek Category 2019", y="Satisfied Proportion", x="Job Seek Category", fill="Satisfied") + 
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
  labs(title = "Responses Per Country 2019", fill="Responses")

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
  labs(title = "Proportion Satisfied Per Country 2019")

head(data_world_sat_summary[order(data_world_sat_summary$SatisfiedProportion),], 20)
tail(data_world_sat_summary[order(data_world_sat_summary$SatisfiedProportion),], 20)

######################################################## PLOT AGE VS SATISFIED #########################################################################

unique(data_satisfied$AgeBinned)

data_satisfied$AgeBinned <- gsub("<18", "Under 18", data_satisfied$AgeBinned)
data_satisfied$AgeBinned <- gsub("18-24", "18 to 24", data_satisfied$AgeBinned)
data_satisfied$AgeBinned <- gsub("25-34", "25 to 34", data_satisfied$AgeBinned)
data_satisfied$AgeBinned <- gsub("35-44", "35 to 44", data_satisfied$AgeBinned)
data_satisfied$AgeBinned <- gsub("45-54", "45 to 54", data_satisfied$AgeBinned)
data_satisfied$AgeBinned <- gsub("55-64", "55 to 64", data_satisfied$AgeBinned)
data_satisfied$AgeBinned <- gsub(">65", "Over 65", data_satisfied$AgeBinned)

level_order <- c('Under 18', '18 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 64', 'Over 65', 'Missing')

ggplot(data=data_satisfied, aes(x=reorder(AgeBinned, table(AgeBinned)[AgeBinned]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Age Variance Satisfication 2019", y="Counts", x="Age", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x =factor(AgeBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Age Category 2019", y="Satisfied Proportion", x="Age", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

###################################################### PLOT GENDER VS SATISFIED #########################################################################

unique(data_satisfied$Gender)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Gender, table(Gender)[Gender])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Gender Distribution 2019", y="Gender Response", x="Counts") 

gender_data_satisfied <- data_satisfied[data_satisfied$Gender %in% c("Man", "Woman"), ]

levels(gender_data_satisfied$Gender)[levels(gender_data_satisfied$Gender)=="Man"] <- "Male"
levels(gender_data_satisfied$Gender)[levels(gender_data_satisfied$Gender)=="Woman"] <- "Female"
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
  labs(title="Salary Distribution 2019", y="Salaray Response", x="Counts") 

salary_data_satisfied <- data_satisfied[!(data_satisfied$SalaryUSDBinned %in% c("Missing")), ]
summary(salary_data_satisfied$SalaryUSDBinned)

ggplot(salary_data_satisfied) + 
  geom_bar(aes(y = reorder(SalaryUSDBinned, table(SalaryUSDBinned)[SalaryUSDBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Salary Distribution 2019", y="Salaray Response", x="Counts") 

level_order <- c('<5028', '5028-9228', '9228-13620', '13620-18996', '18996-24648', '24648-30384', '30384-36444', '36444-42036', '42036-47996', 
                 '47996-54049', '54049-60000', '60000-66716', '66716-75000', '75000-84000', '84000-95000', '95000-110000', '110000-129588', 
                 '129588-160000', '160000-275000', '>275000', 'Missing')

ggplot(data=salary_data_satisfied, aes(x=factor(SalaryUSDBinned, level=level_order), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Salary Variance Satisfication 2019", y="Counts", x="Salary USD", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(salary_data_satisfied, aes(x = factor(SalaryUSDBinned, level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Salary USD 2019", y="Satisfied Proportion", x="Salaray USD", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

#################################################### PLOT EMPLOYMENT VS SATISFIED #####################################################################

unique(data_satisfied$Employment)

data_satisfied$Employment <- gsub("Employed full-time", "Full-time", data_satisfied$Employment)
data_satisfied$Employment <- gsub("Employed part-time", "Part-time", data_satisfied$Employment)
data_satisfied$Employment <- gsub("Independent contractor, freelancer, or self-employed", "Independent", data_satisfied$Employment)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Employment, table(Employment)[Employment])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Employment Distribution 2019", y="Employment Response", x="Counts") 

level_order <- c('Full-time', 'Independent', 'Part-time', 'Missing')

ggplot(data=data_satisfied, aes(x=factor(Employment, level=level_order), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Employment Variance Satisfication 2019", y="Counts", x="Employment", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(Employment, level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Employment Type 2019", y="Satisfied Proportion", x="Employment Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

###################################################### PLOT STUDENT VS SATISFIED #####################################################################

summary(data_satisfied$Student)

data_satisfied$Student <- gsub("Yes, full-time", "Full-time", data_satisfied$Student)
data_satisfied$Student <- gsub("Yes, part-time", "Part-time", data_satisfied$Student)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Student, table(Student)[Student])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Student Distribution 2019", y="Student Response", x="Counts") 

ggplot(data=data_satisfied, aes(x=reorder(Student, table(Student)[Student]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Student Variance Satisfication 2019", y="Counts", x="Student", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(Student), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Student Type 2019", y="Satisfied Proportion", x="Student Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

#################################################### PLOT COMPANY SIZE VS SATISFIED #####################################################################

unique(data_satisfied$OrgSize)

data_satisfied$OrgSize <- gsub("Just me - I am a freelancer, sole proprietor, etc.", "<10", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("2-9 employees", "<10", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("10 to 19 employees", "10 to 19", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("20 to 99 employees", "20 to 99", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("100 to 499 employees", "100 to 499", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("500 to 999 employees", "500 to 999", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("1,000 to 4,999 employees", "1,000 to 4,999", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("5,000 to 9,999 employees", "5,000 to 9,999", data_satisfied$OrgSize)
data_satisfied$OrgSize <- gsub("10,000 or more employees", ">10,000", data_satisfied$OrgSize)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(OrgSize, table(OrgSize)[OrgSize])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="CompanySize Distribution 2019", y="CompanySize Response", x="Counts") 

level_order <- c('<10', '10 to 19', '20 to 99', '100 to 499', '500 to 999', '1,000 to 4,999', '5,000 to 9,999', '>10,000', 'Missing')

ggplot(data=data_satisfied, aes(x=factor(OrgSize, level=level_order), fill=factor(Satisfied))) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="CompanySize Vs Satisfication", y="Counts", x="CompanySize", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(OrgSize, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Company Size", y="Satisfied Proportion", x="CompanySize", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


##################################################### PLOT DEVTYPE VS SATISFIED #########################################################################

str(data_satisfied$DevType)


data_satisfied_dev_type <- data_satisfied %>% 
  select(Satisfied, DevType) %>%
  mutate(isExecutive = ifelse(str_count(DevType, 'Executive') > 0, 1, 0)) %>%
  mutate(isManager = ifelse(str_count(DevType, 'manager') > 0, 1, 0)) %>%
  mutate(isAnalyst = ifelse(str_count(DevType, 'analyst') > 0, 1, 0)) %>%
  mutate(isDataScientist = ifelse(str_count(DevType, 'scientist') > 0, 1, 0)) %>% 
  mutate(isAdmin = ifelse(str_count(DevType, 'administrator') > 0, 1, 0)) %>% 
  mutate(isDesigner = ifelse(str_count(DevType, 'Designer') > 0, 1, 0)) %>% 
  mutate(isDevOps = ifelse(str_count(DevType, 'DevOps') > 0, 1, 0)) %>% 
  mutate(isSales = ifelse(str_count(DevType, 'sales') > 0, 1, 0)) %>% 
  mutate(isOther = ifelse(str_count(DevType, 'Other') > 0, 1, 0)) %>% 
  mutate(isDeveloper = ifelse(str_count(DevType, 'developer') > 0, 1, 0)) %>%
  mutate(DeveloperType = case_when(str_count(DevType, 'Executive') > 0 ~ "Executive", 
                                   str_count(DevType, 'manager') > 0 ~ "Manager",
                                   str_count(DevType, 'Academic') > 0 ~ "Academic/Researcher",
                                   str_count(DevType, 'Educator') > 0 ~ "Academic/Researcher",
                                   str_count(DevType, 'analyst') > 0 & str_count(DevType, 'developer') == 0 ~ "Data Analyst/Scientist",
                                   str_count(DevType, 'scientist') > 0 ~ "Data Analyst/Scientist",
                                   str_count(DevType, 'administrator') > 0 & str_count(DevType, 'developer') == 0 ~ "Administrator",
                                   str_count(DevType, 'Designer') > 0 & str_count(DevType, 'developer') == 0 ~ "Designer",
                                   str_count(DevType, 'DevOps') > 0 ~ "DevOps",
                                   str_count(DevType, 'sales') > 0 ~ "Sales",
                                   str_count(DevType, 'Other') > 0 ~ "Other",
                                   str_count(DevType, 'Engineer') > 0 & str_count(DevType, 'developer') == 0 ~ "Other",
                                   str_count(DevType, 'Student') > 0 & str_count(DevType, 'developer') == 0 ~ "Other",
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
  labs(title="Developer Type Distribution 2019", x="Developer Types", y="Counts") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_dev_type) + 
  geom_bar(aes(y = reorder(DeveloperType, table(DeveloperType)[DeveloperType])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Developer Type Distribution 2019", y="Developer Type Response", x="Counts") 

ggplot(data_satisfied_dev_type, aes(x = factor(DeveloperType), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Developer Type", y="Satisfied Proportion", x="Developer Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

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
  mutate(isClojure = ifelse(str_count(LanguageDesireNextYear, 'Clojure') > 0, 1, 0)) %>%
  mutate(isDart = ifelse(str_count(LanguageDesireNextYear, 'Dart') > 0, 1, 0)) %>%
  mutate(isElixir = ifelse(str_count(LanguageDesireNextYear, 'Elixir') > 0, 1, 0)) %>%
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
  mutate(isRuby = ifelse(str_count(LanguageDesireNextYear, 'Ruby') > 0, 1, 0)) %>%
  mutate(isScala = ifelse(str_count(LanguageDesireNextYear, 'Scala') > 0, 1, 0)) %>%
  mutate(isSQL = ifelse(str_count(LanguageDesireNextYear, 'SQL') > 0, 1, 0))

data_languages_desire <- data_satisfied_languages_desire[, !(names(data_satisfied_languages_desire) %in% c('LanguageDesireNextYear', 'Satisfied'))]
data_languages_desire <- data.frame(colname = names(data_languages_desire), counts = colSums(data_languages_desire))

head(data_languages_desire)

ggplot(data_languages_desire, aes(x=colname, y=counts)) + 
  geom_bar(stat='identity', fill="lightblue", color="darkblue") + 
  labs(title="Language Desire Distribution 2019", x="Languages Desire", y="Counts") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_languages_desire, aes(x = factor(isBash), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied If Bash Language Desired", y="Satisfied Proportion", x="Bash Language Desire", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################### PLOT YEARS CODE VS SATISFIED ########################################################################

unique(data_satisfied$YearsCodeBinned)

data_satisfied$YearsCodeBinned <- gsub("0-2", "0 to 2", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("3-5", "3 to 5", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("6-8", "6 to 8", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("9-11", "9 to 11", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("12-14", "12 to 14", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("15-17", "15 to 17", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("18-20", "18 to 20", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("21-23", "21 to 23", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("24-26", "24 to 26", data_satisfied$YearsCodeBinned)
data_satisfied$YearsCodeBinned <- gsub("27-29", "27 to 29", data_satisfied$YearsCodeBinned)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(YearsCodeBinned, table(YearsCodeBinned)[YearsCodeBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="YearsCode Distribution 2019", y="YearsCode Response", x="Counts") 

level_order <- c('0 to 2', '3 to 5', '6 to 8', '9 to 11', '12 to 14', '15 to 17', '18 to 20', '21 to 23', 
                 '24 to 26', '27 to 29', '>30', 'Missing')

ggplot(data_satisfied, aes(x = factor(YearsCodeBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Years Coding", y="Satisfied Proportion", x="Years Code", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))


################################################# PLOT YEARS CODE PRO VS SATISFIED ######################################################################

unique(data_satisfied$YearsCodeProBinned)

data_satisfied$YearsCodeProBinned <- gsub("0-2", "0 to 2", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("3-5", "3 to 5", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("6-8", "6 to 8", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("9-11", "9 to 11", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("12-14", "12 to 14", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("15-17", "15 to 17", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("18-20", "18 to 20", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("21-23", "21 to 23", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("24-26", "24 to 26", data_satisfied$YearsCodeProBinned)
data_satisfied$YearsCodeProBinned <- gsub("27-29", "27 to 29", data_satisfied$YearsCodeProBinned)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(YearsCodeProBinned, table(YearsCodeProBinned)[YearsCodeProBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Years Code Pro Distribution 2019", y="Years Code Pro Response", x="Counts") 

level_order <- c('0 to 2', '3 to 5', '6 to 8', '9 to 11', '12 to 14', '15 to 17', '18 to 20', '21 to 23', 
                 '24 to 26', '27 to 29', '>30', 'Missing')

ggplot(data_satisfied, aes(x = factor(YearsCodeProBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Years Coding Professional", y="Satisfied Proportion", x="Years Code Professional", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


##################################################### PLOT WORK HRS VS SATISFIED ##################################################################

unique(data_satisfied$WorkWeekHrsBinned)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(WorkWeekHrsBinned, table(WorkWeekHrsBinned)[WorkWeekHrsBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Work Week Hrs Distribution 2019", y="Work Week Hrs", x="Counts") 

level_order <- c('<37.5', '37.5-40', '40-45', '>=45', 'Missing')

ggplot(data_satisfied, aes(x = factor(WorkWeekHrsBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Work Week Hrs", y="Satisfied Proportion", x="Work Week Hrs", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################# PLOT MGR IDIOT VS SATISFIED ######################################################################

unique(data_satisfied$MgrIdiot)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(MgrIdiot, table(MgrIdiot)[MgrIdiot])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Confidence In Manager Distribution 2019", y="Confidence In Manager", x="Counts") 

data_satisfied$MgrIdiot <- gsub("I don't have a manager", "NA", data_satisfied$MgrIdiot)
data_satisfied$MgrIdiot <- gsub("Not at all confident", "None", data_satisfied$MgrIdiot)
data_satisfied$MgrIdiot <- gsub("Somewhat confident", "Some", data_satisfied$MgrIdiot)
data_satisfied$MgrIdiot <- gsub("Very confident", "Lots", data_satisfied$MgrIdiot)

level_order <- c('None', 'Some', 'Lots', 'NA', 'Missing')

ggplot(data_satisfied, aes(x = factor(MgrIdiot, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Confidence In Manager", y="Satisfied Proportion", x="Confidence In Manager", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


################################################## PLOT OPEN SOURCER VS SATISFIED ######################################################################

unique(data_satisfied$OpenSourcer)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(OpenSourcer, table(OpenSourcer)[OpenSourcer])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Open Sourcer Distribution 2019", y="Open Source Contribution", x="Counts") 

data_satisfied$OpenSourcer <- gsub("Less than once a month but more than once per year", "Less Once Month", data_satisfied$OpenSourcer)
data_satisfied$OpenSourcer <- gsub("Once a month or more often", "More Once Month", data_satisfied$OpenSourcer)
data_satisfied$OpenSourcer <- gsub("Less than once per year", "Less Once Year", data_satisfied$OpenSourcer)

level_order <- c('Never', 'Less Once Month', 'More Once Month', 'Less Once Year')

ggplot(data_satisfied, aes(x = factor(OpenSourcer, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Open Source Contribution", y="Satisfied Proportion", x="Open Source Contribution", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################### PLOT HOBBYIST VS SATISFIED ########################################################################

unique(data_satisfied$Hobbyist)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Hobbyist, table(Hobbyist)[Hobbyist])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Hobby Coder Distribution 2019", y="Hobby Coder Response", x="Counts") 


ggplot(data_satisfied, aes(x = factor(Hobbyist), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Coding As A Hobby", y="Satisfied Proportion", x="Hobby Coder", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

################################################### WORK PLAN VS SATISFIED ########################################################################

unique(data_satisfied$WorkPlan)

data_satisfied$WorkPlan <- mapvalues(data_satisfied$WorkPlan, 
from = c("There's no schedule or spec; I work on what seems most important or urgent", 
         "There is a schedule and/or spec (made by me or by a colleague), and I follow it very closely", 
         "There is a schedule and/or spec (made by me or by a colleague), and my work somewhat aligns"), 
to = c("No Schedule", "Schedule Closely Follow", "Schedule Somewhat Follow"))


summary(data_satisfied$WorkPlan)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(WorkPlan, table(WorkPlan)[WorkPlan])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Work Plan Distribution 2019", y="Work Plan Response", x="Counts") 

level_order <- c('No Schedule', 'Schedule Closely Follow', 'Schedule Somewhat Follow', 'Missing')


ggplot(data_satisfied, aes(x = factor(WorkPlan, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Work Plan", y="Satisfied Proportion", x="Work Plan", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################### WORK REMOTE VS SATISFIED ########################################################################

unique(data_satisfied$WorkRemote)

data_satisfied$WorkRemote <- mapvalues(data_satisfied$WorkRemote, 
                                     from = c("Less than once per month / Never", 
                                              "A few days each month", 
                                              "Less than half the time, but at least one day each week", 
                                              "More than half, but not all, the time", 
                                              "All or almost all the time (I'm full-time remote)"), 
                                     to = c("Never or Rarely", 
                                            "Few Days Monthly", 
                                            "Least A Day Weekly", 
                                            "More than half", 
                                            "All or Mostly"))

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(WorkRemote, table(WorkRemote)[WorkRemote])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Remote Working Distribution 2019", y="Remote Working Response", x="Counts") 

level_order <- c("Never or Rarely", "Few Days Monthly", "Least A Day Weekly", "More than half", "All or Mostly", "It's complicated", "Missing")


ggplot(data_satisfied, aes(x = factor(WorkRemote, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Remote Working Response", y="Satisfied Proportion", x="Remote Working Response", fill="Satisfied") + 
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

### Results - X-squared = 1874.3, df = 169, p-value < 2.2e-16
### Results fisher test result 0.0004998

######################################################## CHI2 AGE VS SATISFIED #########################################################################

### Hypothesis - There is a difference between age proportion for satisfied = yes vs satisfied = no
### Null Hypothesis - There is NO difference between age proportion for satisfied = yes vs satisfied = no

M <- with(data_satisfied,table(Satisfied, AgeBinned))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)


### X-squared = 175.12, df = 7, p-value < 2.2e-16
### Fisher test results are 0.0004998

####################################################### CHI2 GENDER VS SATISFIED #########################################################################
gender_data_satisfied <- data_satisfied[,c('Gender', 'Satisfied')]
# Rename Man & Woman to Male & Female
levels(gender_data_satisfied$Gender)[levels(gender_data_satisfied$Gender)=="Man"] <- "Male"
levels(gender_data_satisfied$Gender)[levels(gender_data_satisfied$Gender)=="Woman"] <- "Female"
gender_data_satisfied <- gender_data_satisfied[gender_data_satisfied$Gender %in% c("Male", "Female"), ]
gender_data_satisfied$Gender <- as.factor(as.character(gender_data_satisfied$Gender))
summary(gender_data_satisfied$Gender)

M <- with(gender_data_satisfied,table(Satisfied, Gender))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 14.771, df = 1, p-value = 0.0001214
### Fisher test results are 0.0001094

###################################################### CHI2 SALARY VS SATISFIED #########################################################################

M <- with(data_satisfied,table(Satisfied, SalaryUSDBinned))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 1115.3, df = 20, p-value < 2.2e-16
### Fisher test results are 0.0004998

#################################################### CHI2 EMPLOYMENT VS SATISFIED #####################################################################

M <- with(data_satisfied,table(Satisfied, Employment))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 59.75, df = 3, p-value = 6.647e-13
### Fisher test results are 0.0004998

###################################################### CHI2 STUDENT VS SATISFIED #####################################################################

M <- with(data_satisfied,table(Satisfied, Student))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 121.07, df = 3, p-value < 2.2e-16
### Fisher test results are 0.0004998

#################################################### CHI2 COMPANY SIZE VS SATISFIED #####################################################################

M <- with(data_satisfied,table(Satisfied, OrgSize))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 63.172, df = 8, p-value = 1.108e-10
### Fisher test results are 0.0004998

##################################################### CHI2 DEVTYPE VS SATISFIED #########################################################################

summary(data_satisfied_dev_type)

M <- with(data_satisfied_dev_type,table(Satisfied, DeveloperType))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 64.371, df = 9, p-value = 1.912e-10
### Fisher test results are 0.0004998

##################################################### CHI2 LANGUAGE DESIRE VS SATISFIED ########################################################################

summary(data_satisfied_languages_desire)

## Assembly Desired

M <- with(data_satisfied_languages_desire,table(Satisfied, isAssembly))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.27724, df = 1, p-value = 0.5985
### Fisher test results are 0.5932

M <- with(data_satisfied_languages_desire,table(Satisfied, isBash))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 81.312, df = 1, p-value < 2.2e-16
### Fisher test results are 2.2e-16

M <- with(data_satisfied_languages_desire,table(Satisfied, isClojure))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.093193, df = 1, p-value = 0.7602 
### Fisher test results are 0.7549

M <- with(data_satisfied_languages_desire,table(Satisfied, isDart))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 19.019, df = 1, p-value = 1.294e-05
### Fisher test results are 1.432e-05

M <- with(data_satisfied_languages_desire,table(Satisfied, isElixir))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.074692, df = 1, p-value = 0.7846
### Fisher test results are p-value = 0.7759

M <- with(data_satisfied_languages_desire,table(Satisfied, isPython))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 14.727, df = 1, p-value = 0.0001242
### Fisher test results are 0.0001218

M <- with(data_satisfied_languages_desire,table(Satisfied, isJava))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.024159, df = 1, p-value = 0.8765
### Fisher test results are 0.8715

M <- with(data_satisfied_languages_desire,table(Satisfied, isJavaScript))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 3.8459, df = 1, p-value = 0.04987
### Fisher test results are 0.04983

M <- with(data_satisfied_languages_desire,table(Satisfied, isGo))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 1.2938, df = 1, p-value = 0.2553
### Fisher test results are 0.2538

M <- with(data_satisfied_languages_desire,table(Satisfied, isErlang))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 3.637, df = 1, p-value = 0.05651
### Fisher test results are p-value = 0.05361


M <- with(data_satisfied_languages_desire,table(Satisfied, isKotlin))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 15.355, df = 1, p-value = 8.908e-05
### Fisher test results are p-value = 9.057e-05

M <- with(data_satisfied_languages_desire,table(Satisfied, isRust))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 7.4363, df = 1, p-value = 0.006392
### Fisher test results are p-value = 0.006144

M <- with(data_satisfied_languages_desire,table(Satisfied, isRuby))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 0.40899, df = 1, p-value = 0.5225
### Fisher test results are p-value = 0.519

M <- with(data_satisfied_languages_desire,table(Satisfied, isScala))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 20.247, df = 1, p-value = 6.806e-06
### Fisher test results are p-value = 7.098e-06

M <- with(data_satisfied_languages_desire,table(Satisfied, isSQL))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 4.0348, df = 1, p-value = 0.04457
### Fisher test results are p-value = 0.04398

################################################### CHI2 YEARS CODE VS SATISFIED ########################################################################

M <- with(data_satisfied,table(Satisfied, YearsCodeBinned))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 128.41, df = 11, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################### CHI2 YEARS CODE PRO VS SATISFIED ####################################################################

M <- with(data_satisfied,table(Satisfied, YearsCodePro))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 128.41, df = 11, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998


##################################################### CHI2 WORK HRS VS SATISFIED #######################################################################

M <- with(data_satisfied,table(Satisfied, WorkWeekHrsBinned))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 105.22, df = 4, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

###################################################### CHI2 MGR IDIOT VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, MgrIdiot))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 6924.8, df = 4, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################### CHI2 OPEN SOURCER VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, OpenSourcer))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 162.35, df = 3, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################## CHI2 HOBBYIST VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, Hobbyist))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 27.94, df = 1, p-value = 1.252e-07
### Fisher test results are p-value = 1.319e-07

################################################## CHI2 WORK PLAN VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, WorkPlan))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 648.33, df = 3, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

################################################## CHI2 WORK REMOTE VS SATISFIED ######################################################################

M <- with(data_satisfied,table(Satisfied, WorkRemote))
print(M)
Xsq <-chisq.test(M)
Xsq

fisher.test(M, simulate.p.value=TRUE)

### X-squared = 550.03, df = 7, p-value < 2.2e-16
### Fisher test results are p-value = 0.0004998

###################################################### FEATURE ENGINEERING #############################################################################

###  Variables that appear to contribute to satisfaction are:
###  Country, Age, SalaryUSDBinned, Student, Company Size, DevType, languageDesiredIsBash, languageDesiredIsDart, languageDesiredIsKotlin, 
###  languageDesiredIsScala, YearsCode, YearsCodePro, WorkWeekHrsBinned, MgrIdiot, OpenSourcer, Hobbyist, WorkPlan, WorkRemote
###  Need to use devtype to build DeveloperType column
###  Need to use LanguageDesireNextYear to build languageDesiredIsBash column

data_satisfied_features <- data_satisfied %>% 
  select(Satisfied, Country, AgeBinned, SalaryUSDBinned, Student, OrgSize, DevType, YearsCodeBinned, YearsCodeProBinned, 
         WorkWeekHrsBinned, MgrIdiot, OpenSourcer, Hobbyist, WorkPlan, WorkRemote, LanguageDesireNextYear) %>%
  mutate(languageDesiredIsBash = ifelse(str_count(LanguageDesireNextYear, 'Bash') > 0, "Yes", "No")) %>% 
  mutate(languageDesiredIsDart = ifelse(str_count(LanguageDesireNextYear, 'Dart') > 0, "Yes", "No")) %>% 
  mutate(languageDesiredIsKotlin = ifelse(str_count(LanguageDesireNextYear, 'Kotlin') > 0, "Yes", "No")) %>% 
  mutate(languageDesiredIsScala = ifelse(str_count(LanguageDesireNextYear, 'Scala') > 0, "Yes", "No")) %>% 
  mutate(DeveloperType = case_when(str_count(DevType, 'Executive') > 0 ~ "Executive", 
                                   str_count(DevType, 'manager') > 0 ~ "Manager",
                                   str_count(DevType, 'Academic') > 0 ~ "Academic/Researcher",
                                   str_count(DevType, 'Educator') > 0 ~ "Academic/Researcher",
                                   str_count(DevType, 'analyst') > 0 & str_count(DevType, 'developer') == 0 ~ "Data Analyst/Scientist",
                                   str_count(DevType, 'scientist') > 0 ~ "Data Analyst/Scientist",
                                   str_count(DevType, 'administrator') > 0 & str_count(DevType, 'developer') == 0 ~ "Administrator",
                                   str_count(DevType, 'Designer') > 0 & str_count(DevType, 'developer') == 0 ~ "Designer",
                                   str_count(DevType, 'DevOps') > 0 ~ "DevOps",
                                   str_count(DevType, 'sales') > 0 ~ "Sales",
                                   str_count(DevType, 'Other') > 0 ~ "Other",
                                   str_count(DevType, 'Engineer') > 0 & str_count(DevType, 'developer') == 0 ~ "Other",
                                   str_count(DevType, 'Student') > 0 & str_count(DevType, 'developer') == 0 ~ "Other",
                                   DevType == 'Missing' ~ "Missing",
                                   TRUE ~ "Developer"))

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

data_task = TaskClassif$new(id = 'devsurvey2019', backend = data_satisfied_features, target = "Satisfied", positive = "Yes")

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
