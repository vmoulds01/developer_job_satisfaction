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


cleaned_data_file_2020 = 'datasets/interim/2020_clean_data_pre_factor_reduction.csv'
clean_data_2020 <- read_csv(cleaned_data_file_2020)
clean_data_2020 <- clean_data_2020 %>% mutate_if(is.character, as.factor)
summary(clean_data_2020)

initial_information_gain_ratio <- information_gain_ratio_feature_importance(clean_data_2020)
print(initial_information_gain_ratio)

summary(clean_data_2020$JobSat)
data_satisfied <- clean_data_2020 %>% mutate(Satisfied = ifelse(JobSat %in% c('Slightly satisfied', 'Very satisfied'), "Yes","No"))
data_satisfied <- data_satisfied[, !(names(data_satisfied) %in% c('JobSat'))]

satisfied_information_gain_ratio <- information_gain_ratio_target(data_satisfied, 'Satisfied')
print(satisfied_information_gain_ratio)


############################################################# PLOTS ###################################################################################

####################################################### PLOT JOB SATISFACTION #########################################################################

ggplot(clean_data_2020) + 
  geom_bar(aes(y = reorder(JobSat, table(JobSat)[JobSat])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Job Satisfaction 2020", y="Satisfaction Response", x="Counts") 

######################################################### PLOT SATISFACTION ###########################################################################

ggplot(data_satisfied, aes(x = Satisfied)) + 
  geom_bar(position = position_dodge(width=0.5), fill="lightblue", color="darkblue") + 
  geom_text(aes(label=scales::percent(stat(prop)), group=1), stat='count', vjust = -.5) +
  labs(title="Satisfied Distribution 2020", x="Satisfied", y="Counts") 

###################################################### PLOT JOB SEEK VS SATISFIED ######################################################################

unique(data_satisfied$JobSeek)

data_satisfied$JobSeek <- gsub("I am actively looking for a job", "Actively Looking", data_satisfied$JobSeek)
data_satisfied$JobSeek <- gsub("I am not interested in new job opportunities", "Not Interested", data_satisfied$JobSeek)
data_satisfied$JobSeek <- gsub("I’m not actively looking, but I am open to new opportunities", "Not Actively Looking", data_satisfied$JobSeek)

ggplot(data=data_satisfied, aes(x=reorder(JobSeek, table(JobSeek)[JobSeek]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="2020 Job Seek Variance For Satisfied", y="Counts", x="Job Seek", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

level_order <- c('Not Interested', 'Not Actively Looking', 'Actively Looking', 'Missing')


ggplot(data_satisfied, aes(x =factor(JobSeek, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Job Seek Category 2020", y="Satisfied Proportion", x="Job Seek Category", fill="Satisfied") + 
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
  labs(title = "Responses Per Country 2020", fill="Responses")

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
  labs(title = "Proportion Satisfied Per Country 2020")

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
  labs(title="Age Variance Satisfication 2020", y="Counts", x="Age", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x =factor(AgeBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Job Seek Category 2020", y="Satisfied Proportion", x="Age", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

###################################################### PLOT GENDER VS SATISFIED #########################################################################

unique(data_satisfied$Gender)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Gender, table(Gender)[Gender])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Gender Distribution 2020", y="Gender Response", x="Counts") 

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
  labs(title="Proportion Satisfied Per Gender 2020", y="Satisfied Proportion", x="Gender", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

###################################################### PLOT SALARY VS SATISFIED #########################################################################

unique(data_satisfied$SalaryUSDBinned)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(SalaryUSDBinned, table(SalaryUSDBinned)[SalaryUSDBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Salary Distribution 2020", y="Salaray Response", x="Counts") 

salary_data_satisfied <- data_satisfied[!(data_satisfied$SalaryUSDBinned %in% c("Missing")), ]
summary(salary_data_satisfied$SalaryUSDBinned)

ggplot(salary_data_satisfied) + 
  geom_bar(aes(y = reorder(SalaryUSDBinned, table(SalaryUSDBinned)[SalaryUSDBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Salary Distribution 2020", y="Salaray Response", x="Counts") 

level_order <- c('<5028', '5028-9228', '9228-13620', '13620-18996', '18996-24648', '24648-30384', '30384-36444', '36444-42036', '42036-47996', 
                 '47996-54049', '54049-60000', '60000-66716', '66716-75000', '75000-84000', '84000-95000', '95000-110000', '110000-129588', 
                 '129588-160000', '160000-275000', '>275000', 'Missing')

ggplot(data=salary_data_satisfied, aes(x=factor(SalaryUSDBinned, level=level_order), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Salary Variance Satisfication 2020", y="Counts", x="Salary USD", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(salary_data_satisfied, aes(x = factor(SalaryUSDBinned, level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Salary USD 2020", y="Satisfied Proportion", x="Salaray USD", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

#################################################### PLOT EMPLOYMENT VS SATISFIED #####################################################################

unique(data_satisfied$Employment)

data_satisfied$Employment <- gsub("Employed full-time", "Full-time", data_satisfied$Employment)
data_satisfied$Employment <- gsub("Employed part-time", "Part-time", data_satisfied$Employment)
data_satisfied$Employment <- gsub("Independent contractor, freelancer, or self-employed", "Independent", data_satisfied$Employment)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Employment, table(Employment)[Employment])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Employment Distribution 2020", y="Employment Response", x="Counts") 

level_order <- c('Full-time', 'Independent', 'Part-time', 'Missing')

ggplot(data=data_satisfied, aes(x=factor(Employment, level=level_order), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Employment Variance Satisfication 2020", y="Counts", x="Employment", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(Employment, level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Employment Type 2020", y="Satisfied Proportion", x="Employment Type", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

###################################################### PLOT LEARN VS SATISFIED #####################################################################

summary(data_satisfied$Learn)


ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Learn, table(Learn)[Learn])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Learn New Language Distribution 2020", y="Learn New Language Response", x="Counts") 

ggplot(data=data_satisfied, aes(x=reorder(Learn, table(Learn)[Learn]), fill=Satisfied)) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="Learning Rate Variance Satisfication 2020", y="Counts", x="Learn New Language Response", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(Learn), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Learning New Language Rate 2020", y="Satisfied Proportion", x="Learn New Language Response", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

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
  labs(title="CompanySize Distribution 2020", y="CompanySize Response", x="Counts") 

level_order <- c('<10', '10 to 19', '20 to 99', '100 to 499', '500 to 999', '1,000 to 4,999', '5,000 to 9,999', '>10,000', 'Missing')

ggplot(data=data_satisfied, aes(x=factor(OrgSize, level=level_order), fill=factor(Satisfied))) +
  geom_bar(stat="count", position="stack")  + 
  labs(title="CompanySize Vs Satisfication", y="Counts", x="CompanySize", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied, aes(x = factor(OrgSize, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Company Size 2020", y="Satisfied Proportion", x="CompanySize", fill="Satisfied") + 
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
  labs(title="Developer Type Distribution 2020", x="Developer Types", y="Counts") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_dev_type) + 
  geom_bar(aes(y = reorder(DeveloperType, table(DeveloperType)[DeveloperType])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Developer Type Distribution 2020", y="Developer Type Response", x="Counts") 

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
  labs(title="Language Desire Distribution 2020", x="Languages Desire", y="Counts") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))

ggplot(data_satisfied_languages_desire, aes(x = factor(isBash), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Bash Language Desired 2020", y="Satisfied Proportion", x="Bash Language Desire", fill="Satisfied") + 
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
  labs(title="YearsCode Distribution 2020", y="YearsCode Response", x="Counts") 

level_order <- c('0 to 2', '3 to 5', '6 to 8', '9 to 11', '12 to 14', '15 to 17', '18 to 20', '21 to 23', 
                 '24 to 26', '27 to 29', '>30', 'Missing')

ggplot(data_satisfied, aes(x = factor(YearsCodeBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Years Coding 2020", y="Satisfied Proportion", x="Years Code", fill="Satisfied") + 
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
  labs(title="Years Code Pro Distribution 2020", y="Years Code Pro Response", x="Counts") 

level_order <- c('0 to 2', '3 to 5', '6 to 8', '9 to 11', '12 to 14', '15 to 17', '18 to 20', '21 to 23', 
                 '24 to 26', '27 to 29', '>30', 'Missing')

ggplot(data_satisfied, aes(x = factor(YearsCodeProBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Years Coding Professional 2020", y="Satisfied Proportion", x="Years Code Professional", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


##################################################### PLOT WORK HRS VS SATISFIED ##################################################################

unique(data_satisfied$WorkWeekHrsBinned)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(WorkWeekHrsBinned, table(WorkWeekHrsBinned)[WorkWeekHrsBinned])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Work Week Hrs Distribution 2020", y="Work Week Hrs", x="Counts") 

level_order <- c('<37.5', '37.5-40', '40-45', '>=45', 'Missing')

ggplot(data_satisfied, aes(x = factor(WorkWeekHrsBinned, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Work Week Hrs 2020", y="Satisfied Proportion", x="Work Week Hrs", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))

################################################# PLOT OVERTIME VS SATISFIED ######################################################################

unique(data_satisfied$Overtime)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Overtime, table(Overtime)[Overtime])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Worked Overtime Distribution 2020", y="Worked Overtime Response", x="Counts") 

data_satisfied$Overtime <- gsub("Rarely: 1-2 days per year or less", "Rarely", data_satisfied$Overtime)
data_satisfied$Overtime <- gsub("Occasionally: 1-2 days per quarter but less than monthly", "Occasionally", data_satisfied$Overtime)
data_satisfied$Overtime <- gsub("Sometimes: 1-2 days per month but less than weekly", "Sometimes", data_satisfied$Overtime)
data_satisfied$Overtime <- gsub("Often: 1-2 days per week or more", "Often", data_satisfied$Overtime)

level_order <- c('Never', 'Rarely', 'Occasionally', 'Sometimes', 'Often', 'Missing')

ggplot(data_satisfied, aes(x = factor(Overtime, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Worked Overtime", y="Satisfied Proportion", x="Worked Overtime", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))


################################################## PLOT ONBOARD GOOD VS SATISFIED ######################################################################

unique(data_satisfied$OnboardGood)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(OnboardGood, table(OnboardGood)[OnboardGood])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Onboarding Good Distribution 2020", y="Onboarding Good Response", x="Counts") 

level_order <- c('What onboarding?', 'No', 'Yes', 'Onboarding?', 'Missing')

ggplot(data_satisfied, aes(x = factor(OnboardGood, level=level_order), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Onboarding Setup Good 2020", y="Satisfied Proportion", x="Onboarding Good Response", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=12, angle=45))


################################################### PLOT HOBBYIST VS SATISFIED ########################################################################

unique(data_satisfied$Hobbyist)

ggplot(data_satisfied) + 
  geom_bar(aes(y = reorder(Hobbyist, table(Hobbyist)[Hobbyist])), position = position_stack(reverse = TRUE), fill="lightblue", color="darkblue") + 
  labs(title="Hobby Coder Distribution 2020", y="Hobby Coder Response", x="Counts") 


ggplot(data_satisfied, aes(x = factor(Hobbyist), fill = factor(Satisfied))) +
  geom_bar(position="fill") +
  labs(title="Proportion Satisfied Per Coding As A Hobby", y="Satisfied Proportion", x="Hobby Coder", fill="Satisfied") + 
  theme(axis.text.x = element_text(face="plain", color="black", size=8, angle=45))


################################################## CHI2 SIGNIFICANCE TESTS FOR SATISFIED ################################################################

### Hypothesis - There is a difference between features for satisfied = yes vs satisfied = no
### Null Hypothesis - There is NO difference between feature for satisfied = yes vs satisfied = no

### Instantiate DataFrame to hold significance results

chi2_df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("feature", "xsq_p_value", "fisher_p_value"))

### Instantiate vector to hold list of features to calculate significance of proportional differences between satisfied and not satisfied people

cols_to_check <- c('Country', 'AgeBinned', 'SalaryUSDBinned', 'Employment', 'Learn', 'OrgSize', 'YearsCodeBinned', 'YearsCodeProBinned', 
                   'WorkWeekHrsBinned', 'Overtime', 'OnboardGood', 'Hobbyist')

### Loop over columns and build up significance results dataframe 

for (column_name in cols_to_check){
  M <- table(data_satisfied[, c("Satisfied", column_name)])
  Xsq <-chisq.test(M)
  fisherTest <- fisher.test(M, simulate.p.value=TRUE)
  
  temp_df<-data.frame(column_name, Xsq$p.value, fisherTest$p.value)
  names(temp_df)<-c("feature", "xsq_p_value", "fisher_p_value")
  
  chi2_df <- rbind(chi2_df, temp_df)
}


head(chi2_df, 12)

####################################################### CHI2 GENDER VS SATISFIED #########################################################################
gender_data_satisfied <- data_satisfied[,c('Gender', 'Satisfied')]
# Rename Man & Woman to Male & Female
levels(gender_data_satisfied$Gender)[levels(gender_data_satisfied$Gender)=="Man"] <- "Male"
levels(gender_data_satisfied$Gender)[levels(gender_data_satisfied$Gender)=="Woman"] <- "Female"
gender_data_satisfied <- gender_data_satisfied[gender_data_satisfied$Gender %in% c("Male", "Female"), ]
gender_data_satisfied$Gender <- as.factor(as.character(gender_data_satisfied$Gender))
summary(gender_data_satisfied$Gender)

M <- with(gender_data_satisfied,table(Satisfied, Gender))
Xsq <-chisq.test(M)
fisherTest <- fisher.test(M, simulate.p.value=TRUE)

temp_df<-data.frame("Gender", Xsq$p.value, fisherTest$p.value)
names(temp_df)<-c("feature", "xsq_p_value", "fisher_p_value")

chi2_df <- rbind(chi2_df, temp_df)

head(chi2_df, 13)


##################################################### CHI2 DEVTYPE VS SATISFIED #########################################################################

summary(data_satisfied_dev_type)

M <- with(data_satisfied_dev_type,table(Satisfied, DeveloperType))
Xsq <-chisq.test(M)
fisherTest <- fisher.test(M, simulate.p.value=TRUE)

temp_df<-data.frame("DeveloperType", Xsq$p.value, fisherTest$p.value)
names(temp_df)<-c("feature", "xsq_p_value", "fisher_p_value")

chi2_df <- rbind(chi2_df, temp_df)

head(chi2_df, 14)

##################################################### CHI2 LANGUAGE DESIRE VS SATISFIED ########################################################################

summary(data_satisfied_languages_desire)

### Instantiate DataFrame to hold significance results

lang_chi2_df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("feature", "xsq_p_value", "fisher_p_value"))

### Instantiate vector to hold list of features to calculate significance of proportional differences between satisfied and not satisfied people

lang_cols_to_check <- c('isAssembly', 'isBash', 'isDart', 'isPython', 'isJava', 'isJavaScript', 'isGo', 'isKotlin', 
                   'isRust', 'isRuby', 'isScala', 'isSQL')

### Loop over columns and build up significance results dataframe 

for (column_name in lang_cols_to_check){
  M <- table(data_satisfied_languages_desire[, c("Satisfied", column_name)])
  Xsq <-chisq.test(M)
  fisherTest <- fisher.test(M, simulate.p.value=TRUE)
  
  temp_df<-data.frame(column_name, Xsq$p.value, fisherTest$p.value)
  names(temp_df)<-c("feature", "xsq_p_value", "fisher_p_value")
  
  lang_chi2_df <- rbind(lang_chi2_df, temp_df)
}


head(lang_chi2_df, 12)


final_chi2_df <- rbind(chi2_df, lang_chi2_df)
ordered_chi2_df <- final_chi2_df[order(final_chi2_df$xsq_p_value),]
head(ordered_chi2_df, 15)

write.csv(ordered_chi2_df, file = "feature_satisfied_significance_results_2020.csv", row.names = FALSE)


###################################################### FEATURE ENGINEERING #############################################################################

###  Variables that appear to contribute to satisfaction are:
###  Country, Age, SalaryUSDBinned, Student, Company Size, DevType, languageDesiredIsBash, YearsCode, YearsCodePro, 
###  WorkWeekHrsBinned, Overtime, OnboardGood, Hobbyist 
###  Need to use devtype to build DeveloperType column
###  Need to use LanguageDesireNextYear to build languageDesiredIsBash column

data_satisfied_features <- data_satisfied %>% 
  select(Satisfied, Country, AgeBinned, SalaryUSDBinned, OrgSize, DevType, YearsCodeBinned, YearsCodeProBinned, 
         WorkWeekHrsBinned, Overtime, OnboardGood, Hobbyist, LanguageDesireNextYear) %>%
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

data_task = TaskClassif$new(id = 'devsurvey2020', backend = data_satisfied_features, target = "Satisfied", positive = "Yes")

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

# Analyse performance of models
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

write.csv(performance_measures_df, file = "model_performance_2020.csv", row.names = FALSE)


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


balanced_data_performance_measures_df <- data.frame(preformance_measures$learner_id, preformance_measures$accuracy_test, 
                                      preformance_measures$auc_test, preformance_measures$precision_test,
                                      preformance_measures$recall_test)

write.csv(balanced_data_performance_measures_df, file = "model_performance_balanced_data_2020.csv", row.names = FALSE)


########################################## ANALYSE LOGISTIC REGRESSION COEFFICIENTS ##############################################################

data_task = TaskClassif$new(id = 'surveyData2020', backend = data_satisfied_features, target = "Satisfied", positive = "Yes")
train_set = sample(data_task$nrow, 0.8 * data_task$nrow)
test_set = setdiff(seq_len(data_task$nrow), train_set)

lr_learner = lrn("classif.log_reg", id="lr")
lr_learner$predict_type = "prob"
lr_learner$train(adta_task, row_ids = train_set)
lr_learner$model
prediction_lr = lr_learner$predict(data_task, row_ids = test_set)
prediction_lr$confusion

auc_measure = msr("classif.auc")
precision_measure = msr("classif.precision")
recall_measure = msr("classif.recall")
accuracy = msr("classif.acc")

print(prediction_lr$score(accuracy))
print(prediction_lr$score(auc_measure))
print(prediction_lr$score(precision_measure))
print(prediction_lr$score(recall_measure))



















