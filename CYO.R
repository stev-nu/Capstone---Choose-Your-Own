###Choose your own project for Capstone

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(vip)) install.packages("vip", repos = "http://cran.us.r-project.org")  
if(!require(pdp)) install.packages("pdp", repos = "http://cran.us.r-project.org")  
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")

library(MASS)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(gt)
library(scales)
library(gridExtra)
library(randomForest)
library(janitor)
library(ranger)
library(e1071)
library(vip)
library(pdp)
library(caTools)
library(pROC)

#loading human resources datasets 
#loading human resources train dataset for model tuning
library(readr)
train <- read_csv("https://raw.githubusercontent.com/stev-nu/Capstone---Choose-Your-Own/main/train.csv")
View(train)


#saving the datasets
saveRDS(train, file = "train.Rds")

#examining the datasets to see the dimensions, number of columns, number of unique entries for variables etc

#dimensions of datasets
dim(train)  #54808 rows and 14 columns

#viewing column names we realize some are not in a tidy format
#making untidy column names tidy, note original names are maintained
#changing column names "KPIs_met >80%" and "awards_won?"
train <- train %>% rename(KPI = `KPIs_met >80%`) %>% #changing KPIs_met >80% to KPIs
  rename(awards_won = `awards_won?`) #changing awards_won? to awards_won

#check for any missing values
any(is.na(train)) #returns TRUE, ie there are missing values

#for simplicity of the analysis we will remove all rows that have all or some missing values
#renaming processed datatsets 
train_drop_na <- drop_na(train)
any(is.na(train_drop_na)) #rechecking to ensure there are no missing values, returns FALSE

#saving the processed dataset with no missing values 
saveRDS(train_drop_na, file = "train_drop_na.Rds")

#examination of the data also shows that the employee id variable is random and as such will not add to the analysis of predicting likelihood
#we will drop the employee id variable
train_drop_eid <- train_drop_na %>% dplyr::select(-employee_id)


#setting categorical variables as factor using mutate function
train_drop_eid_factor <- train_drop_eid %>% 
  mutate(KPI = as.factor(KPI),
         department = as.factor(department),
         region = as.factor(region),
         education = as.factor(education),
         gender = as.factor(gender),
         recruitment_channel = as.factor(recruitment_channel),
         previous_year_rating = as.factor(previous_year_rating),
         awards_won = as.factor(awards_won),
         is_promoted = as.factor(is_promoted)) %>% 
  clean_names()


#examine class of variables in train_drop_na_factor
sapply(train_drop_eid_factor, class)

#splitting the data into to train and test datasets doing a stratified split to ensure the distribution is similar
set.seed(1, sample.kind="Rounding") # set seed of random numbers so results can be replicated...if using R3.5 or earlier, use set.seed(1)
test_index <- createDataPartition(y = train_drop_eid_factor$kpi, times = 1, p = 0.2, list = FALSE)
hr_train <- train_drop_eid_factor[-test_index,]   #this is the training dataset
hr_test <- train_drop_eid_factor[test_index,]     #this is the testing dataset

#to ensure the variable kpi is in line with R principles, changing all ones to yes and all zeros to no for both the hr_train and hr_test datasets
hr_train <- hr_train %>% 
  mutate(kpi = if_else(kpi==1,"yes", "no"))

hr_test <- hr_test %>% 
  mutate(kpi = if_else(kpi==1,"yes", "no"),
        kpi = as.factor(kpi))

#checking similarity in the distribution
mean(hr_train$kpi=="yes") #mean is 0.3564621
mean(hr_test$kpi=="yes") #mean is 0.3565191, confirms similar distribution

#save hr_train and hr_test data file to be used in Rmarkdown
saveRDS(hr_train, file = "hr_train.Rds")
saveRDS(hr_test, file = "hr_test.Rds")

#first 6 entries of the hr_train dataset
head_hr_train <- hr_train %>% head()

#saving first 6 rows of hr_train dataset
saveRDS(head_hr_train, file = "head_hr_train.Rds")

#create two tables showing the first 6 rows of hr_train
#####TABLE 1a
head_hr_train %>% 
  dplyr::select(1:7) %>% gt() #table selecting columns 1 to 7
#####TABLE 1b
head_hr_train %>% 
  dplyr::select(8:13) %>% gt() #table selecting columns 8 to 13

#examining number of distinct observations for some variables
hr_train %>% summarize( number_of_departments = n_distinct(department), #number of distinct departments
                        number_of_regions = n_distinct(region), #number of distinct regions
                        number_of_rchannels = n_distinct(recruitment_channel), #number of distinct recruitment channels
                        number_of_educationlevels = n_distinct(education)) #number of distinct education levels

#looking at the number of employees that would have met more than 80% of their KPIs versus those who didn't
###FIGURE 1
hr_train %>% group_by(kpi) %>% 
  tally() %>% 
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>% 
  ggplot(aes(x = kpi, y = n, fill = kpi)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = c(0,1)) +
  theme_minimal() +
  labs(y = "Number of Employees") +
  ggtitle("Figure 1: Count of Employees Meeting or Not Meeting >80% of KPIs") +
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))


#proportion of employees that met above 80% of thier KPIs
employee_kpi_share <- as.data.frame(prop.table(table(hr_train$kpi)))

#creating a table to show the proportion of employees that met more than 80% of their KPIs
###TABLE 2
employee_kpi_share %>% 
  rename("Met > 80% of KPIs" = Var1) %>% 
  rename("Propotion of Employees" = Freq) %>% 
  gt()

#exploratory analysis, examining each variable using data visualization to determine if they
#will be good predictors for the models

#exploring the age variable using a histogram to see the dominant age group of employees
####FIGURE 2
#calculating the mean age
#showing age distribution by kpi relative to the mean age
mean_age <- mean(hr_train$age)

hr_train %>% 
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>% 
  ggplot(aes(x=age, fill = kpi)) +
  geom_histogram() +
  geom_vline(xintercept = mean_age, colour="black") +
  facet_wrap(vars(kpi), ncol=1) +
  labs(x="Age", y="Number of Employees") +
  ggtitle("Figure 2: Employee Age Distribution by kpi")


#exploring the average training score variable using a histogram to see the dominant age group of employees
####FIGURE 3
mean_avg_training_score = mean(hr_train$avg_training_score)  # mean of average training score of individuals

hr_train %>%
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>%
  ggplot(aes(x=avg_training_score, fill = kpi))+
  geom_vline(xintercept = mean_avg_training_score, colour="black")+
  geom_histogram() +
  facet_wrap(vars(kpi), ncol=1) +
  labs(x ="Average Training Score", y="Number of Employees") +
  ggtitle("Figure 3: Employee Average Training Score Distribution by kpi")

#examining the influence potential independent variables have on an employee meeting more than 80% of their KPIs
#using bar charts and boxplots

#calculating proportion of persons that met greater than 80% of KPI for each region and creating a plot
plotregion <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>% 
  group_by(region, kpi) %>% 
  summarise(n=n()) %>% 
  group_by(region) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=region, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 4: KPI Proportion by Region",
       x="Region", y="Proportion")+
  coord_flip()+
  theme_bw()
plotregion 

#age
#using ggplot to plot a boxplot of the age variable
plotage <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>% 
  ggplot(aes(y=age, fill = kpi)) + 
  geom_boxplot(alpha = 0.3) +
  labs(title = "Figure 5a: Age Boxplot by kpi")
plotage

#length of service
plotservice <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>% 
  ggplot(aes(length_of_service, fill = kpi)) + 
  geom_boxplot(alpha = 0.3) + coord_flip() +
  labs(title = "Figure 5b: Length of Service Boxplot by kpi")
plotservice

#average training score
#using ggplot to plot a boxplot of the average training score variable
plotscore <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes","Met > 80% of KPIs","Did Not Meet > 80% of KPIs")) %>% 
  ggplot(aes(avg_training_score, fill = kpi)) + 
  geom_boxplot(alpha = 0.3) + coord_flip() +
  labs(title = "Figure 5c: Avg Training Score Boxplot by kpi")
plotscore


#calculating proportion of persons that met greater than 80% of KPI for each department and creating a plot
plotdepartment <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(department,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(department) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=department, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 6a: KPI Proportion by Department",
       x="Department", y="Proportion")+
  coord_flip()+
  theme_bw()
plotdepartment

#calculating proportion of persons that met greater than 80% of KPI for each education level and creating a plot
plotedu <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(education,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(education) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=education, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 6b: KPI Proportion by Education",
       x="Education Level", y="Proportion")+
  coord_flip()+
  theme_bw()
plotedu

#calculating proportion of persons that met greater than 80% of KPI for each gender and creating a plot
plotgender <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(gender,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(gender) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=gender, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 7a: KPI Proportion by Gender",
       x="Gender", y="Proportion")+
  coord_flip()+
  theme_bw()
plotgender


#calculating proportion of persons that met greater than 80% of KPI for each recruitment channel and creating a plot
plotrecruit <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(recruitment_channel,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(recruitment_channel) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=recruitment_channel, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 7b: KPI Proportion by Recruitment Channel",
       x="Recruitment Channel", y="Proportion")+
  coord_flip()+
  theme_bw()
plotrecruit


#calculating proportion of persons that met greater than 80% of KPI for each number of trainings received and creating a plot
plottraining <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(no_of_trainings,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(no_of_trainings) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=as.factor(no_of_trainings), y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 7c: KPI Proportion by Number of Trainings undertaken",
       x="Number of Trainings Undertaken", y="Proportion")+
  coord_flip()+
  theme_bw()
plottraining


#calculating proportion of persons that met greater than 80% of KPI for each previous year rating and creating a plot
plotrating <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(previous_year_rating,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(previous_year_rating) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=previous_year_rating, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 8a: KPI Proportion by Previous Year Ratings",
       x="Previous Year Rating", y="Proportion")+
  coord_flip()+
  theme_bw()
plotrating


##calculating proportion of persons that met greater than 80% of KPI for if an award was received or not and creating a plot
plotawards <-   hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(awards_won,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(awards_won) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=awards_won, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 8b: KPI Proportion by Awards Won",
       x="Awards Won", y="Proportion")+
  coord_flip()+
  theme_bw()
plotawards

#calculating the proportion of persons that met greater than 80% of KPI for where a person is promoted or not
plotpromo <- hr_train %>% 
  mutate(kpi = if_else(kpi=="yes", "Met >80% of KPIs", "Did Not Meet > 80% of KPIs")) %>% 
  group_by(is_promoted,kpi) %>% 
  summarise(n=n()) %>% 
  group_by(is_promoted) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=is_promoted, y=prop, fill=kpi))+
  geom_col(position = "dodge")+
  labs(title = "Figure 8c: KPI Proportion by Whether a person was Promoted",
       x="Received Promotion", y="Proportion")+
  coord_flip()+
  theme_bw()
plotpromo

#using the grid.arrange function to group some individual graphs as grids, instead of having multiple individual graphs
##FIGURE 5
grid.arrange(plotage, plotservice, plotscore, nrow=3, 
             top = "Figure 5: Boxplots Showing Age, Length of Service & Average Training Score by KPIs")
##FIGURE 6
grid.arrange(plotdepartment, plotedu, nrow = 2,
             top = "Figure 6: Bar Charts Showing KPI Proportion for Department and Education Level")
##FIGURE 7
grid.arrange(plotgender, plotrecruit, plottraining, nrow = 3,
             top = "Figure 7: Bar Charts Showing KPI Proportion for Gender, Recruitment Channel and Number of Training Received")
##FIGURE 8
grid.arrange(plotrating, plotawards, plotpromo, nrow = 3,
             top = "Figure 8: Bar Charts Showing KPI Proportion for Previous Year Ratiing, Receiving an Award and Receiving a Promotion")


#MODEL ANALYSIS
set.seed(1, sample.kind="Rounding")
myFold <- createFolds(hr_train$kpi, k=5)

set.seed(1, sample.kind="Rounding")
myControl <- trainControl(
    method = "cv", 
    number = 5,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE,
    index = myFold
  )

#glm model
set.seed(1, sample.kind="Rounding")
train_glm <- train(
  kpi ~ ., 
  hr_train,
  metric = "ROC",
  method = "glm",
  trControl = myControl,
)

#save results for glm
results_glm <- train_glm$results  # save results of the glm model
saveRDS(results_glm, file = "results_glm.Rds")  # save results of glm model


#lda model
set.seed(1, sample.kind="Rounding")
train_lda <- train(
  kpi ~ ., 
  hr_train,
  metric = "ROC",
  method = "lda",
  trControl = myControl,
)

#save results for lda model
results_lda <- train_lda$results  # save results of the lda model
saveRDS(results_lda, file = "results_lda.Rds")  # save results of lda model


#random forest
set.seed(1, sample.kind="Rounding")
train_rf6 <- train(
  kpi ~ ., 
  hr_train,
  metric = "ROC",
  method = "rf",
  trControl = myControl,
  tuneGrid = data.frame(mtry = seq(5, 11, 1)),
)

train_rf6$results %>% 
  ggplot(aes(x=mtry, y=ROC)) +
  geom_line() +
  geom_point() +
  ggtitle("Figure 9: Random Forest Receiver Operator Character")

#save results for random forest 6 model
results_rf <- train_rf6$results  # save results of the random forest model
saveRDS(results_rf, file = "results_rf.Rds")  # save results of random forest model

# Make a list
model_list <- list(
  glm = train_glm,  # model name1
  lda = train_lda,   # model name 2
  rf = train_rf6  # model name3
)

# collect resamples from the cv folds
resample_HR <- resamples(model_list)
resample_HR
summary(resample_HR)



#plotting variable importance
vip_glm <- vip(train_glm, method="firm", scale = TRUE)+
  labs(title = "Figure 10a: GLM")+
  theme_bw()

vip_lda <- vip(train_lda, method="firm", scale = TRUE)+
  labs(title = "Figure 10b: LDA")+
  theme_bw()

vip_rf <- vip(train_rf6, method="firm", scale=TRUE) +
  labs(title = "Figure 10c: Random Forest")+
  theme_bw()

grid.arrange(vip_glm, vip_lda, vip_rf, nrow=2, top="Figure 10: Variable Importance")

#box plots comparing metrics of the models, figure 11
plotroc <- bwplot(resample_HR, metric = "ROC", 
       ylab = "Model",
       main ="Figure 11a: Comparing the ROC for Each Model") #comparing ROC for each model

plotsens <- bwplot(resample_HR, metric = "Sens", 
       ylab = "Model",
       main ="Figure 11b: Comparing the Sensitivity for Each Model") #comparing sensitivity for each model, figure 10

plotspec <- bwplot(resample_HR, metric = "Spec", 
       ylab = "Model",
       main ="Figure 11c: Comparing the Specificity for Each Model") #comparing specificity for each model, figure 10

grid.arrange(plotroc, plotsens, plotspec, nrow=2, top="Figure 11: Comparing Metrics for Each Model")


#predict using best model using the test dataset
hr_predict <- predict(train_rf6, hr_test) %>% factor(levels = levels(hr_test$kpi))

#saving hr_predict
saveRDS(hr_predict, file = "hr_predict.Rds") 

#applying the chosen model to the test dataset using roc
roc(response = hr_test$kpi %>% ordered(), 
    hr_predict %>% ordered(), 
    smoothed = TRUE,
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE, # arguments for plot
    print.auc=TRUE, show.thres=TRUE)

#confusion matrix
confusionMatrix(hr_predict, hr_test$kpi, positive = "yes")
