install.packages("car")
install.packages("carData")
install.packages("cowplot")
install.packages("ROCR")
install.packages("ROSE")
install.packages("rpart.plot")

library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(carData)
library(car)
library(cowplot)
library(gridExtra)
library(corrplot)
library(MASS)
library(ROCR)
library(ROSE)
library(rpart)
library(rpart.plot)

# Reading files
getwd()
setwd("C:\\Users\\Acc\\Desktop\\Meri SKILL Internship\\Projects\\Project 3 - HR Analytics")

general_data <- read.csv('HR-Employee-Attrition.csv', stringsAsFactors = TRUE, header = TRUE, sep = ',')

employee_survey_data <- read.csv('Employee-Survey_data.csv', stringsAsFactors = TRUE,header = TRUE,sep = ',')

manager_survey_data <- read.csv('Manager_Survey-data.csv', stringsAsFactors = TRUE,header = TRUE, sep = ',')

in_time_raw <- read.csv('In-time.csv', stringsAsFactors = TRUE,header = TRUE, sep = ',')

out_time_raw <- read.csv('Out-time.csv', stringsAsFactors = TRUE,header = TRUE, sep = ',')

# Exploring the dataframes

str(employee_survey_data)
str(manager_survey_data)
str(general_data)

# Checking for missing values

sapply(employee_survey_data, function(x) sum(is.na(x)))/nrow(employee_survey_data)*100

sapply(manager_survey_data, function(x) sum(is.na(x)))/nrow(manager_survey_data)*100

sapply(general_data, function(x) sum(is.na(x)))/nrow(general_data)*100

in_time <- sapply(in_time_raw, function(x) as.POSIXlt(x, origin = '1970-01-01', format='%y-%m-%d %H:%M:%S'))
in_time <- as.data.frame(in_time)

out_time <- sapply(out_time_raw, function(x) as.POSIXlt(x, origin = '1970-01-01',format='%y-%m-%d %H:%M:%S'))
out_time <- as.data.frame(out_time)

# Removing the first column since there has no value in the data

in_time <- in_time[,c(1:ncol(in_time))]

out_time <- out_time[,c(1:ncol(out_time))]

# Computing the difference between out_time and in_time

hours_worked_raw <- out_time - in_time

# We want to keep the records even if there was one employee that went to work

hours_worked_raw <- hours_worked_raw[,colSums(is.na(hours_worked_raw)) < nrow(hours_worked_raw)]

# Transforming the resuls to numeric so we can take the average

hours_worked_raw <- sapply(hours_worked_raw, function(x) as.numeric(x))

hours_worked_raw <- as.data.frame(hours_worked_raw)

# Taking the average of working hours per employee
AvrWorkHrs <- apply(hours_worked_raw, 1, mean, na.rm = TRUE) # 1 Is to indicate rows

# Creating ID column so it can be merged with the rest of the datasets

id_vector <- hours_worked_raw
AverageWorkedHours <- data.frame(EmployeeID = id_vector, AvrWorkHrs = AvrWorkHrs)

setdiff(general_data$EmployeeID, AverageWorkedHours$EmployeeID)
# Before merging we take a look if each observation is from a different employee

setdiff(employee_survey_data$EmployeeID, manager_survey_data$EmployeeID)

setdiff(manager_survey_data$EmployeeID, general_data$EmployeeID)

# Since all of them are complete we can merge them

general_data_merged <- inner_join(general_data,manager_survey_data, by = 'EmployeeID') %>%
  inner_join(., employee_survey_data, by = 'EmployeeID')
  
# Droping values that have the same value for all observations

same_values <- nearZeroVar(general_data_merged, names = TRUE)
general_data_merged <- general_data_merged %>%
  dplyr::select(-c(c('EmployeeID', same_values)))

# Now we check the structure of the new dataframe and the missing values. And evaluate if they are significant in terms of the total observations

str(general_data_merged)

sapply(general_data_merged, function(x) sum(is.na(x)))/nrow(general_data_merged)*100

# We have a very small proportion of missing values per feature. We translate those in terms of observations

flag_nulls <- ifelse(rowSums(is.na(general_data_merged)) == 0, 0, 1)
sum(flag_nulls)/nrow(general_data_merged)*100 

# Since missing values represent 2.5% of the observations we remove them (just for this case, one should be careful when dropping missing values).

general_data_merged <- general_data_merged[rowSums(is.na(general_data_merged)) == 0,]

# First we keep the numeric variables

nums <- unlist(lapply(general_data_merged, is.numeric))
general_data_merged_with_ordinal_values <- general_data_merged[, nums]

# With this data we create a correlation matrix in order to see the relantionships between the features. Since the data contains ordinal data (hierarchy or ranks) we use Spearman correlation instead of Pearson.

cor_matrix <- cor(general_data_merged_with_ordinal_values, method = 'spearman')
corrplot(corr = cor_matrix, method = 'color', addCoef.col = 'gray',tl.cex = 0.7, number.cex = 0.5)

# We create the second dataframe with the labels of the ordinal features

general_data_merged_with_categories <- general_data_merged

general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 1)] <- 'Below College'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 2)] <- 'College'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 3)] <- 'Bachelor'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 4)] <- 'Master'
general_data_merged_with_categories$Education[which(general_data_merged_with_categories$Education == 5)] <- 'Doctor'

general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 1)] <- 'Low'
general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 2)] <- 'Medium'
general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 3)] <- 'High'
general_data_merged_with_categories$EnvironmentSatisfaction[which(general_data_merged_with_categories$EnvironmentSatisfaction == 4)] <- 'Very High'

general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 1)] <- 'Low'
general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 2)] <- 'Medium'
general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 3)] <- 'High'
general_data_merged_with_categories$JobInvolvement[which(general_data_merged_with_categories$JobInvolvement == 4)] <- 'Very High'

general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 1)] <- 'Low'
general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 2)] <- 'Medium'
general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 3)] <- 'High'
general_data_merged_with_categories$JobSatisfaction[which(general_data_merged_with_categories$JobSatisfaction == 4)] <- 'Very High'

general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 1)] <- 'Low'
general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 2)] <- 'Good'
general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 3)] <- 'Excellent'
general_data_merged_with_categories$PerformanceRating[which(general_data_merged_with_categories$PerformanceRating == 4)] <- 'Outstanding'

general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 1)] <- 'Bad'
general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 2)] <- 'Good'
general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 3)] <- 'Better'
general_data_merged_with_categories$WorkLifeBalance[which(general_data_merged_with_categories$WorkLifeBalance == 4)] <- 'Best'

general_data_merged_with_categories %>%
  group_by(Gender, Education) %>%
  summarize(Average_Income = mean(MonthlyIncome)) %>%
  arrange(-Average_Income)

general_data_merged_with_categories %>%
  group_by(MaritalStatus, WorkLifeBalance) %>%
  summarize(Average_Work = mean(AvrWorkHrs), Percent_employees = round(n()/nrow(general_data_merged_with_categories)*100,0)) %>%
  arrange(-Percent_employees)

g1 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = Education)) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~Education) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Attrition by Education', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g2 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = EnvironmentSatisfaction)) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~EnvironmentSatisfaction) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Environment satisfaction vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g3 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = JobInvolvement)) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~JobInvolvement) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Job involvement vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g4 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = JobSatisfaction)) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~JobSatisfaction) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Job Satisfaction vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g5 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = PerformanceRating)) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~PerformanceRating) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Performance Rating vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g6 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = WorkLifeBalance)) +
  geom_bar(aes(y = after_stat(prop), fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~WorkLifeBalance) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Work-life balance vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

plot_grid(g1,g2,g3,g4,g5,g6, nrow = 3)

# Plots of Attrition vs Age, Average working hours, Monthly income, Gender, Marital status, and Department.

g7 <-  ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), y = Age, fill = Attrition)) +
  geom_boxplot() +
  xlab('')+
  theme(legend.position = 'none')

#g8 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), y = AvrWorkHrs, fill = Attrition)) +
#  geom_boxplot()+
# xlab('')+
#  theme(legend.position = 'none')

g9 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), y = log(MonthlyIncome), fill = Attrition)) +
  geom_boxplot()+
  xlab('')+
  ylab('Log of Monthly Income')+
  theme(legend.position = 'none')

g10 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = Gender)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~Gender) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Gender vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g11 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = MaritalStatus)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~MaritalStatus) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Marital Status vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

g12 <- ggplot(data = general_data_merged_with_categories, aes(x = factor(Attrition), group = Department)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = 'count', position = 'stack') +
  facet_grid(~Department) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..prop..),2)), y = ..prop..), stat = 'count', vjust = -.2, size = 3) +
  labs(y = 'Percent', x = 'Attrition', title = 'Department vs Attrition', fill = 'Attrition')+
  theme_light() +
  theme(legend.position = 'none')

plot_grid(g7,g9,g10,g11,g12, nrow = 3)

# Based on the findings in EDA we compute the first model based relevant features.

mod_a <- glm(formula =Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
               Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
               JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
               , data = general_data_merged, family = binomial)

summary(mod_a)

vif(mod = mod_a)

mod_b <- stepAIC(mod_a, direction = 'both', trace = FALSE)

summary(mod_b)

vif(mod = mod_b)

predicted_data <- data.frame(prob_of_attrition = mod_b$fitted.values, attrition = general_data_merged$Attrition)

predicted_data <- predicted_data %>%
  arrange(prob_of_attrition) %>%
  mutate(rank = 1:nrow(predicted_data))

ggplot(data = predicted_data, aes(x = rank, y = prob_of_attrition))+
  geom_point(aes(color = attrition), alpha = 1, shape = 4, stroke = 2)+
  labs(y = 'Predicted probability of attrition', x = 'Index', title = 'Probability of attrition vs Actual attrition')

# Setting seed for reproducibilty

set.seed(123)

# Data split

training.sample <- general_data_merged$Attrition %>%
  createDataPartition(p = 0.8, list = FALSE)


train.data <- general_data_merged[training.sample,]
test.data <- general_data_merged[-training.sample,]

mod_c <- glm(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
               Gender+JobLevel+MaritalStatus+log(MonthlyIncome)+NumCompaniesWorked+YearsAtCompany+
               JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
               , data = train.data, family = 'binomial')

mod_c_StepWise <- stepAIC(mod_c, direction = 'both', trace = FALSE)

summary(mod_c_StepWise)

p.training <- predict(mod_c_StepWise, train.data, type = 'response') # vector of probabilites from the training dataset

p.training.attrition <- as.factor(ifelse(p.training > 0.5, 'Yes', 'No')) # transforming those probabilites into factors (Yes or No)

# Obtaining the confusion matrix for this model in our training data

confusionMatrix(p.training.attrition, train.data$Attrition)

# Testing our results in the test dataset with threshold = 0.5

p.test <- predict(mod_c_StepWise, test.data, type = 'response')
p.test.attrition <- as.factor(ifelse(p.test > 0.5, 'Yes','No'))
confusionMatrix(p.test.attrition, test.data$Attrition)

# Selecting a different treshold based on ROC curve

ROCR_prediction <- prediction(p.training, train.data$Attrition)
ROCR_performance <- performance(ROCR_prediction, 'tpr', 'fpr')

plot(ROCR_performance, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

# Based on this we select the new threshold to be 0.2 and test our model again.
# Evaluating in training dataset withe new threshold

p.training <- predict(mod_c_StepWise, train.data, type = 'response')
p.training.attrition <- as.factor(ifelse(p.training > 0.2, 'Yes', 'No'))
confusionMatrix(p.training.attrition, train.data$Attrition)

# Evaluating in test dataset

p.test <- predict(mod_c_StepWise, test.data, type = 'response')
p.test.attrition <-  as.factor(ifelse(p.test> 0.2, 'Yes', 'No'))

confusionMatrix(p.test.attrition, test.data$Attrition)

# Using oversampling in the training dataset
over <- ovun.sample(formula = Attrition ~., data = train.data, method = 'over')$data

table(over$Attrition) # now the data is balanced between No and Yes

# Building the model

balanced_model <- glm(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                        Gender+JobLevel+MaritalStatus+log(MonthlyIncome)+NumCompaniesWorked+YearsAtCompany+
                        JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
                        , data = over, family = 'binomial')

# Model output

summary(balanced_model)

# Step wise

balanced_model_step_wise <- stepAIC(balanced_model, direction = 'both', trace = FALSE)

# Step wise model output

summary(balanced_model_step_wise)

p.training <- predict(balanced_model_step_wise, train.data, type = 'response')

p.training.attrition <- as.factor(ifelse(p.training > 0.5, 'Yes', 'No'))

confusionMatrix(p.training.attrition, train.data$Attrition)

p.test <- predict(balanced_model_step_wise, test.data, type = 'response')

p.test.attrition <- as.factor(ifelse(p.test > 0.5, 'Yes', 'No'))

confusionMatrix(p.test.attrition, test.data$Attrition)

# Setting seed for reproducibilty

set.seed(123)

# Data split

training.sample <- general_data_merged$Attrition %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data <- general_data_merged[training.sample,]
test.data <- general_data_merged[-training.sample,]

# Decision tree model

model.tree <- rpart::rpart(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                             Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                             JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
                             , data = train.data, method = 'class')

# Plotting decision tree
rpart.plot::prp(model.tree)

predict_tree <- predict(model.tree, test.data, type = 'class')

confusionMatrix(predict_tree, test.data$Attrition)

over <- ovun.sample(formula = Attrition ~., data = train.data, method = 'over')$data

model.tree <- rpart::rpart(formula = Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                             Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                             JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
                             , data = over, method = 'class')

# Plotting decision tree
rpart.plot::prp(model.tree)

predict_tree <- predict(model.tree, test.data, type = 'class')

confusionMatrix(predict_tree, test.data$Attrition)

set.seed(123)

partition <- createDataPartition(general_data_merged$Attrition, p = 0.8, list = FALSE)

training <- general_data_merged[partition,]
test <- general_data_merged[-partition,]

install.packages('randomForest')
library(randomForest)
# Random forest with cross-validation
model_rf_1 <- randomForest::randomForest(Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                                           Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                                           JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
                                           , data = training, trControl = trainControl(method = 'cv', 10))

# Printing the model
model_rf_1

# Evaluation of the model with useen data
p <- predict(model_rf_1, test, type = 'response')

confusionMatrix(p, test$Attrition)

# Plotting error rate
plot(model_rf_1, main = 'Error rate vs number of trees')

# Plotting mtry
t <- randomForest::tuneRF(training[,-2], training[,2], stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)

model_rf_2 <- randomForest::randomForest(Attrition ~ Age+BusinessTravel+Department+DistanceFromHome+Education+
                                           Gender+JobLevel+MaritalStatus+MonthlyIncome+NumCompaniesWorked+YearsAtCompany+
                                           JobInvolvement+PerformanceRating+EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance
                                           , data = training, ntree = 100, trControl = trainControl(method = 'cv', 10))

model_rf_2


p <- predict(model_rf_2, test, type = 'response')

confusionMatrix(p, test$Attrition)

hist(randomForest::treesize(model_rf_2), main = 'No. of nodes', xlab = '', col = 'forestgreen')

# Plotting features based on importance
randomForest::varImpPlot(model_rf_2, main = 'Feature Importance')
# When the Average working hours is more than 7 it tends to predict Attrition ('Yes')

randomForest::partialPlot(model_rf_2, training, Age, 'Yes')