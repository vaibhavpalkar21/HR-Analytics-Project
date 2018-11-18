#setwd("F:\\PGDDS\\HR_analytics group case study")
# Adding necessary libraries
library(ggplot2)
library(tidyr)
library(cowplot)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(lubridate)
library(GGally)
#Importing files
intime<-read.csv("in_time.csv",stringsAsFactors = FALSE)
outtime<-read.csv("out_time.csv",stringsAsFactors = FALSE)
mgr_survey<-read.csv("manager_survey_data.csv",stringsAsFactors = TRUE)
emp_survey<-read.csv("employee_survey_data.csv",stringsAsFactors = TRUE)
gendata<-read.csv("general_data.csv",stringsAsFactors = TRUE)
# All files have 4410 observations

######### UNDERSTANDING DATA######
#Making columnnames same for all files
colnames(intime)[1] <- "EmployeeID"
colnames(outtime)[1] <- "EmployeeID"


#Checking if any employee ID is duplicate
nrow(intime)==length(unique(intime$EmployeeID))
# No duplicate employee ID

#Checking if all files have same employee IDs
setdiff(intime$EmployeeID,outtime$EmployeeID)
setdiff(intime$EmployeeID,mgr_survey$EmployeeID)
setdiff(intime$EmployeeID,emp_survey$EmployeeID)
setdiff(intime$EmployeeID,gendata$EmployeeID)
# All files have same emp IDs


#Checking if columnnames of intime and outtime match and are in same sequence (same dates)
sum(colnames(intime)==colnames(outtime))
#262 Same as column names. Means all column names match

#Checking if there are any holidays where ALL employees have intime and outtime as NA on same date
intime_na<-names(which(sapply(intime,function(x) all(is.na(x)))==TRUE))
outtime_na<-names(which(sapply(outtime,function(x) all(is.na(x)))==TRUE))
sum(!(intime_na==outtime_na)) #Zero.
length((intime_na==outtime_na)) #12
#Means 12 columns in intime and outtime have all NA values and the column names (dates) also match

#Removing columns corresponding to holidays from intime and outtime (12 nos)
intime<-intime[!colnames(intime) %in% c(intime_na)]
outtime<-outtime[!colnames(outtime) %in% c(outtime_na)]

## Derived metric based on intime and outtime

## IF intime and outtime both are NA, treating it as leave
emp_leaves<-data.frame(intime$EmployeeID,seq(0,0,nrow(intime)))
colnames(emp_leaves)<-c("EmployeeID","leaves")
for (i in 2: ncol(intime)){
  c1<-which(is.na(intime[,i]))
  c2<-which(is.na(outtime[,i]))
  c_comp<-(c1==c2)
  if(sum(!c_comp)==0 ){
    emp_leaves[c1,2]<-emp_leaves[c1,2]+1}}
str(emp_leaves)

# COnverting date columns from character to date
intime[,2:250]<-lapply(intime[,2:250], function(x) as_datetime(x))
outtime[,2:250]<-lapply(outtime[,2:250], function(x) as_datetime(x))
str(outtime)
#Finding out workhours for each employee
work_hours<-outtime[,2:250]-intime[,2:250]
str(work_hours)
#Converting columns to numeric
work_hours[,1:249]<-lapply(work_hours[,1:249], function(x) as.numeric(x))
work_hours$avg_work_hr<-rowMeans(work_hours[,1:249],na.rm = TRUE)
work_hours$avg_work_hr<-round(work_hours$avg_work_hr,2)

# adding leaves to emp_work_hrs dataframe
emp_leaves<-cbind(emp_leaves,work_hours$avg_work_hr)
colnames(emp_leaves)[3]<-"avg_work_hr"
str(emp_leaves)

#Merging all to create master
master<-merge(emp_survey,gendata,"EmployeeID")
master<-merge(master,mgr_survey,"EmployeeID")
master<-merge(master,emp_leaves,"EmployeeID")

str(master)
summary(master)

## DATA CLEANING from master
summary(master$EmployeeCount)
summary(master$StandardHours)
summary(master$Over18)
## All values for EmployeeCount,Over18 and StandardHours are same. So removing 
master<-master[!colnames(master)%in% c("EmployeeCount","StandardHours","Over18")]

#Checking for NA in master
sapply(master, function(x) sum(is.na(x)))
# 5 variables have some NA values

length(which(is.na(master))) 
length(unique(which(is.na(master))))
# Total 111 records have NA values , 2.5 % of total. Very less.
#Removing records with NA values

master<-subset(master,!is.na(master$EnvironmentSatisfaction) & !is.na(master$JobSatisfaction) & !is.na(master$WorkLifeBalance) & !is.na(master$NumCompaniesWorked) & !is.na(master$TotalWorkingYears))


## Preliminary data analysis
summary(master$Attrition)
sum(master$Attrition=="Yes")/nrow(master)
#695 attrition count out of 4300 records, 16.16%

### Checking impact of variables on attrition using bar charts
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(master, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill"), 
          ggplot(master, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill"),
          ggplot(master, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill") + bar_theme1, align = "h") 
# low values of satisfaction and work life balance have impact on attrition

plot_grid(  ggplot(master, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill"),
            ggplot(master, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = "fill"),
            ggplot(master, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill"),
            ggplot(master, aes(x=Education,fill=Attrition))+ geom_bar(position = "fill"),
            ggplot(master, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+ bar_theme1, align = "h") 
# Frequent travel, single marital status, HR education field have impact on attrition

plot_grid(     ggplot(master, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar(position = "fill"),
               ggplot(master, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill"),
               ggplot(master, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position = "fill")+
                 bar_theme1, align = "h")  
# HR department  have contribution in attrition
plot_grid(          ggplot(master, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill"),
                    ggplot(master, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill"),
                    ggplot(master, aes(x=JobRole,fill=Attrition))+ geom_bar(position = "fill")
                    + bar_theme1, align = "v")   
# poor job involvement has impact. Job level and job role do not have


## plotting histograms for cotinuous numeric variables
plot_grid(ggplot(master, aes(x=master$DistanceFromHome,fill=Attrition))+ geom_histogram(binwidth=3, position = "fill"),
          ggplot(master, aes(x=master$MonthlyIncome,fill=Attrition))+ geom_histogram(binwidth = 29000,position = "fill"),
          ggplot(master, aes(x=master$avg_work_hr,fill=Attrition))+ geom_histogram(binwidth = 3,position = "fill"),
          ggplot(master, aes(x=master$Age,fill=Attrition))+ geom_histogram(binwidth = 5,position = "fill"),
          ggplot(master, aes(x=master$NumCompaniesWorked,fill=Attrition))+ geom_histogram(binwidth = 1,position = "fill"),
          ggplot(master, aes(x=master$PercentSalaryHike,fill=Attrition))+ geom_histogram(binwidth = 1,position = "fill"),
          ggplot(master, aes(x=master$TotalWorkingYears,fill=Attrition))+ geom_histogram(binwidth = 3,position = "fill"),
          ggplot(master, aes(x=master$YearsAtCompany,fill=Attrition))+ geom_histogram(binwidth = 1,position = "fill"),
          ggplot(master, aes(x=master$YearsSinceLastPromotion,fill=Attrition))+ geom_histogram(binwidth = 1,position = "fill"),
          ggplot(master, aes(x=master$leaves,fill=Attrition))+ geom_histogram(binwidth = 3,position = "fill")
          
          + bar_theme1, align = "h")  
#Higher Average work hour, low age have impact on attition
# Correlation between numeric variables
ggpairs(master[, c("Age","DistanceFromHome", "MonthlyIncome", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager", "avg_work_hr","leaves")])
### Strong coorelation between YearsAtCompany and YearsWithCurrManager (0.769)

### PREPARING DATA FOR MODELING

str(master)
## Converting the columns with 2 factor levels into 1 and 0
levels(master$Attrition)<-c(0,1) # Putting 1 for attrition =yes
levels(master$Gender)<-c(0,1)


#Identifying categorical columns
sapply(master,function(x) length(unique(x)))
#Checking the columns having less unique values

## Creating dummy variables where values are are more than 2
factcols<-c("PerformanceRating","JobInvolvement","TrainingTimesLastYear",
  "StockOptionLevel","NumCompaniesWorked","JobLevel","EnvironmentSatisfaction",
  "JobSatisfaction","WorkLifeBalance","Education","BusinessTravel","Department",
  "EducationField","JobRole","MaritalStatus")

master[,factcols]<-data.frame(sapply(master[,factcols], function(x) factor(x)))

# converting all numeric variables to scale
master[,2:ncol(master)]<-mutate_if(master[,2:ncol(master)],is.numeric,funs(scale(.)))


dummies<-data.frame(sapply(master[,factcols], 
                  function(x) data.frame(model.matrix(~x,data =master[,factcols]))[,-1]))

#Removing original columns from master and adding dummies
master1<-master[!names(master) %in% factcols]
master_model<-cbind(master1,dummies )
#Removing employee ID for modeling
master_model<-master_model[,-1]
 
######Analysis
# creating train and test data sets without 
set.seed(100)

indices = sample.split(master_model$Attrition, SplitRatio = 0.7)

train = master_model[indices,]

test = master_model[!(indices),]

#### generating models

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)
#AIC: 2104.8 Null deviance: 2661.4 Residual deviance: 1964.8
#Notice that there are many insignificant variables in the model
#we will use step-wise function to remove the extremely insignificant variables in the beginning itself

model_2<- stepAIC(model_1, direction="both")
summary(model_2)
#AIC: 2061.7
#The variables have been selected using the stepwise AIC algorithm
#remove insignificant variables and multicollinear ones from the model on the basis of VIF and p-value
# If the VIF is above 2 as the business goal says, we would remove the variables if they are statistically insignificant

vif(model_2)
#TotalWorkingYears and YearsAtCompany have high vif values. Removing YearsAtCompany as it has less significance than other

# Build model Excluding YearsAtCompany
model_3<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
               Department.xSales + EducationField.xLife.Sciences + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)



summary(model_3)
vif(model_3)
# Removing BusinessTravel.xTravel_Rarely as high vif and less significance
model_4<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
               Department.xSales + EducationField.xLife.Sciences + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_4)
vif(model_4)

# Department.xResearch...Development and Department.xSales have high corelation 
cor(master_model$Department.xResearch...Development,master_model$Department.xSales)
# 90% corerelation Hence removing Department.xResearch...Development with more vif

model_5<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               Department.xSales + EducationField.xLife.Sciences + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_5)
vif(model_5)

#Department.xSales has p value 0.923662, highly insignificant. Removing
model_6<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               EducationField.xLife.Sciences + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_6)
vif(model_6)
#AIC: 2090.3
#EducationField.xLife.Sciences as p value 0.146785, insignificant. Removing
model_7<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_7)
vif(model_7)
#AIC: 2090.4
# WorkLifeBalance.x3 has high vif 3.306024. Removing
model_8<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 +  
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_8)
vif(model_8)
# WorkLifeBalance.x2 is insignificant p value 0.639022 Removing
model_9<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 +   
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_9)
vif(model_9)

# WorkLifeBalance.x4 is insignificant p value 0.342541 Removing
model_10<-glm(formula = Attrition ~ Age + TotalWorkingYears + 
               YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
               JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
               TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
               NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
               NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
               JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
               JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
             data = train)
summary(model_10)
vif(model_10)

#TotalWorkingYears has high vif and less significance so removing
model_11<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + JobLevel.x3 + 
                JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_11)
vif(model_11)
## All vif good below 2, focussing only on significance
# Removing JobLevel.x3 
model_12<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x8 + NumCompaniesWorked.x9 +  
                JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_12)
vif(model_12)

# Removing NumCompaniesWorked.x8

model_13<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                 NumCompaniesWorked.x9 +  
                JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_13)
vif(model_13)


# Removing TrainingTimesLastYear.x4

model_14<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +  TrainingTimesLastYear.x5 + 
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_14)
vif(model_14)

# Removing TrainingTimesLastYear.x5

model_15<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                JobLevel.x5 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_15)
vif(model_15)
# Removing JobLevel.x5
model_16<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 + NumCompaniesWorked.x4 + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_16)
vif(model_16)
# Removing NumCompaniesWorked.x4
model_17<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xLaboratory.Technician + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_17)
vif(model_17)
# Removing JobRole.xLaboratory.Technician 
model_18<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_18)
#AIC: 2154

# Removing JobRole.xResearch.Scientist 
model_19<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director +  
                JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_19)

# Removing JobRole.xSales.Executive
model_20<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 +    BusinessTravel.xTravel_Frequently + 
                JobRole.xResearch.Director +  
                 MaritalStatus.xSingle, family = "binomial", 
              data = train)
summary(model_20)
# Removing JobRole.xResearch.Director
model_21<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
              MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_21)
#AIC: 2220.3

# Removing JobSatisfaction.x2
model_22<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  JobSatisfaction.x3 + 
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_22)
#Removing JobSatisfaction.x3
model_23<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                JobInvolvement.x3 +   
                TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_23)
# Removing JobInvolvement.x3
model_24<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                          TrainingTimesLastYear.x6 + NumCompaniesWorked.x1 +  
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_24)
# Removing NumCompaniesWorked.x1
model_25<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                TrainingTimesLastYear.x6 +   
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_25)
#AIC:  2224.2
# Removing NumCompaniesWorked.x6
model_26<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                TrainingTimesLastYear.x6 +   
                NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_26)

# Removing TrainingTimesLastYear.x6
model_27<-glm(formula = Attrition ~ Age + 
                YearsSinceLastPromotion + YearsWithCurrManager + avg_work_hr + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x7 + 
                NumCompaniesWorked.x9 +  
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 +  
                MaritalStatus.xSingle, family = "binomial", 
              data = train)

summary(model_27)
vif(model_27)
#AIC: 2239.1 Null deviance: 2661.4 Residual deviance: 2215.1
####ALL 11 vVariables in model_27 have *** meaning high significance.

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                -1.63300    0.11743 -13.907  < 2e-16 ***
#   Age                        -0.45316    0.05999  -7.554 4.22e-14 ***
#   YearsSinceLastPromotion     0.43229    0.06966   6.206 5.44e-10 ***
#   YearsWithCurrManager       -0.63381    0.07962  -7.961 1.71e-15 ***
#   avg_work_hr                 0.55034    0.05109  10.772  < 2e-16 ***
#   NumCompaniesWorked.x5       1.01181    0.23507   4.304 1.68e-05 ***
#   NumCompaniesWorked.x7       0.89489    0.21649   4.134 3.57e-05 ***
#   NumCompaniesWorked.x9       1.04483    0.25862   4.040 5.34e-05 ***
#   EnvironmentSatisfaction.x2 -0.93916    0.16165  -5.810 6.26e-09 ***
#   EnvironmentSatisfaction.x3 -1.17120    0.15070  -7.772 7.74e-15 ***
#   EnvironmentSatisfaction.x4 -1.19819    0.15102  -7.934 2.12e-15 ***
#   MaritalStatus.xSingle       1.06551    0.11099   9.600  < 2e-16 ***


##**** Conclusion from model
#Young, single employees, employees with many job changes (>5), employees with 
#longer duration with manager, delayed promotions and more working hours are prone
# to attrition

#Finalizing model_27
final_model<-model_27
###########

#MODEL EVALUATION

#PRediciton on test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])
summary(test_pred)
# We are getting probability range between 0.28% to 85% with mean as 15.86% and median as 10%
# Third qurtile as 21%


# Putting prediction in test data frame
test$prob <- test_pred

############Choosing cutoffs using trial and error method

# Taking  cutoff as 50% to predict attrition

test_pred_attrition <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))

#Putting values as "Yes" for 1 and "No" for 0 in acutal attition column from test and taking that value in other variable
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
# Creating table
table(test_actual_attrition,test_pred_attrition)
#               test_pred_attrition
# test_actual_attrition   No  Yes
#                   No  1063   18
#                   Yes  174   35

# This table has high accuracy and Specificity but very low Sensitivity 
# Hence changing cutoff based on mean values of probability 25%
test_pred_attrition <- factor(ifelse(test_pred >= 0.25, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)
# Creating confusion matrix
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
# Accuracy 0.7946, Sensitivity 0.46890, Specificity : 0.85754
#Changing value to 0.2
test_pred_attrition <- factor(ifelse(test_pred >= 0.20, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)
# Creating confusion matrix
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Accuracy : 0.7519  Sensitivity : 0.55024 Specificity : 0.79093

# Changing value to 0.18 to increase sensitivity
test_pred_attrition <- factor(ifelse(test_pred >= 0.15, "Yes", "No"))
table(test_actual_attrition,test_pred_attrition)
# Creating confusion matrix
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
#Accuracy : 0.6884, Sensitivity : 0.7081 Specificity : 0.6846

####### NOW we will use R code to choose right cut off 
### MODEL evaluation method used - Accuracy, sensitivity and specificity

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix( predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(test_pred)
#We are getting probability range between 0.28% to 85%
s = seq(.0028,.85,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.025)]
# Diff between "sensitivity", "specificity" is chosen is 0.025 for selecting cutoff
cutoff
#0.1568364

test_cutoff_attrition <- factor(ifelse(test_pred >= 0.1568364, "Yes", "No"))
#Taking final confusionMatrix
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final 

acc <- conf_final$overall[1]
#ACCURACY 0.6977   69.77%

sens <- conf_final$byClass[1]
#sensitivity 0.6794   67.94%

spec <- conf_final$byClass[2]
# Specificity 0.7012 70.12%

## Good model to use