cancer<-read.csv(file.choose())
head(cancer)
nrow(cancer)
ncol(cancer)
dim(cancer)
is(cancer)
na.omit(cancer)
class(cancer[,7])
class(cancer[,8])
class(cancer[,9])
str(cancer)
table(cancer[,8])
table(cancer[,9])
summary(cancer)
sd(cancer[,8])
sd(cancer[,9])
library(dplyr)
apply(cancer[,1:9],2,mean)
apply(cancer[,1:9],2,median)
apply(cancer[,1:9],2,mode)
library(moments)
apply(cancer[,1:9],2,kurtosis)
apply(cancer[,1:9],2,skewness)
names(cancer)
quantile(cancer[,2])
str(cancer)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggplot2)

# Convert CancerHistory to a factor
cancer$CancerHistory <- factor(cancer$CancerHistory)

# Create a bar plot
ggplot(cancer, aes(x = Age, fill = CancerHistory)) +
  geom_bar() +
  labs(y = "Cancer History", x = "Age", title = "Cancer History of Respective Ages") +
  theme_bw()


library(ggplot2)

# Convert the "Diagnosis" variable to a factor
cancer$Diagnosis <- factor(cancer$Diagnosis)

# Create the bar plot
ggplot(cancer, aes(x = Gender, fill = Diagnosis)) +
  geom_bar() +
  labs(y = "Diagnosis", x = "Gender", title = "Diagnosis at Given Gender") +
  theme_bw() +
  facet_wrap(~Gender)


ggplot(cancer, aes(x = Gender, fill = Diagnosis)) +
  geom_bar(stat = "count", color = "pink") +
  labs(y = "Diagnosis", x = "Gender", title = "Diagnosis at given gender",
       subtitle = "...") +
  facet_grid(Gender~Diagnosis, scale = "free")

cancer$Smoking <- factor(cancer$Smoking)

ggplot(cancer,aes(x=Smoking,fill=Gender))+theme_bw()+geom_bar()+
labs(y="Gender",x="Smoking level",title="smoking levels in respective gender ")+
facet_grid(~Gender)


cancer$Diagnosis<-as.numeric(cancer$Diagnosis)
cancer$GeneticRisk<-as.numeric(cancer$GeneticRisk)
cancer$Smoking <- as.numeric(cancer$Smoking)
#create the histogram
ggplot(cancer, aes(x=Diagnosis, fill=Smoking)) +
  geom_bar(stat="count", position="stack", colour="pink") +
  theme_bw() +
  labs(y="Count", x="Diagnosis levels", title="Cancer Diagnosis by Smoking") +
  facet_wrap(Smoking ~ Gender, scale="free")

#Boxplot

ggplot(data = cancer, aes(x = Age, y = Smoking, group = Gender)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 1, colour = "black") +
  geom_jitter(aes(colour = Gender)) +
  labs(title = "Smoking levels with respective gender") +
  xlab("Smoking") +
  ylab("Gender") +
  theme_light() +
  facet_wrap(~Age)

ggplot(data=cancer,aes(x=BMI,y=Smoking))+geom_boxplot(alpha=0.7)+geom_point(size=1,colour="black")+geom_jitter(aes(colour=BMI))+
labs(title="Smoking levels with respective gender")+xlab("Smoking")+ylab("BMI")+theme_light()+facet_wrap(~BMI)


#boxplot facet_wrap()

library(ggplot2)
ggplot(cancer,aes(x=Gender,y=Smoking))+
geom_boxplot(alpha=0.7)+
geom_point(size=1,col="black")+
geom_jitter(aes(fill="Smoking"))+
labs(title="Smoking level in respective gender",
subtitle="distribution of smoking in respective gender")+
xlab("Gender")+
ylab("Smoking")+
theme_light()+
facet_wrap(~Smoking)

library(ggplot2)

# Assuming your dataset is called 'cancer'
ggplot(cancer, aes(x = Gender, y = BMI)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "BMI Distribution by Gender",
    x = "Gender",
    y = "BMI"
  ) +
  theme_minimal() +
  facet_wrap(~ Smoking)  # Specify the grouping variable for facets




library(psych)
psych::describe(cancer)
str(cancer)

library(ggplot2)

# Assuming your dataset is named "cancer"
# Replace "cancer" with your actual dataset name
ggplot(cancer, aes(x = Smoking)) +
  geom_bar(fill = "blue") +
  labs(
    x = "Smoking",
    y = "Count",
    title = "Distribution of Smoking Categories"
  ) +
  theme_minimal()
library(ggplot2)

# Assuming your dataset is named "cancer"
# Replace "cancer" with your actual dataset name
ggplot(cancer, aes(x =Age)) +
  geom_bar(fill = "blue") +
  labs(
    x = "BMI",
    y = "Count",
    title = "Distribution of GeneticRisk Categories"
  ) +
  theme_minimal()

ggplot(cancer, aes(x = AlcoholIntake)) +
  geom_histogram(aes(y = ..density..), binwidth = 300, fill = "blue", color = "black") +
  geom_density(alpha = 10, fill = "#FF6666") +
  labs(x = "Alcohol Intake", y = "Density")



cancer$CancerHistory<-as.numeric(cancer$CancerHistory)
ggplot(cancer,aes(BMI))+
geom_histogram(aes(y=..density..),bins=30,
fill="pink")+
geom_density()

library(reshape)
meltdata<-melt(cancer)
P<-ggplot(meltdata,aes(factor(variable),value))
P+geom_boxplot()+
facet_wrap(~variable,scale="free")

library(corrgram)
corrgram(cancer,order=TRUE)
library(corrplot)
head(cancer)
numeric_cancer<-cancer[sapply(cancer,is.numeric)]
cor_matrix<-cor(numeric_cancer)
corrplot(cor_matrix)
corrplot(cor_matrix,method='ellipse')
corrplot(cor_matrix,method='number')
corrplot(cor_matrix,method='square')
corrplot(cor_matrix,method='pie')
corrplot(cor_matrix,method='ellipse',order='AOE')
corrplot(cor_matrix,method='square',order='AOE')
corrplot(cor_matrix,method='number',order='AOE')
corrplot(cor_matrix,method='pie',order='AOE')
corrplot.mixed(cor_matrix)
corrplot.mixed(cor_matrix,order='AOE')
library(caret)
cancer$PhysicalActivity <- as.numeric(cancer$PhysicalActivity)

index<-createDataPartition(cancer$AlcoholIntake,p=0.70,list=FALSE)
str(cancer)
train<-cancer[index,]
str(train)
test<-cancer[-index,]
str(test)

#build a model
fit<-lm(AlcoholIntake~.,data=train)
print(fit)
coefficients(fit)
summary(fit)
predictions <- predict(fit)
train$predicted <- predictions
print(train)
str(train)
train$PhysicalActivity<- as.numeric(as.character(train$PhysicalActivity))
train$predicted <- as.numeric(as.character(train$predicted))
train$error <- train$AlcoholIntake - train$predicted
str(train)
train$sq_error<-(train$error)^2
str(train)
a<-mean(train$sq_error)
a
rmse<-sqrt(a)
rmse
mae<-(sum(abs(train$error)))/length(train)
mae

library(Metrics)
rmse(actual=train$AlcoholIntake,predicted=fit$fitted.values)

test$predicted<-predict(fit,test)
print(test)
str(test)
test$error<-test$AlcoholIntake-test$predicted
str(test)
test$sq_error<-(test$error)^2
str(test)
b<-mean(test$sq_error)
b
rmse1<-sqrt(b)
rmse1
mae<-(sum(abs(test$error)))/length(test)
mae

library(Metrics)
rmse(actual=test$AlcoholIntake,predicted=fit$fitted.values)
rss<-sum(test$AlcoholIntake-test$predicted)^2
rss
tss<-sum(test$AlcoholIntake-mean(test$AlcoholIntake))^2
tss
rsq<-1-(rss/tss)
rsq

# Convert PhysicalActivity to numeric
train$PhysicalActivity <- as.numeric(as.character(train$PhysicalActivity))
test$PhysicalActivity <- as.numeric(as.character(test$PhysicalActivity))

# Load necessary libraries
library(caret)
library(glmnet)

# Prepare data for Ridge regression
train_matrix <- model.matrix(AlcoholIntake ~ .^2, data=train)[,-1]
test_matrix <- model.matrix(AlcoholIntake ~ .^2, data=test)[,-1]

# Fit Ridge regression model
fit <- cv.glmnet(train_matrix, train$AlcoholIntake, alpha=0)
print(fit)
best_lambda <- fit$lambda.min
best_lambda

summary(fit)
# Get the coefficients
coef(fit, s=best_lambda)

# Predictions on training data
train$predicted <- predict(fit, s=best_lambda, newx=train_matrix)
train$predicted
train$error <- train$AlcoholIntake - train$predicted
train$error
train$sq_error <- (train$error)^2
train$sq_error

# Calculate RMSE and MAE for training data
rmse_train <- sqrt(mean(train$sq_error))
rmse_train
mae_train <- mean(abs(train$error))
mae_train

# Predictions on test data
test$predicted <- predict(fit, s=best_lambda, newx=test_matrix)
test$predicted
test$error <- test$AlcoholIntake - test$predicted
test$error
test$sq_error <- (test$error)^2
test$sq_error

# Calculate RMSE and MAE for test data
rmse_test <- sqrt(mean(test$sq_error))
rmse_test
mae_test <- mean(abs(test$error))
mae_test

# Calculate R-squared
rss <- sum(test$sq_error)
tss
tss <- sum((test$AlcoholIntake - mean(test$AlcoholIntake))^2)
tss
rsq <- 1 - (rss/tss)
rsq

library(dplyr)
selected_columns <- cancer%>%
  select( Gender, Age, Diagnosis)
head(selected_columns)

library(dplyr)
GeneticRisk_summary <- cancer%>%
  group_by(GeneticRisk) %>%
  summarise(Total_Diagnosis = sum(Diagnosis), Average_AlcoholIntake = mean(AlcoholIntake))
GeneticRisk_summary

