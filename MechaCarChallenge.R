library(tidyverse)

table <- read.csv(file='MechaCar_mpg.csv', check.names=F,stringsAsFactors=F)
table <- table %>% rename(vehicle_length = `vehicle length`,
                 vehicle_weight = `vehicle weight`,
                 spoiler_angle = `spoiler angle`,
                 ground_clearance = `ground clearance`) # Rename columns

cor_matrix <- as.matrix(table[,c("vehicle_length","vehicle_weight","spoiler_angle","ground clearance","AWD","mpg")]) #Generate a matrix for correlation
cor(cor_matrix) # Correlation matrix of variables

summary(lm(mpg ~ vehicle_length,table)) # Linear regression (mpg and vehicle length)
summary(lm(mpg ~ ground_clearance,table)) # Linear regression (mpg and ground clearance)
summary(lm(mpg ~ vehicle_length + ground_clearance,table)) # Multiple linear regression (mpg and vehicle length+ground clearance)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=table)) ## Multiple linear regression with all variables

table_2 <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors=F)
summary_table <- table_2 %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))

t.test(table_2$PSI, mu=1500) # One-sample t-test 
