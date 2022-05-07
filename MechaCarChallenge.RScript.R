# Read the csv file
mecha_data <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)
# Perform  linear regression module 
mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data)
# Determine the p-value and r-squared value
summary(mecha_lm)
##suspension coil dataset
# Read the csv file
suspension_data <- read.csv("Suspension_Coil.csv",stringsAsFactors = F, check.names = F)
# Total Summary
library(dplyr);total_summary <- suspension_data %>% 
  +     summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI))
# Summary By Lot
# Create the Summary By Lot
library(dplyr);lot_summary <- suspension_data %>% group_by(Manufacturing_Lot) %>% 
  summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI), .groups='keep')
##Suspension Coil Tests

# Peform t-test
t.test(suspension_data$PSI,mu = 1500)
# Lot 1 t-test
t.test(subset(suspension_data,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# Lot 2 t-test
t.test(subset(suspension_data,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

# Lot 3 t-test
t.test(subset(suspension_data,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
