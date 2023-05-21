# GERMANY

## Libraries

library(haven)
library(dplyr)
library(broom)
library(stringr)


## Importing data
biobirth <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/biobirth.dta")
hbrutto <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/hbrutto.dta")
pgen <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/pgen.dta")
pequiv <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/pequiv.dta")


## Select variables
empdata <- data.frame(pgen$pid, pgen$hid ,pgen$syear, pgen$pglabgro, pgen$pgemplst)
birthdata <- data.frame(biobirth$pid, biobirth$sex, biobirth$kidgeb01, biobirth$gebjahr)
statedata <- data.frame(hbrutto$hid, hbrutto$bula_v2)
infdata <- data.frame(pequiv$pid, pequiv$syear, pequiv$y11101)


## Naming columns
colnames(empdata) <- c("persnr", "hid","year", "earnings", "empstatus")
colnames(birthdata) <- c("persnr", "sex", "kidgeb1", "birthyear")
colnames(infdata) <- c("persnr", "year", "y11101")
colnames(statedata) <- c("hid", "state")


## Data cleaning
empdata <- subset(empdata, earnings != -2 & earnings != -5)
empdata <- subset(empdata, empstatus != -1 & empstatus != -2 & empstatus  != -5)
birthdata <- subset(birthdata, kidgeb1 != -1 & kidgeb1 != -2 & kidgeb1 != -3)
birthdata <- subset(birthdata, sex != -1  & sex != -3)
birthdata <- subset(birthdata, birthyear != -1)


## Merging datasets
data <- merge(empdata, birthdata, by = 'persnr')
data <- merge(data, infdata, by = c("persnr","year"))
data <- merge(data, statedata, by = "hid")


## Creating new variables
data$age <- data$year - data$birthyear
data$age_firstbirth <- data$kidgeb1 - data$birthyear


## Subsetting data
data <- subset(data, age_firstbirth >= 20 & age_firstbirth <= 45)
data <- subset(data, kidgeb1 <= 2008)
data <- subset(data, kidgeb1 >= 1990)
data$event_time <- data$year - data$kidgeb1
data <- subset(data, event_time >= -5 & event_time <= 10)


## Estimation

regression_m <- lm(earnings ~ factor(event_time) + factor(age) + factor(year) - 1, data = data[data$sex == 1, ])
regression_w <- lm(earnings ~ factor(event_time) + factor(age) + factor(year) - 1, data = data[data$sex == 2, ])

tidy_regression_m <- tidy(regression_m)
tidy_regression_w <- tidy(regression_w)

### Coefficients

write.csv(tidy_regression_m, "coefficients_m.csv")
write.csv(tidy_regression_w, "coefficients_w.csv")





