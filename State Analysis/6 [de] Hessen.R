## 6 [de] Hessen  

## LIBRARIES

library(haven)

## DATA

# Importing data

biobirth <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/biobirth.dta")
hbrutto <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/hbrutto.dta")
pgen <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/pgen.dta")
pequiv <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/pequiv.dta")

# Selecting relevant variables
empdata <- data.frame(pgen$pid, pgen$hid ,pgen$syear, pgen$pglabgro, pgen$pgemplst)
birthdata <- data.frame(biobirth$pid, biobirth$sex, biobirth$kidgeb01, biobirth$gebjahr)
statedata <- data.frame(hbrutto$hid, hbrutto$bula_v2)
infdata <- data.frame(pequiv$pid, pequiv$syear, pequiv$y11101)

# Naming columns
colnames(empdata) <- c("persnr", "hid","year", "earnings", "empstatus")
colnames(birthdata) <- c("persnr", "sex", "kidgeb1", "birthyear")
colnames(infdata) <- c("persnr", "year", "y11101")
colnames(statedata) <- c("hid", "state")

# Removing incomes equal to -2 (does not apply) and -5 (not included in survey)
empdata <- subset(empdata, earnings != -2 & earnings != -5)

# Removing empstatus equal to -1 (no answer), -2 (does not apply), -5 (not included in survey)
empdata <- subset(empdata, empstatus != -1 & empstatus != -2 & empstatus  != -5)

# Removing 1stbirth equal to -1 (no answer), -2 (does not apply), -3 (answer improbable)
birthdata <- subset(birthdata, kidgeb1 != -1 & kidgeb1 != -2 & kidgeb1 != -3)

# Removing sex equal to -1 (no answer), -3 (answer improbable)
birthdata <- subset(birthdata, sex != -1  & sex != -3)

# Removing years equal to -1 (no answer)
birthdata <- subset(birthdata, birthyear != -1)

# Merging datasets
data <- merge(empdata, birthdata, by = 'persnr')
data <- merge(data, infdata, by = c("persnr","year"))
data <- merge(data, statedata, by = "hid")

# Creating age variable
data$age <- data$year - data$birthyear

# Age at first birth
data$age_firstbirth <- data$kidgeb1 - data$birthyear

# Check min and max
min(data$age_firstbirth) #-41
max(data$age_firstbirth) #61

# Take the ages between 20 and 45 at first birth
data <- subset(data, age_firstbirth >= 20 & age_firstbirth <= 45)

# Child should be at least 10 years old now
data <- subset(data, kidgeb1 <= 2008)

# We need to see 5 years before birth (data starts from 1984)
data <- subset(data, kidgeb1 >= 1990)

# Create event_time
data$event_time <- data$year - data$kidgeb1

# Take 5 years before and 10 years after
data <- subset(data, event_time >= -5 & event_time <= 10)

# 1 [de] Schleswig-Holstein 
data <- subset(data, state == 6)


# Dummy variables

# Load dplyr package for data manipulation
library(dplyr)

# Assuming your data is stored in a data.frame called 'data'

# 2. Sort the data
data <- data %>%
  arrange(persnr, year)

# 3. Generate variable 'I'
data <- data %>%
  group_by(persnr) %>%
  mutate(I = n())

# 4. Generate variable 'i'
data <- data %>%
  group_by(persnr) %>%
  mutate(i = row_number())

# Load dplyr package for data manipulation
library(dplyr)

# Assuming your data is stored in a data.frame called 'data'
# Create 'bef' and 'aft' variables
data <- data %>%
  mutate(bef = ifelse(event_time <= 0, 1, 0),
         aft = ifelse(event_time > 0, 1, 0))

# Calculate 'bef_sum' and 'aft_sum' within each 'persnr' group
data <- data %>%
  group_by(persnr) %>%
  mutate(bef_sum = sum(bef),
         aft_sum = sum(aft)) %>%
  ungroup()

# Keep observations where both 'bef_sum' and 'aft_sum' are greater than 0
data <- data %>%
  filter(bef_sum > 0 & aft_sum > 0)

# Keep observations where 'I' is greater than or equal to 16
data <- data %>%
  filter(I >= 16)

# Calculate the sum of 'kidgeb1'
kidgeb1_sum <- sum(data$kidgeb1, na.rm = TRUE)

# Calculate the number of distinct age groups
n_age <- data %>%
  distinct(age) %>%
  nrow()


# Calculate the number of distinct year groups
n_year <- data %>%
  distinct(year) %>%
  nrow()



# If you need to access the values later in your R script, you can store them as variables:
# kidgeb1_sum, n_age, and n_year

## Women

# Load necessary packages
library(dplyr)
library(broom)

# Assuming your data is stored in a data.frame called 'data'
# Create dummy variables for 'age', 'syear', and 'eventtime'
data <- data %>%
  mutate(dage = factor(age),
         dyear = factor(year),
         det = factor(event_time)) %>%
  mutate(dummy_dage = model.matrix(~dage - 1),
         dummy_dyear = model.matrix(~dyear - 1),
         dummy_det = model.matrix(~det - 1))

# Filter the data to only include observations where 'sex == 2'

data_sex1 <- data %>%
  filter(sex == 1)
data_sex2 <- data %>%
  filter(sex == 2)


# Run the linear regression without a constant term
regression_1 <- lm(earnings ~ dummy_det + dummy_dage + dummy_dyear - 1, data = data_sex1)
regression_2 <- lm(earnings ~ dummy_det + dummy_dage + dummy_dyear - 1, data = data_sex2)
# If you want to obtain a tidy output of the regression results, you can use the broom package
tidy_regression1 <- broom::tidy(regression_1)
tidy_regression2 <- broom::tidy(regression_2)

library(stringr)

## MEN


model_coef_m <- tidy_regression1 %>%
  filter(str_detect(term, "dummy_detdet"))
model_coef_m

model_coef_m$term <- str_replace(model_coef_m$term, "dummy_detdet", "")
model_coef_m

model_coef_m$sex = "m"

base_m <- model_coef_m[model_coef_m$term == -1,]
model_coef_m$dy <- (model_coef_m$estimate - base_m$estimate)/base_m$estimate



## WOMEN




model_coef_w <- tidy_regression2 %>%
  filter(str_detect(term, "dummy_detdet"))
model_coef_w

model_coef_w$term <- str_replace(model_coef_w$term, "dummy_detdet", "")
model_coef_w

model_coef_w$sex = "w"

base_w <- model_coef_w[model_coef_w$term == -1,]
model_coef_w$dy <- (model_coef_w$estimate - base_w$estimate)/base_w$estimate

coef <- rbind(model_coef_m, model_coef_w)

coef$term <- as.integer(coef$term)

## CHILD PENALTY CALCULATION

men <- model_coef_m[,c("term","dy")]

men <- men %>%
  rename(dy_men = dy)

women <- model_coef_w[,c("term","dy")]

women <- women %>%
  rename(dy_women = dy)

penaltydf <- merge(men, women, by="term")
mean_men <- mean(penaltydf$dy_men)
mean_women <- mean(penaltydf$dy_women)

# Calculate the difference between 'men' and 'women'
penalty <- mean_men - mean_women
penalty_formatted <- sprintf("%.3f", penalty)
penalty_formatted

# Child penalty = 0.082

## GRAPH

library(glue)



ggplot(coef, aes(x = term, y = dy, shape = sex)) +
  geom_point(aes(x = term, y = dy), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  geom_line(aes(linetype=sex)) +
  scale_x_continuous("Event Time", breaks = coef$term) +
  labs(x = "Event Time", y = "Relative Change in Income (Base = -1)", title = "Event Study Plot (Southern Germany)") +
  geom_text(aes(x= max(term), y = max(dy) + 0.02, label = glue("Child Penalty: {penalty_formatted}")), hjust =1, vjust =1, size=3)

# Child Penalty = -0.078

