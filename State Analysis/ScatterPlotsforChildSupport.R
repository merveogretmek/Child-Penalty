library(haven)

pequiv <- read_dta("~/Desktop/Summer Semester/Seminar on Inequality and Redistribution/Datatables/pequiv.dta")

data <- data.frame(pequiv$chspt, pequiv$chsub, pequiv$l11101)
colnames(data) <- c("ch_allowance", "chcare_sub", "state")

data <- subset(data, ch_allowance > 0)
data <- subset(data, chcare_sub > 0)
data <- subset(data, state > 0)

states <- data %>%
  group_by(state) %>%
  summarize(ch_allowance_mean = mean(ch_allowance),
            chcare_sub_mean = mean(chcare_sub)) 

states$child_penalty <- c(0.609, 0.185, 0.130, 0.312, 0.102, -0.078, 0.154, 0.226, 0.248, 0.502, -0.279, 0.011, 0.042, 0.039, 0.069, 0.155)

# Create scatterplot with correlation label

library(glue)

correlation1 <- cor(states$ch_allowance_mean, states$child_penalty)

ggplot(data = states, aes(x = ch_allowance_mean, y = child_penalty)) +
  geom_point() +
  geom_text(aes(x= max(ch_allowance_mean) + 0.02, y = max(child_penalty) + 0.05, label = glue("Correlation: {correlation1}")), hjust =1, vjust =1, size=3)

correlation2 <- cor(states$chcare_sub_mean, states$child_penalty)

ggplot(data = states, aes(x = chcare_sub_mean, y = child_penalty)) +
  geom_point() +
  geom_text(aes(x= max(chcare_sub_mean) + 0.02, y = max(child_penalty) + 0.05, label = glue("Correlation: {correlation2}")), hjust =1, vjust =1, size=3)
