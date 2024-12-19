setwd("sta_215")

#installing packages necessary for further steps

install.packages("ggplot2")

install.packages("readr")
library("readr")

library(ggplot2)
#loading data into environment

raw_data <- read.csv("raw_data.csv")

#table 1: Descriptive Values
summary(raw_data$recurring)

sd(raw_data$recurring )

table(raw_data$scream)

summary(raw_data$writers)

sd(raw_data$writers)

table(raw_data$plot)
#table 2: Contingency Table

table(raw_data$tone, raw_data$plot)
chisq.test(raw_data$tone, raw_data$plot)

#Figure 1: Box Plot
ggplot(raw_data, aes(x = as.factor(scream), y = `recurring`)) +
  geom_boxplot() +
  labs(title = "Box Plot of scream and recurring",
       x = "scream",
       y = "recurring") +
  theme_minimal()
#anova testing
anova <- aov(raw_data$`scream` ~ as.factor(raw_data$recurring ))
summary(anova)
#Figure 2: Scatter Plot
plot(raw_data$date, raw_data$`writers`)


# add x line and y line for means

meanx <- mean(raw_data$date)

meany <- mean(raw_data$`writers`)

abline(v = meanx, col = "black")

abline(h = meany, col = "black")

linear_relationship <- lm(raw_data$writers ~ raw_data$`date`)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
abline(linear_relationship, col = "red")
#Figure 3: Residuals

# Plot the residuals
plot(raw_data$`date`, residuals(linear_relationship))