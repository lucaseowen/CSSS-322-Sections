#### PREAMBLE ####
library(dplyr)
library(ggplot2)
library(readr)

#### SET PLOT THEME ####

my_theme <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
theme_set(my_theme)


#### LOAD DATA ####

df <- read_csv(unz("data/322/census/adult.csv.zip", "adult.csv"), na=c("?"))


#### CLEAN AND CREATE VARIABLES ####

df$income_binary <- NA_real_
df$income_binary[df$income=="<=50K"] <- 0
df$income_binary[df$income==">50K"] <- 1

#set reference categories
colnames(df)
sort(unique(df$workclass))
sort(unique(df$education))
sort(unique(df$marital.status))
sort(unique(df$occupation))
sort(unique(df$relationship))
sort(unique(df$race))
sort(unique(df$sex))
sort(unique(df$native.country))

# Collapse workclass
df$workclass <- ifelse(df$workclass %in% c("Self-emp-inc", "Self-emp-not-inc"), "Self-emp", df$workclass)
df$workclass <- ifelse(df$workclass %in% c("Federal-gov", "State-gov", "Local-gov"), "Government", df$workclass)
df$workclass <- factor(df$workclass, levels = c("Private", setdiff(unique(df$workclass), "Private")))

# Set HS-grad as reference for education
df$education <- factor(df$education, levels = c("HS-grad", setdiff(unique(df$education), "HS-grad")))

# Collapse married categories in marital.status
df$marital.status <- ifelse(df$marital.status %in% c("Married-civ-spouse", "Married-AF-spouse"), "Married", df$marital.status)
df$marital.status <- factor(df$marital.status, levels = c("Never-married", setdiff(unique(df$marital.status), "Never-married")))

# Set Sales as reference for occupation
df$occupation <- factor(df$occupation, levels = c("Sales", setdiff(unique(df$occupation), "Sales")))

# Set Unmarried as reference for relationship
df$relationship <- factor(df$relationship, levels = c("Unmarried", setdiff(unique(df$relationship), "Unmarried")))

# Set White as reference for race
df$race <- factor(df$race, levels = c("White", setdiff(unique(df$race), "White")))

# Set United-States as reference for native.country
df$native.country <- factor(df$native.country, levels = c("United-States", setdiff(unique(df$native.country), "United-States")))



#### EXPLORATION ####

#age v. income plot

ggplot(data = df, aes(x = age, y = income_binary)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm")
  ylab("Income (Less than or Greater than 50,000 USD)") +
  xlab("Age")

#skipping the typical geom_smooth() because I want to show something...

# w/ fitted regression line
# Step 1: Fit a the model
model <- lm(income_binary ~ age, data = df)
# Step 2: Create a prediction grid across the full x range
new_data <- data.frame(age = seq(0, 95, by = 1))
preds <- predict(model, newdata = new_data, interval = "confidence")
new_data <- cbind(new_data, preds)

# Step 3: Plot
ggplot(data = df, aes(x = age, y = income_binary)) +
  geom_point(alpha = 0.2) +
  scale_x_continuous(limits = c(0, 95), breaks=seq(from=0, to=90, by=10)) +
  geom_line(data = new_data, aes(x = age, y = fit), color = "black", inherit.aes = FALSE) +
  geom_ribbon(data = new_data, aes(x = age, ymin = lwr, ymax = upr), alpha = 0.2, inherit.aes = FALSE) +
  ylab("Income (Less than or Greater than 50,000 USD)") +
  xlab("Age")

#how should we interpret this?
#notice anything strange?

#### LINEAR REGRESSION ####

#what variables should we include?
lm1 <- lm(income_binary ~ age + workclass + education + marital.status + occupation + race + sex + hours.per.week + native.country, data=df)
summary(lm1)

lm2 <- lm(income_binary ~ sex, data=df)
summary(lm2)

#what can we consider to be causal effects?

#how should we interpret our results?
#what does a coefficient mean in this case?

#### HYPOTHETICALS ####

# Updated hypothetical individuals
hypothetical <- data.frame(
  age = c(60, 25),
  workclass = factor(c("Private", "Private"), levels = levels(df$workclass)),
  fnlwgt = c(mean(df$fnlwgt, na.rm = TRUE), mean(df$fnlwgt, na.rm = TRUE)),
  education = factor(c("Doctorate", "HS-grad"), levels = levels(df$education)),
  education.num = c(16, 9),
  marital.status = factor(c("Married", "Never-married"), levels = levels(df$marital.status)),
  occupation = factor(c("Exec-managerial", "Other-service"), levels = levels(df$occupation)),
  relationship = factor(c("Husband", "Own-child"), levels = levels(df$relationship)),
  race = factor(c("Black", "White"), levels = levels(df$race)),
  sex = c("Male", "Female"),
  capital.gain = c(0, 0),
  capital.loss = c(0, 0),
  hours.per.week = c(40, 40),
  native.country = factor(c("United-States", "United-States"), levels = levels(df$native.country))
)

#predictions w/ confidence intervals - some hypothetical cases
preds <- predict(lm1, newdata = hypothetical, interval = "prediction", type="response")
hypothetical_preds <- cbind(hypothetical, preds)

# Add labels for plotting
hypothetical_preds$label <- c("Black Male Executive", "White Female HS-grad")

ggplot(hypothetical_preds, aes(x = label, y = fit)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.4, color = "black") +
  ylim(-0.1, 1) +
  ylab("Predicted Probability of Income > 50k") +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

#how should we interpret the predicted outcome here?

#potential concerns about this model?


### HOW WELL DOES lm1 FIT THE DATA?
#does it make accurate predictions?
model_data <- model.frame(lm1)
model_data$preds <- predict(lm1, newdata = model_data, type = "response")
# Create predicted class variable based on 50% threshold
model_data$predicted_value <- ifelse(model_data$preds >= 0.5, 1, 0)
# Calculate accuracy
accuracy <- mean(model_data$predicted_value == model_data$income_binary)
# Print accuracy as a percentage
accuracy_percent <- round(accuracy * 100, 2)
cat("Prediction accuracy:", accuracy_percent, "%\n")


