#### PREAMBLE ####
library(dplyr)
library(ggplot2)
library(readr)
library(stargazer)

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

df$race[df$race=="Amer-Indian-Eskimo"] <- "Native American and Alaska Native"
# Set White as reference for race
df$race <- factor(df$race, levels = c("White", "Black", "Asian-Pac-Islander", "Other", "Native American and Alaska Native"))

# Set United-States as reference for native.country
df$native.country <- factor(df$native.country, levels = c("United-States", setdiff(unique(df$native.country), "United-States")))

#set female as reference for sex
df$sex <- factor(df$sex, levels = c("Male", "Female"))



#### LINEAR REGRESSION w/ INTERACTION TERMS ####

lm1 <- lm(income_binary ~ sex, data=df)
lm2 <- lm(income_binary ~ sex + race, data=df)
lm3 <- lm(income_binary ~ sex*race, data=df)

stargazer(lm1, lm2, lm3, type="text")

#how should we interpret our results?
#what does the coefficient for sexFemale:raceBlack mean in this case?

#### HYPOTHETICALS ####

# Updated hypothetical individuals
hypothetical <- data.frame(
  race = factor(c("White", "Black", "Asian-Pac-Islander", "Other", "Native American and Alaska Native",
                  "White", "Black", "Asian-Pac-Islander", "Other", "Native American and Alaska Native"),
                levels = levels(df$race)),
  sex = factor(c("Male", "Male", "Male", "Male", "Male",
          "Female", "Female", "Female", "Female", "Female"),
          levels = levels(df$sex))
)

#predictions w/ confidence intervals - some hypothetical cases
#defaults to 95%
#since we care about causal effects and conditional means, we use confidence interval
#if prediction was our goal, we would use interval = "prediction"
#Don't use prediction intervals unless you're making statements about what individuals might experience!
preds <- predict(lm3, newdata = hypothetical, interval = "confidence")
hypothetical_preds <- cbind(hypothetical, preds)

# Add labels for plotting
hypothetical_preds$label <- paste(hypothetical_preds$race, hypothetical_preds$sex, sep=" ")
hypothetical_preds$label <- factor(hypothetical_preds$label, levels = hypothetical_preds$label)

ggplot(hypothetical_preds, aes(x = label, y = fit)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.4, color = "black") +
  ylim(-0.1, 1) +
  ylab("Predicted Probability of Income > 50k") +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

#how should we interpret the predicted outcome here?

#### LOGIT REGRESSION ####

logit1 <- glm(income_binary ~ age, data = df, family = "binomial")
logit2 <- glm(income_binary ~ age + sex, data = df, family = "binomial")
logit3 <- glm(income_binary ~ age * sex, data = df, family = "binomial")

stargazer(lm1, lm2, lm3, type="text")

#we have to be very careful interpreting these coefficients!
exp(logit3$coefficients[3])
# Holding other variables constant, the odds of earning more than $50k for females are about 53% of the odds for males

#much easier to just calculate hypotheticals

#### HYPOTHETICALS ####

# Updated hypothetical individuals
hypothetical <- expand.grid(
  age = min(df$age, na.rm=T):max(df$age, na.rm=T),
  sex = c("Male", "Female")
)

#predictions w/ confidence intervals - some hypothetical cases
preds <- predict(logit3, newdata = hypothetical, type="response") #base R can't do confidence intervals for this! Requires simulation which is complicated
hypothetical_preds <- cbind(hypothetical, preds)

ggplot(hypothetical_preds, aes(x = age, y = preds, group=sex)) +
  # geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.4, color = "black") +
  geom_line(aes(color=sex, linetype=sex))+
  ylim(-0.1, 1) +
  ylab("Predicted Probability of Income > 50k") +
  xlab("Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))+
  labs(color = "Sex", linetype="Sex")

#### COMPARING TO WHAT WE DID LAST WEEK ####

### EXPANDING THE REGRESSION TO AGE ZERO
# Updated hypothetical individuals
hypothetical <- expand.grid(
  age = 0:max(df$age, na.rm=T),
  sex = c("Male", "Female")
)
#predictions w/ confidence intervals - some hypothetical cases
preds <- predict(logit3, newdata = hypothetical, type="response") #base R can't do confidence intervals for this! Requires simulation which is complicated
hypothetical_preds <- cbind(hypothetical, preds)
ggplot(hypothetical_preds, aes(x = age, y = preds, group=sex)) +
  # geom_pointrange(aes(ymin = lwr, ymax = upr), size = 0.4, color = "black") +
  geom_line(aes(color=sex, linetype=sex))+
  ylim(-0.1, 1) +
  ylab("Predicted Probability of Income > 50k") +
  xlab("Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))+
  labs(color = "Sex", linetype="Sex")


### PREDICTING FOR TWO HYPOTHETICAL INDIVIDUALS W/ ALL COVARIATES
logit4 <- glm(income_binary ~ age + workclass + education + marital.status + occupation + race + sex + hours.per.week + native.country, data=df, family = "binomial")
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
preds <- predict(logit4, newdata = hypothetical, type="response")
hypothetical_preds <- cbind(hypothetical, preds)
# Add labels for plotting
hypothetical_preds$label <- c("Black Male Executive", "White Female HS-grad")
ggplot(hypothetical_preds, aes(x = label, y = preds)) +
  geom_point()+
  ylim(-0.1, 1) +
  ylab("Predicted Probability of Income > 50k") +
  xlab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

### HOW WELL DOES LOGIT 4 FIT THE DATA?
#does it make accurate predictions?
model_data <- model.frame(logit4)
model_data$preds <- predict(logit4, newdata = model_data, type = "response")
# Create predicted class variable based on 50% threshold
model_data$predicted_value <- ifelse(model_data$preds >= 0.5, 1, 0)
# Calculate accuracy
accuracy <- mean(model_data$predicted_value == model_data$income_binary)
# Print accuracy as a percentage
accuracy_percent <- round(accuracy * 100, 2)
cat("Prediction accuracy:", accuracy_percent, "%\n")

