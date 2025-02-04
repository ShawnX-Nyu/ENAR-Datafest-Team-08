library(readxl)
library(dplyr)
library(ggplot2)
library(pscl)
library(pROC)
file_path <- "nhanes_data_clean_low.csv"
ENARdata <- read.csv(file_path)

head(ENARdata)
str(ENARdata)

# change yes and no to numeric value of 1 and 0
ENARdata_01 <- ENARdata %>% 
  mutate(across(where(is.character), ~ ifelse(. == "Yes", 1, ifelse(. == "No", 0, .))))

str(ENARdata_01)

# Number of cases
table(ENARdata_01$htn_jnc7)

#prevalence rate total
prop.table(table(ENARdata_01$htn_jnc7))

ggplot(ENARdata_01, aes(x = demo_gender, fill = as.factor(htn_jnc7))) +
  geom_bar(position = "fill") +
  labs(title = "Gender vs Hypertension", y = "Proportion", fill = "Hypertension")

ggplot(ENARdata_01, aes(x = htn_jnc7, y = cc_hba1c)) +
  geom_boxplot() +
  labs(title = "HbA1c vs Hypertension", x = "Hypertension", y = "HbA1c")

chisq.test(table(ENARdata_01$htn_jnc7, ENARdata_01$demo_gender))
chisq.test(table(ENARdata_01$htn_jnc7, ENARdata_01$demo_race))

t.test(cc_hba1c ~ htn_jnc7, data = ENARdata_01)

wilcox.test(cc_hba1c ~ htn_jnc7, data = ENARdata_01)
str(ENARdata_01)

ENARdata_01$demo_gender <- as.factor(ENARdata_01$demo_gender)
ENARdata_01$cc_smoke <- as.factor(ENARdata_01$cc_smoke)
ENARdata_01$cc_diabetes <- as.factor(ENARdata_01$cc_diabetes)
ENARdata_01$htn_jnc7 <- as.factor(ENARdata_01$htn_jnc7)

model <- glm(htn_jnc7 ~ demo_age_years + demo_gender + cc_bmi + cc_hba1c + cc_smoke + cc_diabetes,
             data = ENARdata_01, family = binomial)
summary(model)


levels(ENARdata_01$demo_gender)
levels(ENARdata_01$cc_smoke)

ENARdata_01 <- na.omit(ENARdata_01)

exp(coef(model))
pR2(model)
roc_curve <- roc(ENARdata_01$htn_jnc7, fitted(model))
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)
# auc is 84, good fit

ENARdata_01$htn_jnc7 <- factor(ENARdata_01$htn_jnc7, levels = c(0, 1))
predicted <- as.factor(ifelse(fitted(model) > 0.5, 1, 0))
levels(predicted) <- c(0, 1)

library(caret)
confusionMatrix(as.factor(ifelse(fitted(model) > 0.5, 1, 0)), ENARdata_01$htn_jnc7)
plot(model, which = 1)  

library(car)
vif(model)
anova(model, test = "Chisq")
#residual plot nonlinear, heteroscedasticity


# 2. General analysis of the population prevalence, drawing trend line for the logistic regression model, then doing certain interaction term regression (I don't think the interaction would be that helpful tbh, but just in case).

# In this section with the trend line, it pretty much confirm that BMI and Diabetes are the most important factors for hypertension under HNC7 level.

#trend of hypertension
#Calculate the sum of hypertension each year
sum_hyper_yr <- ENARdata_01 %>% 
  group_by(svy_year) %>%
  dplyr::summarize(sum_jnc7 = sum(htn_jnc7), total_obs = n(), prevalence = (sum_jnc7/total_obs)*100)
print(sum_hyper_yr)

ggplot(sum_hyper_yr, aes(x=svy_year, y = prevelance))+
  geom_point(color = "blue", size = 3) +
  labs (
    title = "percentage of hypertension of each survey time", 
    x = "survey time",
    y = "prevalence"
  ) + 
  ylim(25, 50) +
  theme_minimal()


# Yearly logistic regression models
yearly_reg <- ENARdata_01 %>%
  group_by(svy_year) %>%
  do(model = glm(htn_jnc7 ~ demo_age_years + demo_gender + cc_bmi + cc_hba1c + cc_smoke + cc_diabetes,
                 data = ., family = binomial)) %>%
  summarise(
    year = svy_year,
    term = names(coef(model)),
    estimate = coef(model)
  )
# results
str(yearly_reg)
head(yearly_reg)

#trends of coefficient by year
ggplot(yearly_reg, aes(x = year, y = estimate, color = term, group = term)) +
  geom_line(size = 1) +         # Add lines for each term
  geom_point(size = 2) +        # Add points for better visibility
  labs(
    title = "Trends in Predictors of Hypertension Over Time",
    x = "Year",
    y = "Coefficient Estimate",
    color = "Predictor"
  ) +
  ylim(-1, 2) + 
  theme_minimal()

#Interaction 
## age * BMI
model_AgeBMI <- glm(htn_jnc7 ~ demo_age_years *cc_bmi + demo_gender + cc_smoke + cc_diabetes, data = ENARdata_01)
summary(model_AgeBMI)

## age * gender
model_AgeGen <- glm(htn_jnc7 ~ demo_age_years * demo_gender + cc_bmi + cc_smoke + cc_diabetes, data = ENARdata_01)
summary(model_AgeGen)

## diabetes * age
model_DiaAge <- glm(htn_jnc7 ~ demo_age_years *cc_diabetes + cc_bmi + demo_gender + cc_smoke , data = ENARdata_01)
summary(model_DiaAge)


# 3. Finding the trend of treatment, how it has impact the hypertension?

#convert variable type to numeric for trend calculating
ENARdata_01$bp_med_use <- as.numeric(ENARdata_01$bp_med_use)

#trend of pills taking
bp_med_trend <- ENARdata_01 %>%
  group_by(svy_year) %>%
  summarise(
    medication_use = mean(bp_med_use, na.rm = TRUE),
    # Proportion of bp_med use
    se = sd(bp_med_use, na.rm = TRUE) / sqrt(n())
    # standard error
  )
#result
print(bp_med_trend)

#draw the trend line
ggplot(bp_med_trend, aes(x = svy_year, y = medication_use, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Add a line for the trend
  geom_point(color = "red", size = 3) +  # Add points for each year
  labs(
    title = "Trend of Antihypertensive Medication Use Over Time",
    x = "Year",
    y = "Proportion Using Antihypertensive Medication"
  ) +
  # Format y-axis as percentages
  scale_y_continuous(labels = scales::percent) +  
  theme_minimal()

# some general statistic
# race
race_summary <- ENARdata_01 %>% 
  count(demo_race) %>%
  mutate(perc = n / sum(n) * 100)

ggplot(race_summary, aes(x = "", y = n, fill = demo_race)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(
    aes(label = paste0(round(perc, 1), "%")),
    position = position_stack(vjust = 0.5), # place text in the middle of each slice
    color = "white"                         # for better contrast (optional)
  ) +
  coord_polar("y") +
  labs(title = "Distribution of Race")
#####
# demographic variable
ENARdata_01 %>% 
  group_by(cc_bmi)  %>%
  summarize(count = n()) %>%
  mutate(
    proportion = count / sum(count),
    percentage = proportion * 100
  )

#VIF, collinearity check
require(faraway)
x <- model.matrix(model)
vif(x)
