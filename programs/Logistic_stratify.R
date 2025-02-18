library(xgboost)
library(dplyr)
library(Matrix)
library(caret)
library(pROC)
library(ggplot2)
library(broom)

ENAR_Merge_Clean <- read.csv("/Users/kangyiyuan/Desktop/ENAR/ENAR_Merge_Clean.csv")
ENAR_Merge_Clean <- ENAR_Merge_Clean %>%
  dplyr::select(-cc_acr, -cc_hba1c, -cc_egfr, -cc_egfr_lt60, -cc_acr_gteq30, -demo_age_years)
ENAR_Merge_Clean$Highbp <- as.factor(ENAR_Merge_Clean$Highbp)

logit_model <- glm(Highbp ~ demo_age_cat + cc_bmi + cc_ckd + cc_diabetes + demo_gender + insulin_uUmL,
                   data = ENAR_Merge_Clean, family = binomial)
summary(logit_model)
exp(coef(logit_model))


# 提取 Logistic 回归结果（OR & 95% 置信区间）
logit_results <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE)

# 重新命名列名（方便可视化）
colnames(logit_results) <- c("Predictor", "Estimate", "Std_Error", "Z_value", "P_value", "CI_Lower", "CI_Upper")

# 过滤掉 Intercept
logit_results <- logit_results[logit_results$Predictor != "(Intercept)", ]

# 重新排序变量（按 OR 大小排序）
logit_results <- logit_results[order(logit_results$Estimate, decreasing = TRUE), ]

ggplot(logit_results, aes(x = reorder(Predictor, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "black") +
  geom_text(aes(label = round(Estimate, 2)), hjust = -0.2, size = 4) +  # 添加 OR 数值
  coord_flip() +
  scale_y_continuous(limits = c(0, max(logit_results$CI_Upper) + 0.5)) +
  labs(title = "Logistic Regression: Odds Ratios for High Blood Pressure",
       x = "Predictors", y = "Odds Ratio (95% CI)") +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black")  # 添加 OR=1 参考线



# Interaction Analysis
# BMI 对不同年龄段人群的影响是否不同
# BMI 是否对男性和女性的高血压影响不同
interaction_model <- glm(Highbp ~ demo_age_cat * cc_bmi + demo_gender * cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL,
                         data = ENAR_Merge_Clean, family = binomial)

# 查看回归结果
summary(interaction_model)




# 按种族分组
df_hispanic <- ENAR_Merge_Clean %>% filter(demo_race == 1)
df_white <- ENAR_Merge_Clean %>% filter(demo_race == 2)
df_black <- ENAR_Merge_Clean %>% filter(demo_race == 3)
df_asian <- ENAR_Merge_Clean %>% filter(demo_race == 4)
df_other <- ENAR_Merge_Clean %>% filter(demo_race == 5)

# Hispanic 族群回归分析
logit_hispanic <- glm(Highbp ~ demo_age_cat + demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL, 
                      data = df_hispanic, family = binomial)

# Non-Hispanic White 族群回归分析
logit_white <- glm(Highbp ~ demo_age_cat + demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL, 
                   data = df_white, family = binomial)

# Non-Hispanic Black 族群回归分析
logit_black <- glm(Highbp ~ demo_age_cat + demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL, 
                   data = df_black, family = binomial)

# Non-Hispanic Asian 族群回归分析
logit_asian <- glm(Highbp ~ demo_age_cat + demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL, 
                   data = df_asian, family = binomial)

# Other 族群回归分析
logit_other <- glm(Highbp ~ demo_age_cat + demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL, 
                   data = df_other, family = binomial)

# 查看每个种族的回归结果
summary(logit_hispanic)
summary(logit_white)
summary(logit_black)
summary(logit_asian)
summary(logit_other)

# 计算 OR（胜算比）
exp(coef(logit_hispanic))
exp(coef(logit_white))
exp(coef(logit_black))
exp(coef(logit_asian))
exp(coef(logit_other))

# 提取 OR 及置信区间
library(dplyr)
library(ggplot2)
library(broom)

# 运行 Logistic 回归 并计算 OR 和 95% 置信区间
or_data <- bind_rows(
  tidy(logit_hispanic, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Race = "Hispanic"),
  tidy(logit_white, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Race = "White"),
  tidy(logit_black, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Race = "Black"),
  tidy(logit_asian, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Race = "Asian"),
  tidy(logit_other, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Race = "Other")
) %>%
  rename(Odds_Ratio = estimate, Lower_CI = conf.low, Upper_CI = conf.high)  # 统一列名
or_data <- or_data[or_data$term != "(Intercept)", ]
# 绘制 OR 条形图
library(ggplot2)

# RACE-----------------------------------------------------------------------------------------------------------------------
custom_colors <- c("grey", "#FBD178", "#BFE4EE", "#F9C3BF","#BAD19F")
desired_order1 <- c("demo_gender","demo_age_cat", "cc_bmi", "cc_ckd", "cc_diabetes", "insulin_uUmL")
plot1 <- ggplot(or_data, aes(x = factor(term, levels = desired_order1), y = Odds_Ratio, fill = Race)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5, alpha = 0.8) +  # 控制条形宽度
  scale_fill_manual(values = custom_colors) +  # 应用自定义颜色
  labs(title = "Odds Ratios of Hypertension by Race",
       x = "Predictors", y = "Odds Ratio (95% CI)", fill = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))
plot1

# 计算 OR
exp(coef(interaction_model))


# 按年龄段进行分层
df_18_44 <- ENAR_Merge_Clean %>% filter(demo_age_cat == 1)
df_45_64 <- ENAR_Merge_Clean %>% filter(demo_age_cat == 2)
df_65_74 <- ENAR_Merge_Clean %>% filter(demo_age_cat == 3)
df_75_plus <- ENAR_Merge_Clean %>% filter(demo_age_cat == 4)

# 18-44 岁回归分析
logit_18_44 <- glm(Highbp ~ demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL + demo_race, 
                   data = df_18_44, family = binomial)

# 45-64 岁回归分析
logit_45_64 <- glm(Highbp ~ demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL + demo_race, 
                   data = df_45_64, family = binomial)

# 65-74 岁回归分析
logit_65_74 <- glm(Highbp ~ demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL + demo_race, 
                   data = df_65_74, family = binomial)
# 75+ 岁回归分析
logit_75_plus <- glm(Highbp ~ demo_gender + cc_bmi + cc_ckd + cc_diabetes + insulin_uUmL + demo_race, 
                     data = df_75_plus, family = binomial)

# 查看各个年龄段的回归结果
summary(logit_18_44)
summary(logit_45_64)
summary(logit_65_74)
summary(logit_75_plus)

# 计算 OR（胜算比）
exp(coef(logit_18_44))
exp(coef(logit_45_64))
exp(coef(logit_65_74))
exp(coef(logit_75_plus))

# 提取不同年龄层的 Logistic 回归 OR 结果
extract_or <- function(model, age_group) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(Age_Group = age_group)
}

# 提取各个年龄层的 OR 结果
or_18_44 <- extract_or(logit_18_44, "18-44")
or_45_64 <- extract_or(logit_45_64, "45-64")
or_65_74 <- extract_or(logit_65_74, "65-74")
or_75_plus <- extract_or(logit_75_plus, "75+")

# 合并所有年龄层数据
or_combined <- bind_rows(or_18_44, or_45_64, or_65_74, or_75_plus)
colnames(or_combined) <- c("Predictor", "Odds_Ratio", "Std_Error", "Z_value", "P_value", "CI_Lower", "CI_Upper", "Age_Group")
or_combined <- or_combined[or_combined$Predictor != "(Intercept)", ]

# 提取每个年龄组的OR值并排序
or_age_group <- or_combined %>%
  dplyr::select(Age_Group, Odds_Ratio) %>%
  arrange(desc(Odds_Ratio))

# 查看排序后的结果
print(or_age_group)

# 进行排序，找出哪个年龄组的OR最大
or_age_group_sorted <- or_age_group %>%
  arrange(desc(Odds_Ratio))

# 查看排序后的结果
print(or_age_group_sorted)
# 打印结果

# AGE -----------------------------------------------------------------------------------------------------------------------
desired_order2 <- c("demo_gender","demo_race", "cc_bmi", "cc_ckd", "cc_diabetes", "insulin_uUmL")
plot2 <- ggplot(or_combined, aes(x = factor(Predictor, levels = desired_order2), y = Odds_Ratio, fill = Age_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5, alpha = 0.8) +  # 控制条形宽度
  scale_fill_manual(values = custom_colors) +  # 应用自定义颜色
  labs(title = "Odds Ratios of Hypertension by Age",
       x = "Predictors", y = "Odds Ratio (95% CI)", fill = "Age_Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))
plot2
# 对18-44岁组进行预测
pred_18_44 <- predict(logit_18_44, type = "response")
pred_class <- ifelse(pred_18_44 > 0.5, 1, 0)
print(pred_class)
# 对45-64岁组进行预测
pred_45_64 <- predict(logit_45_64, type = "response")

# 对65-74岁组进行预测
pred_65_74 <- predict(logit_65_74, type = "response")

# 对75+岁组进行预测
pred_75_plus <- predict(logit_75_plus, type = "response")

library(pROC)

# ROC曲线
roc_18_44 <- roc(df_18_44$Highbp, pred_18_44)
roc_45_64 <- roc(df_45_64$Highbp, pred_45_64)
roc_65_74 <- roc(df_65_74$Highbp, pred_65_74)
roc_75_plus <- roc(df_75_plus$Highbp, pred_75_plus)

# 绘制ROC曲线
plot(roc_18_44, col = "red", main = "ROC Curves by Age Group")
lines(roc_45_64, col = "blue")
lines(roc_65_74, col = "green")
lines(roc_75_plus, col = "purple")

# 显示AUC值
print(paste("AUC for 18-44: ", auc(roc_18_44)))
print(paste("AUC for 45-64: ", auc(roc_45_64)))
print(paste("AUC for 65-74: ", auc(roc_65_74)))
print(paste("AUC for 75+: ", auc(roc_75_plus)))

# GENDER
# 按性别分层
df_male <- ENAR_Merge_Clean %>% filter(demo_gender == 1)  # 男性
df_female <- ENAR_Merge_Clean %>% filter(demo_gender == 2)  # 女性

# Logistic 回归分析
logit_male <- glm(Highbp ~ cc_bmi + cc_ckd + cc_diabetes + demo_age_cat + insulin_uUmL + demo_race, 
                  data = df_male, family = binomial)
logit_female <- glm(Highbp ~ cc_bmi + cc_ckd + cc_diabetes + demo_age_cat + insulin_uUmL + demo_race, 
                    data = df_female, family = binomial)

# 提取 OR（胜算比）及 95% 置信区间
or_male <- tidy(logit_male, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Gender = "Male")
or_female <- tidy(logit_female, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Gender = "Female")

# 合并数据
or_gender <- bind_rows(or_male, or_female)
colnames(or_gender) <- c("Predictor", "Odds_Ratio", "Std_Error", "Z_value", "P_value", "CI_Lower", "CI_Upper", "Gender")
or_gender <- or_gender[or_gender$Predictor != "(Intercept)", ]  # 去掉 Intercept

# Gender -----------------------------------------------------------------------------------------------------------------------
desired_order3 <- c("demo_age_cat","demo_race", "cc_bmi", "cc_ckd", "cc_diabetes", "insulin_uUmL")
plot3 <- ggplot(or_gender, aes(x = factor(Predictor,desired_order3), y = Odds_Ratio, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.5, alpha = 0.8) +
  labs(title = "Odds Ratios of Hypertension by Gender",
       x = "Predictors", y = "Odds Ratio (95% CI)", fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))
plot3

# 确保 cc_bmi 是字符型
df_0 <- ENAR_Merge_Clean %>% filter(cc_bmi == 0) #"<25" 
df_1 <- ENAR_Merge_Clean %>% filter(cc_bmi == 1) #"25 to <30"
df_2 <- ENAR_Merge_Clean %>% filter(cc_bmi == 2) #"30 to <35"
df_3 <- ENAR_Merge_Clean %>% filter(cc_bmi == 3) #"35+"
# **按 BMI 组别进行 Logistic 回归**
logit_bmi_less25 <- glm(Highbp ~ demo_gender + cc_ckd + cc_diabetes + insulin_uUmL + demo_age_cat + demo_race, 
                        data = df_0, family = binomial)
logit_bmi_2530 <- glm(Highbp ~ demo_gender + cc_ckd + cc_diabetes + insulin_uUmL + demo_age_cat + demo_race, 
                      data = df_1, family = binomial)

logit_bmi_3035 <- glm(Highbp ~ demo_gender + cc_ckd + cc_diabetes + insulin_uUmL + demo_age_cat + demo_race, 
                      data = df_2, family = binomial)

logit_bmi_over35 <- glm(Highbp ~ demo_gender + cc_ckd + cc_diabetes + insulin_uUmL + demo_age_cat + demo_race, 
                        data = df_3, family = binomial)

# **提取 OR 及 95% CI**
or_bmi <- bind_rows(
  tidy(logit_bmi_less25, conf.int = TRUE, exponentiate = TRUE) %>% mutate(BMI_Group = "Less than 25"),
  tidy(logit_bmi_2530, conf.int = TRUE, exponentiate = TRUE) %>% mutate(BMI_Group = "Between 25 to 30"),
  tidy(logit_bmi_3035, conf.int = TRUE, exponentiate = TRUE) %>% mutate(BMI_Group = "Between 30 to 35"),
  tidy(logit_bmi_over35, conf.int = TRUE, exponentiate = TRUE) %>% mutate(BMI_Group = "Over 35")
) %>%
  rename(Odds_Ratio = estimate, Lower_CI = conf.low, Upper_CI = conf.high) %>%
  filter(term != "(Intercept)")  # 移除 Intercept

# BMI------------------------------------------------------------------------------------------------------------------------
or_bmi$BMI_Group <- factor(or_bmi$BMI_Group, 
                           levels = c("Less than 25", "Between 25 to 30", "Between 30 to 35", "Over 35"))
desired_order4 <- c("demo_age_cat", "demo_gender","demo_race", "cc_ckd", "cc_diabetes", "insulin_uUmL")
plot4 <- ggplot(or_bmi, aes(x = factor(term,desired_order4), y = Odds_Ratio, fill = BMI_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5, alpha = 0.8) +  # 控制条形宽度
  scale_fill_manual(values = custom_colors) +  # 应用自定义颜色
  labs(title = "Odds Ratios of Hypertension by BMI",
       x = "Predictors", y = "Odds Ratio (95% CI)", fill = "BMI_Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))
plot4


# DIABETS
df_diabetes_0 <- ENAR_Merge_Clean %>% filter(cc_diabetes == 0) # No Diabetes
df_diabetes_1 <- ENAR_Merge_Clean %>% filter(cc_diabetes == 1) # Has Diabetes
# Run logistic regressions for each subgroup based on 'cc_diabetes'
logit_diabetes_0 <- glm(Highbp ~ demo_gender + cc_ckd + insulin_uUmL + demo_age_cat + demo_race + cc_bmi, 
                        data = df_diabetes_0, family = binomial)

logit_diabetes_1 <- glm(Highbp ~ demo_gender + cc_ckd + insulin_uUmL + demo_age_cat + demo_race + cc_bmi, 
                        data = df_diabetes_1, family = binomial)
# Extract OR and 95% CI for each subgroup
or_diabetes <- bind_rows(
  tidy(logit_diabetes_0, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Diabetes_Status = "No Diabetes"),
  tidy(logit_diabetes_1, conf.int = TRUE, exponentiate = TRUE) %>% mutate(Diabetes_Status = "Has Diabetes")
) %>%
  rename(Odds_Ratio = estimate, Lower_CI = conf.low, Upper_CI = conf.high) %>%
  filter(term != "(Intercept)")  # Remove Intercept

# Adjust the factor levels for Diabetes Status
or_diabetes$Diabetes_Status <- factor(or_diabetes$Diabetes_Status, 
                                      levels = c("No Diabetes", "Has Diabetes"))

# DIABETS------------------------------------------------------------------------------------------------------------------------
desired_order5 <- c("demo_age_cat", "demo_gender", "cc_bmi", "demo_race", "cc_ckd", "insulin_uUmL")
plot5 <- ggplot(or_diabetes, aes(x = factor(term, desired_order5), y = Odds_Ratio, fill = Diabetes_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5, alpha = 0.8) + 
  labs(title = "Odds Ratios of Hypertension by Diabetes Status",
       x = "Predictors", y = "Odds Ratio (95% CI)", fill = "Diabetes Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"))
plot5

library(patchwork)
# 使用 patchwork 结合五个图表并调整布局
plot1 + plot2 + plot3 + plot4 + plot5 


#------------------------------------------------------------------------------------------------------------------------
# Descriptive Statistics Table 

library(table1)
# 示例数据
set.seed(42)
data <- data.frame(
  Group = sample(c("A", "B"), 200, replace = TRUE),
  Age = sample(20:70, 200, replace = TRUE),
  BP = rnorm(200, mean = 120, sd = 15),
  Cholesterol = rnorm(200, mean = 200, sd = 30),
  Gender = sample(c("Male", "Female"), 200, replace = TRUE),
  cc_bmi = sample(c("<25", "25-30", "30-35", "35+"), 200, replace = TRUE),
  cc_diabetes = sample(c("No", "Yes"), 200, replace = TRUE),
  demo_age_cat = sample(c("18-44", "45-64", "65-74", "75+"), 200, replace = TRUE),
  demo_race = sample(c("Asian", "Black", "Hispanic", "White"), 200, replace = TRUE),
  demo_gender = sample(c("Male", "Female"), 200, replace = TRUE),
  insulin_uUmL = rnorm(200, mean = 15, sd = 5)
)

# 使用 table1 创建描述性统计表
table1_result <- table1(~ Age + BP + Cholesterol + Gender + cc_bmi + cc_diabetes + demo_age_cat + demo_race + demo_gender + insulin_uUmL | Group, data = data)

# 打印表格
print(table1_result)
# 设置路径保存为 PDF
png("/Users/kangyiyuan/Desktop/ENAR/Descriptive Statistics Table.png", width = 1200, height = 800)
dev.off()


library(table1)
# 示例数据
set.seed(42)
data1 <- data.frame(
  Age = sample(18:80, 200, replace = TRUE),
  BP = rnorm(200, mean = 120, sd = 15),
  Cholesterol = rnorm(200, mean = 200, sd = 30),
  Gender = sample(c("Male", "Female"), 200, replace = TRUE),
  cc_bmi = sample(c("<25", "25-30", "30-35", "35+"), 200, replace = TRUE),
  cc_diabetes = sample(c("No", "Yes"), 200, replace = TRUE),
  demo_age_cat = sample(c("18-44", "45-64", "65-74", "75+"), 200, replace = TRUE),
  demo_race = sample(c("Asian", "Black", "Hispanic", "White"), 200, replace = TRUE),
  demo_gender = sample(c("Male", "Female"), 200, replace = TRUE),
  insulin_uUmL = rnorm(200, mean = 15, sd = 5)
)

# 使用 table1 创建描述性统计表
table2_result <- table1(~ Age + BP + Cholesterol + Gender + cc_bmi + cc_diabetes + demo_age_cat + demo_race + demo_gender + insulin_uUmL, data = data1)

# 打印表格
print(table2_result)
# 设置路径保存为 PDF
png("/Users/kangyiyuan/Desktop/ENAR/Descriptive Statistics Table2.png", width = 1200, height = 800)
dev.off()

