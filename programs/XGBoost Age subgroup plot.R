library(xgboost)
library(dplyr)
library(ggplot2)
library(caret)
library(SHAPforxgboost)
library(Matrix)

library(xgboost)
library(pROC)
library(SHAPforxgboost)

ENAR_Merge_Clean <- read.csv("ENAR_Merge_Clean.csv")
ENAR_Merge_Clean2 <- ENAR_Merge_Clean %>%
  dplyr::select(
    Highbp,
    demo_age_cat,
    demo_gender,
    demo_race,
    cc_smoke, cc_bmi, cc_diabetes, cc_ckd, cc_cvd_any,
    chol_ldl, insulin_uUmL,
    FoodSec_Level
  ) %>%
  
  filter(!is.na(Highbp))

summary(ENAR_Merge_Clean2$demo_age_cat)
str(ENAR_Merge_Clean2)

ENAR_Merge_Clean2 <- ENAR_Merge_Clean2 %>%
  mutate(across(c(demo_race, cc_bmi, FoodSec_Level), as.factor))


ENAR_Merge_Clean2$cc_smoke <- factor(ENAR_Merge_Clean2$cc_smoke, level = c(0, 1, 2), label=c("Never","Former","Current"))

ENAR_Merge_Clean2$cc_bmi <- factor(ENAR_Merge_Clean2$cc_bmi, level = c(0, 1, 2, 3), label=c("BMI < 25","BMI 25 to <30","BMI 30 to <35", "BMI > 35"))

ENAR_Merge_Clean2$FoodSec_Level <- factor(ENAR_Merge_Clean2$FoodSec_Level, level = c(1, 2, 3, 4), label=c("Full","marginal","low", "very low"))

ENAR_Merge_Clean2$demo_race <- factor(ENAR_Merge_Clean2$demo_race, level = c(1, 2, 3, 4, 5), label=c("Hispanic","White","Black","Asian", "Other"))

ENAR_Merge_Clean2$demo_gender <- factor(ENAR_Merge_Clean2$demo_gender, level = c(1, 2), label=c("Male", "Female"))

df_18_44 <- ENAR_Merge_Clean2 %>% filter(demo_age_cat == 1)
df_45_64 <- ENAR_Merge_Clean2 %>% filter(demo_age_cat == 2)
df_65_74 <- ENAR_Merge_Clean2 %>% filter(demo_age_cat == 3)
df_75_up <- ENAR_Merge_Clean2 %>% filter(demo_age_cat == 4)


set.seed(123)

################################### age-group 18-44

index_18_44 <- createDataPartition(df_18_44$Highbp, p=0.7, list=FALSE)
train_18_44 <- df_18_44[index_18_44, ]
train_18_44 <- train_18_44 %>% dplyr::select(-demo_age_cat)
test_18_44  <- df_18_44[-index_18_44, ]
keep <- sapply(train_18_44, function(x) length(unique(x)) > 1)
train_18_44_clean <- train_18_44[ , keep]
train_x <- model.matrix(Highbp ~ . - 1, data=train_18_44_clean)

train_y <- train_18_44$Highbp

test_x  <- model.matrix(Highbp ~ . - 1, data=train_18_44_clean)
test_y  <- train_18_44_clean$Highbp


dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x,  label = test_y)


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

watchlist <- list(train = dtrain, test = dtest)

xgb_model_18_44 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = watchlist,
  early_stopping_rounds = 20
)

pred_prob <- predict(xgb_model_18_44, newdata = dtest)
pred_label <- ifelse(pred_prob > 0.5, 1, 0)


auc_18_44 <- roc(test_y, pred_prob)$auc
acc_18_44 <- mean(pred_label == test_y)

cat("AUC(18-44): ", auc_18_44, "\nAccuracy(18-44): ", acc_18_44, "\n")
importance_18_44 <- xgb.importance(model = xgb_model_18_44, feature_names = colnames(train_x))
print(importance_18_44)
xgb.plot.importance(importance_18_44)
############ SHAP

shap_result_18_44 <- shap.values(
  xgb_model_18_44,      
  X_train = train_x     
)

shap_long_18_44 <- shap.prep(
  xgb_model_18_44,
  X_train = train_x
)

shap.plot.summary(shap_long_18_44)

#################################
# 44-64
index_45_64 <- createDataPartition(df_45_64$Highbp, p=0.7, list=FALSE)
train_45_64 <- df_45_64[index_45_64, ]
train_45_64 <- train_45_64 %>% dplyr::select(-demo_age_cat)
test_45_64  <- df_45_64[-index_45_64, ]
keep <- sapply(train_45_64, function(x) length(unique(x)) > 1)
train_45_64_clean <- train_45_64[ , keep]
train_x <- model.matrix(Highbp ~ . - 1, data=train_45_64_clean)

train_y <- train_45_64$Highbp

test_x  <- model.matrix(Highbp ~ . - 1, data=train_45_64_clean)
test_y  <- train_45_64_clean$Highbp


dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x,  label = test_y)


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

watchlist <- list(train = dtrain, test = dtest)

xgb_model_45_64 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = watchlist,
  early_stopping_rounds = 20
)

pred_prob <- predict(xgb_model_45_64, newdata = dtest)
pred_label <- ifelse(pred_prob > 0.5, 1, 0)

library(pROC)
auc_45_64 <- roc(test_y, pred_prob)$auc
acc_45_64 <- mean(pred_label == test_y)

cat("AUC(45-64): ", auc_45_64, "\nAccuracy(45-64): ", acc_45_64, "\n")
importance_45_64 <- xgb.importance(model = xgb_model_45_64, feature_names = colnames(train_x))
print(importance_45_64)
xgb.plot.importance(importance_45_64)
############ SHAP
shap_result_45_64 <- shap.values(
  xgb_model_45_64,      
  X_train = train_x     
)

shap_long_45_64 <- shap.prep(
  xgb_model_45_64,
  X_train = train_x
)

shap.plot.summary(shap_long_45_64)


#################################
# 65-74
index_65_74 <- createDataPartition(df_65_74$Highbp, p=0.7, list=FALSE)
train_65_74 <- df_65_74[index_65_74, ]
train_65_74 <- train_65_74 %>% dplyr::select(-demo_age_cat)
test_65_74  <- df_65_74[-index_65_74, ]
keep <- sapply(train_65_74, function(x) length(unique(x)) > 1)
train_65_74_clean <- train_65_74[ , keep]
train_x <- model.matrix(Highbp ~ . - 1, data=train_65_74_clean)

train_y <- train_65_74$Highbp

test_x  <- model.matrix(Highbp ~ . - 1, data=train_65_74_clean)
test_y  <- train_65_74_clean$Highbp


dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x,  label = test_y)


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

watchlist <- list(train = dtrain, test = dtest)

xgb_model_65_74 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = watchlist,
  early_stopping_rounds = 20
)

pred_prob <- predict(xgb_model_65_74, newdata = dtest)
pred_label <- ifelse(pred_prob > 0.5, 1, 0)

library(pROC)
auc_65_74 <- roc(test_y, pred_prob)$auc
acc_65_74 <- mean(pred_label == test_y)

cat("AUC(65_74): ", auc_65_74, "\nAccuracy(65_74): ", acc_65_74, "\n")
importance_65_74 <- xgb.importance(model = xgb_model_65_74, feature_names = colnames(train_x))
print(importance_65_74)
xgb.plot.importance(importance_65_74)
############ SHAP
shap_result_65_74 <- shap.values(
  xgb_model_65_74,      
  X_train = train_x     
)

shap_long_65_74 <- shap.prep(
  xgb_model_65_74,
  X_train = train_x
)

shap.plot.summary(shap_long_65_74)


#################################
# 75_up
index_75_up <- createDataPartition(df_75_up$Highbp, p=0.7, list=FALSE)
train_75_up <- df_75_up[index_75_up, ]
train_75_up <- train_75_up %>% dplyr::select(-demo_age_cat)
test_75_up  <- df_75_up[-index_75_up, ]
keep <- sapply(train_75_up, function(x) length(unique(x)) > 1)
train_75_up_clean <- train_75_up[ , keep]
train_x <- model.matrix(Highbp ~ . - 1, data=train_75_up_clean)

train_y <- train_75_up$Highbp

test_x  <- model.matrix(Highbp ~ . - 1, data=train_75_up_clean)
test_y  <- train_75_up_clean$Highbp


dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x,  label = test_y)


params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

watchlist <- list(train = dtrain, test = dtest)

xgb_model_75_up <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = watchlist,
  early_stopping_rounds = 20
)

pred_prob <- predict(xgb_model_75_up, newdata = dtest)
pred_label <- ifelse(pred_prob > 0.5, 1, 0)

library(pROC)
auc_75_up <- roc(test_y, pred_prob)$auc
acc_75_up <- mean(pred_label == test_y)

cat("AUC(75_up): ", auc_75_up, "\nAccuracy(75_up): ", acc_75_up, "\n")
importance_75_up <- xgb.importance(model = xgb_model_75_up, feature_names = colnames(train_x))
print(importance_75_up)
xgb.plot.importance(importance_75_up)
############ SHAP
shap_result_75_up <- shap.values(
  xgb_model_75_up,      
  X_train = train_x     
)

shap_long_75_up <- shap.prep(
  xgb_model_75_up,
  X_train = train_x
)

shap.plot.summary(shap_long_75_up)
