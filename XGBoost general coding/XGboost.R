library(xgboost)
library(dplyr)
library(Matrix)
library(caret)
library(pROC)
library(ggplot2)

ENAR_Merge_Clean <- read.csv("ENAR_Merge_Clean.csv")
ENAR_Merge_Clean$Highbp <- as.factor(ENAR_Merge_Clean$Highbp)
ENAR_Merge_Clean <- ENAR_Merge_Clean %>% select(-average_physical_activity, -cc_smoke)

numeric_features <- ENAR_Merge_Clean %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-c(SEQN, X, demo_age_years))  
X <- as.matrix(numeric_features)
y <- as.numeric(as.character(ENAR_Merge_Clean$Highbp))  

# 80% taining, 20% testing.
set.seed(123)
train_index <- sample(1:nrow(X), 0.8 * nrow(X))
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]
# format changing
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.0025,              
  max_depth = 4,           
  min_child_weight = 30,    
  subsample = 0.6,         
  colsample_bytree = 0.55,   
  lambda = 20,              
  alpha = 10,               
  gamma = 3                 
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1500,                     # 
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 60           # 
)

importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "XGBoost feature analysis", x = "Feature", y = "Gain")
# library(ggplot2)
# library(reshape2)
# cor_matrix <- cor(ENAR_Merge_Clean %>% select_if(is.numeric), use = "pairwise.complete.obs")
# cor_melted <- melt(cor_matrix)  
#
# ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                        midpoint = 0, limit = c(-1, 1), space = "Lab") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ggtitle("Variable Correlation Heatmap")




