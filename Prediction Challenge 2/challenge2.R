############################################################
# Prediction Challenge 2 - Earnings
# Data 101 - Full Script
# - EDA + plots
# - Feature engineering
# - Baseline model (predict mean)
# - Linear model with interactions
# - rpart regression tree
# - Random forest (final model)
# - 5-fold CV comparison (MSE)
# - submission.csv creation at:
#   C:/Users/cjfis/OneDrive/Documents/Data 101/Prediction Challenge 2/submission.csv
############################################################

# ---- 0. Libraries ----
# Install these once if needed:
# install.packages("tidyverse")
# install.packages("rpart")
# install.packages("randomForest")

library(tidyverse)
library(rpart)
library(randomForest)

set.seed(123)  # reproducibility

# ---- 1. Paths & Load training data ----

TRAIN_PATH <- "C:/Users/cjfis/OneDrive/Documents/Data 101/Prediction Challenge 2/Earnings_Train.csv"
TEST_PATH  <- "C:/Users/cjfis/OneDrive/Documents/Data 101/Prediction Challenge 2/earnings_test.csv"

earn <- read.csv(TRAIN_PATH)

str(earn)
summary(earn)

# Treat Major as a factor
earn$Major <- factor(earn$Major)

# ---- 2. EDA: Plots ----

# 2.1 Earnings by Major
ggplot(earn, aes(x = Major, y = Earnings)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Earnings by Major",
       x = "Major",
       y = "Annual Earnings")

# 2.2 Earnings vs GPA, colored by Major
ggplot(earn, aes(x = GPA, y = Earnings, color = Major)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Earnings vs GPA by Major",
       x = "GPA",
       y = "Earnings")

# 2.3 Earnings vs Professional Connections for Major == 'Other'
earn_other <- earn %>% filter(Major == "Other")

ggplot(earn_other, aes(x = Number_Of_Professional_Connections, y = Earnings)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Earnings vs Connections (Major = 'Other')",
       x = "Number of Professional Connections",
       y = "Earnings")

# ---- 3. Feature Engineering ----
# - GPA_Connections: interaction between GPA and networking
# - log_connections: log transform of connections to reduce skew

earn <- earn %>%
  mutate(
    GPA_Connections = GPA * Number_Of_Professional_Connections,
    log_connections = log1p(Number_Of_Professional_Connections)
  )

# Model-ready dataset
earn_model <- earn %>%
  select(
    Earnings,
    GPA,
    Number_Of_Professional_Connections,
    log_connections,
    GPA_Connections,
    Major,
    Graduation_Year,
    Height,
    Number_Of_Credits,
    Number_Of_Parking_Tickets
  )

# ---- 4. Baseline model: predict mean earnings ----

baseline_pred <- mean(earn_model$Earnings)
baseline_mse  <- mean((earn_model$Earnings - baseline_pred)^2)
baseline_rmse <- sqrt(baseline_mse)

cat("Baseline (predict mean) - MSE:", baseline_mse, "\n")
cat("Baseline (predict mean) - RMSE:", baseline_rmse, "\n\n")

# ---- 5. Cross-validation setup ----
set.seed(123)
K <- 5
n <- nrow(earn_model)
fold_id <- sample(rep(1:K, length.out = n))

cv_mse_generic <- function(fit_fun, pred_fun, data, folds) {
  K <- max(folds)
  mse_vec <- numeric(K)
  
  for (k in 1:K) {
    train_idx <- which(folds != k)
    val_idx   <- which(folds == k)
    
    train_k <- data[train_idx, ]
    val_k   <- data[val_idx, ]
    
    fit_k <- fit_fun(train_k)
    preds <- pred_fun(fit_k, val_k)
    
    mse_vec[k] <- mean((val_k$Earnings - preds)^2)
  }
  
  list(
    fold_mse = mse_vec,
    mean_mse = mean(mse_vec),
    mean_rmse = sqrt(mean(mse_vec))
  )
}

# ---- 6. Model 1: Linear Regression with Interactions ----

lm_formula <- Earnings ~ Major * GPA +
  Major * log_connections +
  GPA_Connections

lm_fit_fun <- function(train_data) {
  lm(lm_formula, data = train_data)
}

lm_pred_fun <- function(model, newdata) {
  predict(model, newdata = newdata)
}

cv_lm <- cv_mse_generic(lm_fit_fun, lm_pred_fun, earn_model, fold_id)

cat("Linear Model - 5-fold CV MSEs:\n")
print(cv_lm$fold_mse)
cat("Linear Model - Mean CV MSE:", cv_lm$mean_mse, "\n")
cat("Linear Model - Mean CV RMSE:", cv_lm$mean_rmse, "\n\n")

# ---- 7. Model 2: Regression Tree (rpart) ----

rpart_fit_fun <- function(train_data) {
  rpart(
    Earnings ~ GPA + log_connections + GPA_Connections +
      Major + Graduation_Year + Height +
      Number_Of_Credits + Number_Of_Parking_Tickets,
    data = train_data,
    method = "anova",
    control = rpart.control(
      minsplit = 50,
      cp = 0.0005
    )
  )
}

rpart_pred_fun <- function(model, newdata) {
  predict(model, newdata = newdata)
}

cv_rpart <- cv_mse_generic(rpart_fit_fun, rpart_pred_fun, earn_model, fold_id)

cat("rpart Tree - 5-fold CV MSEs:\n")
print(cv_rpart$fold_mse)
cat("rpart Tree - Mean CV MSE:", cv_rpart$mean_mse, "\n")
cat("rpart Tree - Mean CV RMSE:", cv_rpart$mean_rmse, "\n\n")

# ---- 8. Model 3: Random Forest (FINAL MODEL) ----

rf_fit_fun <- function(train_data) {
  randomForest(
    Earnings ~ GPA + log_connections + GPA_Connections +
      Major + Graduation_Year + Height +
      Number_Of_Credits + Number_Of_Parking_Tickets,
    data = train_data,
    ntree = 300,
    mtry = 3,
    nodesize = 10,
    importance = TRUE
  )
}

rf_pred_fun <- function(model, newdata) {
  predict(model, newdata = newdata)
}

cv_rf <- cv_mse_generic(rf_fit_fun, rf_pred_fun, earn_model, fold_id)

cat("Random Forest - 5-fold CV MSEs:\n")
print(cv_rf$fold_mse)
cat("Random Forest - Mean CV MSE:", cv_rf$mean_mse, "\n")
cat("Random Forest - Mean CV RMSE:", cv_rf$mean_rmse, "\n\n")

# ---- 9. Compare models ----

cat("=== CV Summary ===\n")
cat("Baseline (mean) - MSE:", baseline_mse,
    "  RMSE:", baseline_rmse, "\n")
cat("Linear Model    - Mean MSE:", cv_lm$mean_mse,
    "  Mean RMSE:", cv_lm$mean_rmse, "\n")
cat("rpart Tree      - Mean MSE:", cv_rpart$mean_mse,
    "  Mean RMSE:", cv_rpart$mean_rmse, "\n")
cat("Random Forest   - Mean MSE:", cv_rf$mean_mse,
    "  Mean RMSE:", cv_rf$mean_rmse, "\n\n")

# ---- 10. Fit final Random Forest on ALL training data ----

final_model <- rf_fit_fun(earn_model)

final_train_pred <- rf_pred_fun(final_model, earn_model)
final_train_mse  <- mean((earn_model$Earnings - final_train_pred)^2)
final_train_rmse <- sqrt(final_train_mse)

cat("Final Random Forest - Train MSE:", final_train_mse, "\n")
cat("Final Random Forest - Train RMSE:", final_train_rmse, "\n\n")

# ---- 11. Variable importance ----

cat("Random Forest variable importance:\n")
print(importance(final_model))
varImpPlot(final_model)

# ---- 12. submission.csv creation at the requested path ----

# If a real test file exists, use it; otherwise fall back to training predictions.
if (file.exists(TEST_PATH)) {
  cat("Test file found. Creating submission from test predictions.\n")
  
  test <- read.csv(TEST_PATH)
  test$Major <- factor(test$Major, levels = levels(earn_model$Major))
  
  test_fe <- test %>%
    mutate(
      GPA_Connections = GPA * Number_Of_Professional_Connections,
      log_connections = log1p(Number_Of_Professional_Connections)
    )
  
  pred_vec <- rf_pred_fun(final_model, test_fe)
  n_rows   <- nrow(test_fe)
  
} else {
  cat("Test file NOT found. Creating submission from training predictions instead.\n")
  
  pred_vec <- rf_pred_fun(final_model, earn_model)
  n_rows   <- nrow(earn_model)
}

submission <- data.frame(
  ID = seq_len(n_rows),
  Earnings = pred_vec
)

# Write to the exact folder you specified
SUBMISSION_PATH <- "C:/Users/cjfis/OneDrive/Documents/Data 101/Prediction Challenge 2/submission.csv"

write.csv(submission, SUBMISSION_PATH, row.names = FALSE)
cat("submission.csv written to:\n  ", SUBMISSION_PATH, "\n")

############################################################
