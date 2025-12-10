# ---------------------------------------------------------
# Prediction Challenge 3 - Who will be hired?
# Ensemble of unbalanced + balanced RF with CV-tuned alpha and threshold
# ---------------------------------------------------------

setwd("~/Data 101/Prediction Challenge 3")

library(randomForest)

# 1. Load data -------------------------------------------------------------
train <- read.csv("Prediction3Train.csv")
test  <- read.csv("Prediction3Test-truncated.csv")

# 2. Clean / harmonize categorical variables -------------------------------
# Convert to character first
train$Major      <- as.character(train$Major)
train$University <- as.character(train$University)

test$Major      <- as.character(test$Major)
test$University <- as.character(test$University)

# Replace NA with explicit "Unknown" categories
train$Major[is.na(train$Major)]        <- "UnknownMajor"
test$Major[is.na(test$Major)]          <- "UnknownMajor"

train$University[is.na(train$University)] <- "UnknownUni"
test$University[is.na(test$University)]   <- "UnknownUni"

# Make sure train and test share the SAME levels
all_majors <- sort(unique(c(train$Major, test$Major)))
all_unis   <- sort(unique(c(train$University, test$University)))

train$Major      <- factor(train$Major,      levels = all_majors)
test$Major       <- factor(test$Major,       levels = all_majors)

train$University <- factor(train$University, levels = all_unis)
test$University  <- factor(test$University,  levels = all_unis)

# 3. Handle GPA missing values (if any) ------------------------------------
gpa_mean <- mean(train$GPA, na.rm = TRUE)
train$GPA[is.na(train$GPA)] <- gpa_mean
test$GPA[is.na(test$GPA)]   <- gpa_mean

# 4. Target as factor / numeric --------------------------------------------
train$Hired <- factor(train$Hired, levels = c(0, 1))
y_factor <- train$Hired
y_num    <- as.numeric(y_factor) - 1  # 0/1 numeric

# 5. One-hot encoding via model.matrix -------------------------------------
X_train <- model.matrix(~ Major + GPA + University, data = train)[, -1]
X_test  <- model.matrix(~ Major + GPA + University, data = test)[, -1]

cat("nrow(train) =", nrow(train), "  nrow(X_train) =", nrow(X_train), "\n")
cat("nrow(test)  =", nrow(test),  "  nrow(X_test)  =", nrow(X_test),  "\n")

# 6. 5-fold CV for ensemble of unbalanced + balanced RF --------------------
set.seed(202)

n <- nrow(X_train)
K <- 5
fold_ids <- sample(rep(1:K, length.out = n))

cv_unbal <- rep(NA_real_, n)  # RF unbalanced probabilities
cv_bal   <- rep(NA_real_, n)  # RF balanced probabilities

for (k in 1:K) {
  cat("Training fold", k, "of", K, "...\n")
  
  train_idx <- which(fold_ids != k)
  val_idx   <- which(fold_ids == k)
  
  y_train_fold <- y_factor[train_idx]
  
  # --- Unbalanced RF on this fold ---
  rf_unbal_k <- randomForest(
    x          = X_train[train_idx, ],
    y          = y_train_fold,
    ntree      = 400,
    mtry       = floor(sqrt(ncol(X_train))),
    importance = FALSE
  )
  
  # --- Balanced RF on this fold ---
  # Balance classes in the bootstrap sample for each tree
  tab_fold <- table(y_train_fold)
  min_class_size_fold <- min(tab_fold)
  
  rf_bal_k <- randomForest(
    x          = X_train[train_idx, ],
    y          = y_train_fold,
    ntree      = 400,
    mtry       = floor(sqrt(ncol(X_train))),
    strata     = y_train_fold,
    sampsize   = rep(min_class_size_fold, 2),
    importance = FALSE
  )
  
  # Store validation probs
  cv_unbal[val_idx] <- predict(rf_unbal_k, newdata = X_train[val_idx, ], type = "prob")[, "1"]
  cv_bal[val_idx]   <- predict(rf_bal_k,   newdata = X_train[val_idx, ], type = "prob")[, "1"]
}

cat("Any NA in cv_unbal? ", any(is.na(cv_unbal)), "\n")
cat("Any NA in cv_bal?   ", any(is.na(cv_bal)), "\n")

# 7. Grid search over alpha + threshold ------------------------------------
alphas <- seq(0, 1, by = 0.25)       # weight on unbalanced RF
thresholds <- seq(0.30, 0.70, by = 0.01)

best_acc <- -Inf
best_alpha <- 0.5
best_thresh <- 0.5

for (a in alphas) {
  p_ens_cv <- a * cv_unbal + (1 - a) * cv_bal
  
  for (t in thresholds) {
    preds_t <- ifelse(p_ens_cv >= t, 1, 0)
    acc <- mean(preds_t == y_num)
    
    if (acc > best_acc) {
      best_acc   <- acc
      best_alpha <- a
      best_thresh <- t
    }
  }
}

cat(sprintf("\nBest CV alpha = %.2f, threshold = %.3f, accuracy = %.4f\n",
            best_alpha, best_thresh, best_acc))

# Confusion at best combo
p_ens_cv_best <- best_alpha * cv_unbal + (1 - best_alpha) * cv_bal
preds_cv_best <- ifelse(p_ens_cv_best >= best_thresh, 1, 0)

cat("\nCV confusion at best alpha/threshold:\n")
print(table(Predicted = preds_cv_best, Actual = y_num))

# 8. Train final unbalanced + balanced RF on full data ---------------------
set.seed(42)

# Unbalanced RF
rf_unbal_final <- randomForest(
  x          = X_train,
  y          = y_factor,
  ntree      = 800,
  mtry       = floor(sqrt(ncol(X_train))),
  importance = TRUE
)

# Balanced RF
tab_full <- table(y_factor)
min_class_size_full <- min(tab_full)

rf_bal_final <- randomForest(
  x          = X_train,
  y          = y_factor,
  ntree      = 800,
  mtry       = floor(sqrt(ncol(X_train))),
  strata     = y_factor,
  sampsize   = rep(min_class_size_full, 2),
  importance = TRUE
)

cat("\nFinal unbalanced RF OOB confusion:\n")
print(rf_unbal_final$confusion)
cat("\nFinal balanced RF OOB confusion:\n")
print(rf_bal_final$confusion)

# 9. Predict on test set using best alpha + threshold ----------------------
p_unbal_test <- predict(rf_unbal_final, newdata = X_test, type = "prob")[, "1"]
p_bal_test   <- predict(rf_bal_final,   newdata = X_test, type = "prob")[, "1"]

p_ens_test <- best_alpha * p_unbal_test + (1 - best_alpha) * p_bal_test

preds_test <- ifelse(p_ens_test >= best_thresh, 1, 0)

# 10. Build submission file -------------------------------------------------
submission <- data.frame(
  ID    = test$ID,
  Hired = preds_test
)

write.csv(submission, "prediction3_submission_rf_ensemble_cv.csv", row.names = FALSE)
cat("\nSaved file: prediction3_submission_rf_ensemble_cv.csv\n")
