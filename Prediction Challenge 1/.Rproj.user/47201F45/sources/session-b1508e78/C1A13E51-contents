#prediction challenge 1

suppressPackageStartupMessages({
  library(glmnet)
  library(Matrix)
})

set.seed(42)

# ------------ Config ------------
TRAIN_PATH <- "CarsTrainNew.csv"
TEST_PATH  <- "CarsTestNew+Truncated.csv"
RARE_MIN_COUNT    <- 20    # rare-level lumping threshold
TE_SMOOTH_ALPHA   <- 5.0   # stronger smoothing for target encoding
NFOLDS_CV         <- 5     # more stable than 3-fold

# ------------ Load ------------
train <- read.csv(TRAIN_PATH, stringsAsFactors = FALSE)
test  <- read.csv(TEST_PATH,  stringsAsFactors = FALSE)

# ------------ Basic types ------------
y <- factor(train$Deal, levels = c("Average","Bad","Good"))

num_cols <- c("Price","Mileage","ValueBenchmark")
for (c in num_cols) {
  train[[c]] <- suppressWarnings(as.numeric(train[[c]]))
  test[[c]]  <- suppressWarnings(as.numeric(test[[c]]))
}

cat_cols <- c("Make","Model","Location")
for (c in cat_cols) {
  train[[c]] <- as.factor(train[[c]])
  test[[c]]  <- as.factor(test[[c]])
}

# Lump rare levels (helps TE & pairs)
lump_rare <- function(f, min_count) {
  tab  <- table(f)
  keep <- names(tab)[tab >= min_count]
  g <- as.character(f)
  g[!(g %in% keep)] <- "OTHER"
  factor(g, levels = c(sort(keep), "OTHER"))
}

train$Make  <- lump_rare(train$Make,  RARE_MIN_COUNT)
train$Model <- lump_rare(train$Model, RARE_MIN_COUNT)

# align test to train (and guarantee "OTHER" level)
test$Make  <- factor(as.character(test$Make),
                     levels = union(levels(train$Make), "OTHER"))
test$Model <- factor(as.character(test$Model),
                     levels = union(levels(train$Model), "OTHER"))
test$Make[is.na(test$Make)]   <- "OTHER"
test$Model[is.na(test$Model)] <- "OTHER"

# align Location levels (include OTHER for safety)
train$Location <- factor(train$Location,
                         levels = union(levels(train$Location), "OTHER"))
test$Location  <- factor(as.character(test$Location),
                         levels = union(levels(train$Location), "OTHER"))
test$Location[is.na(test$Location)] <- "OTHER"

# ------------ Pair features (very predictive) ------------
train$MakeModel <- interaction(train$Make, train$Model, drop = TRUE)
test$MakeModel  <- interaction(test$Make,  test$Model,  drop = TRUE)
train$MakeLoc   <- interaction(train$Make, train$Location, drop = TRUE)
test$MakeLoc    <- interaction(test$Make,  test$Location,  drop = TRUE)

# Lump rare pairs to keep size sane
train$MakeModel <- lump_rare(train$MakeModel, RARE_MIN_COUNT)
test$MakeModel  <- factor(as.character(test$MakeModel),
                          levels = union(levels(train$MakeModel), "OTHER"))
test$MakeModel[is.na(test$MakeModel)] <- "OTHER"

train$MakeLoc <- lump_rare(train$MakeLoc, RARE_MIN_COUNT)
test$MakeLoc  <- factor(as.character(test$MakeLoc),
                        levels = union(levels(train$MakeLoc), "OTHER"))
test$MakeLoc[is.na(test$MakeLoc)] <- "OTHER"

# ------------ Numeric features (+ tiny nonlinear bump) ------------
safe_log1p <- function(x) log(pmax(x, 0) + 1)

train$logPrice          <- safe_log1p(train$Price)
train$logMileage        <- safe_log1p(train$Mileage)
train$logValueBenchmark <- safe_log1p(train$ValueBenchmark)

test$logPrice           <- safe_log1p(test$Price)
test$logMileage         <- safe_log1p(test$Mileage)
test$logValueBenchmark  <- safe_log1p(test$ValueBenchmark)

eps <- 1e-6
train$Price_to_Benchmark <- train$Price / (train$ValueBenchmark + eps)
test$Price_to_Benchmark  <-  test$Price / ( test$ValueBenchmark + eps)

train$price_minus_bench  <- train$Price - train$ValueBenchmark
test$price_minus_bench   <-  test$Price -  test$ValueBenchmark

train$log_ratio <- log(train$Price + 1) - log(train$ValueBenchmark + 1)
test$log_ratio  <- log( test$Price + 1) - log( test$ValueBenchmark + 1)

# small nonlinearities (still cheap)
train$Price_to_Benchmark_sq <- train$Price_to_Benchmark^2
test$Price_to_Benchmark_sq  <- test$Price_to_Benchmark^2

train$log_ratio_sq <- train$log_ratio^2
test$log_ratio_sq  <- test$log_ratio^2

# ------------ Leakage-safe OOF Target Encoding ------------
te_oof <- function(f_train, f_test, y, nfolds, alpha, prefix) {
  set.seed(42)
  f_train <- droplevels(f_train)
  # ensure "OTHER" present in test levels to avoid warnings
  f_test  <- factor(as.character(f_test), levels = union(levels(f_train), "OTHER"))
  f_test[is.na(f_test)] <- "OTHER"
  
  n <- length(y)
  folds <- sample(rep(1:nfolds, length.out = n))
  classes <- levels(y); K <- length(classes)
  col_names <- paste0(prefix, "_", classes)
  
  prior <- prop.table(table(y)); prior <- prior[classes]
  
  TE_train <- matrix(NA_real_, nrow = n, ncol = K)
  colnames(TE_train) <- col_names
  
  full_tab   <- table(f_train, y)
  full_tab   <- full_tab[, classes, drop = FALSE]
  lvl_counts <- rowSums(full_tab)
  full_TE <- sapply(classes, function(cl) {
    num <- full_tab[, cl] + alpha * prior[cl]
    den <- lvl_counts + alpha
    as.numeric(num / den)
  })
  colnames(full_TE) <- col_names
  full_df <- as.data.frame(full_TE)
  full_df$level <- rownames(full_df)
  rownames(full_df) <- full_df$level
  full_df$level <- NULL
  
  for (k in 1:nfolds) {
    tr_idx <- which(folds != k)
    va_idx <- which(folds == k)
    f_tr <- f_train[tr_idx]
    y_tr <- y[tr_idx]
    f_va <- f_train[va_idx]
    
    tab    <- table(f_tr, y_tr)
    tab    <- tab[, classes, drop = FALSE]
    lvl_cnt <- rowSums(tab)
    enc_k <- sapply(classes, function(cl) {
      num <- tab[, cl] + alpha * prior[cl]
      den <- lvl_cnt + alpha
      as.numeric(num / den)
    })
    colnames(enc_k) <- col_names
    enc_df <- as.data.frame(enc_k)
    enc_df$level <- rownames(enc_df)
    rownames(enc_df) <- enc_df$level
    enc_df$level <- NULL
    
    TE_va <- matrix(rep(as.numeric(prior), each = length(va_idx)),
                    nrow = length(va_idx), ncol = K, byrow = FALSE)
    colnames(TE_va) <- col_names
    lvls <- as.character(f_va)
    seen <- lvls %in% rownames(enc_df)
    if (any(seen)) {
      TE_va[seen, ] <- as.matrix(enc_df[lvls[seen], , drop = FALSE])
    }
    TE_train[va_idx, ] <- TE_va
  }
  
  TE_test <- matrix(rep(as.numeric(prior), each = length(f_test)),
                    nrow = length(f_test), ncol = K, byrow = FALSE)
  colnames(TE_test) <- col_names
  lvls_t <- as.character(f_test)
  seen_t <- lvls_t %in% rownames(full_df)
  if (any(seen_t)) {
    TE_test[seen_t, ] <- as.matrix(full_df[lvls_t[seen_t], , drop = FALSE])
  }
  
  list(train = TE_train, test = TE_test)
}

# Build TE for base and pair features
te_make      <- te_oof(train$Make,      test$Make,      y, NFOLDS_CV, TE_SMOOTH_ALPHA, "Make")
te_model     <- te_oof(train$Model,     test$Model,     y, NFOLDS_CV, TE_SMOOTH_ALPHA, "Model")
te_loc       <- te_oof(train$Location,  test$Location,  y, NFOLDS_CV, TE_SMOOTH_ALPHA, "Loc")
te_makemodel <- te_oof(train$MakeModel, test$MakeModel, y, NFOLDS_CV, TE_SMOOTH_ALPHA, "MakeModel")
te_makeloc   <- te_oof(train$MakeLoc,   test$MakeLoc,   y, NFOLDS_CV, TE_SMOOTH_ALPHA, "MakeLoc")

# ------------ Assemble numeric-only matrices ------------
num_feats <- c(
  "Price","Mileage","ValueBenchmark",
  "logPrice","logMileage","logValueBenchmark",
  "Price_to_Benchmark","price_minus_bench","log_ratio",
  "Price_to_Benchmark_sq","log_ratio_sq"
)

TrainNum <- as.matrix(train[, num_feats])
TestNum  <- as.matrix(test[,  num_feats])

X  <- cbind(
  TrainNum,
  te_make$train, te_model$train, te_loc$train,
  te_makemodel$train, te_makeloc$train
)

Xt <- cbind(
  TestNum,
  te_make$test, te_model$test, te_loc$test,
  te_makemodel$test, te_makeloc$test
)

X  <- as(Matrix(X,  sparse = TRUE), "dgCMatrix")
Xt <- as(Matrix(Xt, sparse = TRUE), "dgCMatrix")

keep <- complete.cases(as.matrix(X), y)
X <- X[keep, ]
y <- y[keep]

# ------------ cv.glmnet — wider alpha sweep ------------
alphas <- c(0.00, 0.25, 0.50, 0.75, 1.00)

run_cv <- function(alpha_val) {
  set.seed(42)
  cvfit <- cv.glmnet(
    x = X, y = y,
    family = "multinomial",
    type.measure = "class",
    nfolds = NFOLDS_CV,
    alpha = alpha_val,
    standardize = TRUE
  )
  lam <- cvfit$lambda.min
  # cvm is mean loss (misclassification rate when type.measure="class")
  acc <- 1 - cvfit$cvm[which(cvfit$lambda == lam)]
  list(cvfit = cvfit, lambda = lam, acc = as.numeric(acc), alpha = alpha_val)
}

results <- lapply(alphas, run_cv)
best <- results[[which.max(vapply(results, function(r) r$acc, numeric(1)))]]

cat(sprintf(
  "\nSelected alpha = %.2f | %d-fold CV Accuracy (lambda.min) ≈ %.4f\n",
  best$alpha, NFOLDS_CV, best$acc
))

# ------------ Predict & write ------------
pred_test <- predict(best$cvfit, newx = Xt, s = best$lambda, type = "class")
pred_test <- factor(pred_test, levels = levels(y))

submission <- if (!"id" %in% names(test)) {
  data.frame(id = seq_len(nrow(test)), Deal = pred_test)
} else {
  data.frame(id = test$id, Deal = pred_test)
}

write.csv(submission, "submission.csv", row.names = FALSE)
cat("\nWrote submission.csv (id, Deal)\n")

# ------------ Quick sanity ------------
cat("\nTrain class distribution:\n")
print(prop.table(table(y)))

cat("\nTest prediction distribution:\n")
print(prop.table(table(submission$Deal)))
