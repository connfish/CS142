set.seed(101)

# 1. Load data ------------------------------------------------------------

df <- read.csv("assessment_generated.csv")

# Drop any identifying columns if present
id_cols <- c("timestamp", "netid", "ruid")
id_cols <- intersect(id_cols, names(df))
df[id_cols] <- list(NULL)

# 2. Define column groups for indices (ADJUST as needed) ------------------

theory_cols  <- c("data_structures", "algorithms", "complexity_measures")
ml_cols      <- c("probability_and_statistics", "regression",
                  "data_visualization", "visualization_tools")
systems_cols <- c("shell_scripting", "sql", "massive_data_processing")
python_cols  <- c("python_libraries", "python_scripting", "jupyter_notebook")

row_mean <- function(cols) {
  rowMeans(df[, cols, drop = FALSE], na.rm = TRUE)
}

# 3. Construct and scale latent indices -----------------------------------

df$Theory_Index   <- as.numeric(scale(row_mean(theory_cols)))
df$ML_Index       <- as.numeric(scale(row_mean(ml_cols)))
df$Systems_Index  <- as.numeric(scale(row_mean(systems_cols)))
df$Python_Index   <- as.numeric(scale(row_mean(python_cols)))

# 4. Base score: AI-resilient skills matter most --------------------------

base_score <-
  0.7 * df$Theory_Index +
  0.7 * df$ML_Index +
  0.6 * df$Systems_Index +
  0.3 * df$Python_Index

# 5. Role & major bonuses -------------------------------------------------

role_bonus <- ifelse(df$role == "Senior", 0.7,
              ifelse(df$role == "Junior", 0.3, 0.0))

major_bonus <- ifelse(df$major == "Computer Science", 0.4,
                ifelse(df$major == "Data Science", 0.5, 0.0))

# 6. AI displacement penalty ----------------------------------------------

ai_penalty <- ifelse(
  df$Python_Index > 0.7 &
    df$Theory_Index < -0.3 &
    df$Systems_Index < -0.3,
  0.8, 0.0
)

# 7. Resilience exceptions -------------------------------------------------

high_stats_cutoff <- quantile(df$probability_and_statistics,
                              0.85, na.rm = TRUE)

exception_bonus <- ifelse(
  df$probability_and_statistics > high_stats_cutoff,
  0.6, 0.0
)

exception_bonus <- exception_bonus + ifelse(
  df$major == "Mathematics" &
    df$probability_and_statistics > high_stats_cutoff,
  0.5, 0.0
)

# 8. Final score, probability, and binary got_job -------------------------

score <- base_score +
         role_bonus +
         major_bonus -
         ai_penalty +
         exception_bonus +
         rnorm(nrow(df), mean = 0, sd = 0.5)

p_job <- 1 / (1 + exp(-score))
df$got_job <- rbinom(nrow(df), size = 1, prob = p_job)

# 9. Remove indices before sharing the dataset -----------------------------

df$Theory_Index   <- NULL
df$ML_Index       <- NULL
df$Systems_Index  <- NULL
df$Python_Index   <- NULL

# 10. Train / test split --------------------------------------------------

set.seed(202)
n <- nrow(df)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

train <- df[train_idx, ]
test  <- df[-train_idx, ]

# For the public challenge, you would give:
# - train: full rows (including got_job)
# - test: features only (no got_job)
# - solution file: got_job for the test set only
write.csv(train, "pc_train.csv", row.names = FALSE)
write.csv(test[, setdiff(names(test), "got_job")],
          "pc_test.csv", row.names = FALSE)
write.csv(test[, c("got_job")],
          "pc_test_solution.csv", row.names = FALSE)
