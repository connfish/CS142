

library(readxl)
library(rpart)
library(arules)

set.seed(2025)
setwd("C:/Users/cjfis/OneDrive/Documents/Data 101/HW3")

data_path <- "OnTheSnow_v4_SeparateState.xlsx"  # make sure this is in your working directory


snow_raw <- read_excel(data_path, sheet = "sheet1")
snow <- as.data.frame(snow_raw)

cat("=== Basic Structure ===\n")
str(snow)
cat("\n=== First rows ===\n")
print(head(snow))


snow$resort_name <- snow$Name

snow$ticket_price_weekday <- snow$AdultWeekday
snow$ticket_price_weekend <- snow$AdultWeekend

snow$ticket_price <- snow$ticket_price_weekend

snow$snowfall_avg   <- snow$averageSnowfall
snow$elevation_top  <- snow$summit
snow$elevation_base <- snow$base
snow$runs_total     <- snow$Runs
snow$skiable_terrain <- snow$`Skiable Terrain`  #because of space in name

# Some checks
cat("\n=== Summary of key new variables ===\n")
print(summary(snow[, c("ticket_price", "snowfall_avg",
                       "elevation_top", "elevation_base",
                       "runs_total", "skiable_terrain")]))



cat("# 1. Languages of Data     #\n")

cat("Summary of ticket_price (numeric, AdultWeekend):\n")
print(summary(snow$ticket_price))

cat("\nFrequency of state (categorical):\n")
print(table(snow$state))

cat("\nMean ticket_price by state:\n")
print(tapply(snow$ticket_price, snow$state, mean, na.rm = TRUE))

snow$vertical_drop_calc <- snow$elevation_top - snow$elevation_base
snow$vertical_drop <- ifelse(is.na(snow$drop),
                             snow$vertical_drop_calc,
                             snow$drop)

#price per run
snow$price_per_run <- snow$ticket_price / snow$runs_total

cat("\nSummary of new variable: vertical_drop\n")
print(summary(snow$vertical_drop))

cat("\nSummary of new variable: price_per_run\n")
print(summary(snow$price_per_run))

cat("# 2. Exploratory Data Analysis #\n")


hist(snow$ticket_price,
     main = "Distribution of Weekend Lift Ticket Prices",
     xlab = "Weekend Ticket Price ($)")

boxplot(ticket_price ~ state,
        data = snow,
        main = "Weekend Ticket Prices by State",
        xlab = "State",
        ylab = "Ticket Price ($)",
        las = 2)

state_counts <- table(snow$state)
barplot(state_counts,
        main = "Number of Resorts by State",
        xlab = "State",
        las = 2)


cat("# 3. Fooled by Data: Randomness/Shuffling #\n")

valid_idx_cor <- complete.cases(snow$ticket_price, snow$snowfall_avg)
cor_true <- cor(snow$ticket_price[valid_idx_cor],
                snow$snowfall_avg[valid_idx_cor])

cat("True correlation (ticket_price vs snowfall_avg): ", cor_true, "\n")

snow$snowfall_shuffled <- sample(snow$snowfall_avg)

valid_idx_shuf <- complete.cases(snow$ticket_price, snow$snowfall_shuffled)
cor_shuffled <- cor(snow$ticket_price[valid_idx_shuf],
                    snow$snowfall_shuffled[valid_idx_shuf])

cat("Correlation with shuffled snowfall: ", cor_shuffled, "\n")

plot(snow$snowfall_avg, snow$ticket_price,
     main = "Ticket Price vs Average Snowfall",
     xlab = "Average Snowfall (inches / cm)",
     ylab = "Weekend Ticket Price ($)")

plot(snow$snowfall_shuffled, snow$ticket_price,
     main = "Ticket Price vs Shuffled Snowfall",
     xlab = "Shuffled Snowfall",
     ylab = "Weekend Ticket Price ($)")



cat("# 4. CLT & Confidence Intervals            #\n")

valid_prices <- snow$ticket_price[!is.na(snow$ticket_price)]
n_sample <- 40   
B <- 500         
means <- numeric(B)

for (b in 1:B) {
  samp <- sample(valid_prices, n_sample, replace = TRUE)
  means[b] <- mean(samp)
}

hist(means,
     main = "Sampling Distribution of Mean Weekend Ticket Price",
     xlab = "Sample Mean Weekend Ticket Price")

xbar <- mean(valid_prices)
s <- sd(valid_prices)
n_total <- length(valid_prices)

se <- s / sqrt(n_total)
lower <- xbar - 1.96 * se
upper <- xbar + 1.96 * se

cat("Mean weekend ticket price: ", xbar, "\n")
cat("95% CI for mean weekend ticket price: [", lower, ", ", upper, "]\n")





cat("# 5. Hypothesis Testing (Mean)     #\n")

median_snow <- median(snow$snowfall_avg, na.rm = TRUE)
snow$high_snow <- ifelse(snow$snowfall_avg >= median_snow, "High", "Low")

cat("Counts of High vs Low snow resorts:\n")
print(table(snow$high_snow))

tt_idx <- complete.cases(snow$ticket_price, snow$high_snow)
t_res <- t.test(ticket_price ~ high_snow,
                data = snow[tt_idx, ])

print(t_res)

cat("\nInterpretation: H0 = mean weekend ticket_price is the same for high and low snow.\n")
cat("p-value = ", t_res$p.value, "\n")



cat("# 6. Chi-Square: Independence of Categories #\n")

snow$expensive <- ifelse(snow$ticket_price > 90, "Yes", "No")

cat("Contingency table: state x expensive\n")
tab <- table(snow$state, snow$expensive)
print(tab)

chi_res <- chisq.test(tab)
print(chi_res)

cat("\nChi-square p-value = ", chi_res$p.value, "\n")


cat("# 7. Multiple Hypothesis Testing            #\n")

tests <- list(
  cor.test(snow$ticket_price, snow$elevation_top),
  cor.test(snow$ticket_price, snow$snowfall_avg),
  cor.test(snow$ticket_price, snow$skiable_terrain),
  cor.test(snow$ticket_price, snow$vertical_drop)
)

raw_p <- sapply(tests, function(t) t$p.value)

p_bonf <- p.adjust(raw_p, method = "bonferroni")
p_bh   <- p.adjust(raw_p, method = "BH")

mt_results <- data.frame(
  test = c("price~elev_top",
           "price~snowfall",
           "price~skiable_terrain",
           "price~vertical_drop"),
  raw_p = raw_p,
  p_bonf = p_bonf,
  p_bh = p_bh
)

cat("Multiple testing results:\n")
print(mt_results)



cat("# 8. Bayesian Reasoning       #\n")

thr_snow <- quantile(snow$snowfall_avg, 0.75, na.rm = TRUE)
snow$powder_heaven <- snow$snowfall_avg >= thr_snow

thr_elev <- quantile(snow$elevation_top, 0.75, na.rm = TRUE)
snow$high_elev <- snow$elevation_top >= thr_elev

p_powder <- mean(snow$powder_heaven, na.rm = TRUE)
p_high_elev <- mean(snow$high_elev, na.rm = TRUE)
p_high_elev_given_powder <-
  mean(snow$high_elev[snow$powder_heaven], na.rm = TRUE)
p_powder_given_high <-
  p_high_elev_given_powder * p_powder / p_high_elev

cat("P(powder_heaven) = ", p_powder, "\n")
cat("P(powder_heaven | high_elev) = ", p_powder_given_high, "\n")

if (!is.na(p_powder) && !is.na(p_powder_given_high) && p_powder > 0) {
  cat("Posterior is about ",
      round(p_powder_given_high / p_powder, 2),
      " times the prior.\n")
}



cat("# 9. Prediction Models (Linear/Tree)   #\n")

set.seed(2025)
n <- nrow(snow)
train_idx <- sample(1:n, size = floor(0.7 * n))
train <- snow[train_idx, ]
test  <- snow[-train_idx, ]

lm_fit <- lm(ticket_price ~ snowfall_avg + elevation_top +
               skiable_terrain + vertical_drop,
             data = train)

cat("Linear model summary:\n")
print(summary(lm_fit))

pred_train_lm <- predict(lm_fit, train)
pred_test_lm  <- predict(lm_fit, test)

mse_train_lm <- mean((pred_train_lm - train$ticket_price)^2, na.rm = TRUE)
mse_test_lm  <- mean((pred_test_lm  - test$ticket_price)^2,  na.rm = TRUE)

cat("\nLinear model MSE (train): ", mse_train_lm, "\n")
cat("Linear model MSE (test):  ", mse_test_lm,  "\n")

tree_fit <- rpart(ticket_price ~ snowfall_avg + elevation_top +
                    skiable_terrain + vertical_drop,
                  data = train,
                  method = "anova")

cat("\nTree model:\n")
print(tree_fit)

plot(tree_fit)
text(tree_fit, cex = 0.6)

pred_train_tree <- predict(tree_fit, train)
pred_test_tree  <- predict(tree_fit, test)

mse_train_tree <- mean((pred_train_tree - train$ticket_price)^2, na.rm = TRUE)
mse_test_tree  <- mean((pred_test_tree  - test$ticket_price)^2,  na.rm = TRUE)

cat("\nTree MSE (train): ", mse_train_tree, "\n")
cat("Tree MSE (test):  ", mse_test_tree,  "\n")


cat("# 10. Association Rules & Lift (arules)   #\n")

snow$price_cat <- cut(
  snow$ticket_price,
  breaks = quantile(snow$ticket_price,
                    probs = c(0, 0.33, 0.66, 1),
                    na.rm = TRUE),
  labels = c("LowPrice", "MidPrice", "HighPrice"),
  include.lowest = TRUE
)

snow$snow_cat <- cut(
  snow$snowfall_avg,
  breaks = quantile(snow$snowfall_avg,
                    probs = c(0, 0.5, 0.75, 1),
                    na.rm = TRUE),
  labels = c("LowSnow", "MedSnow", "HighSnow"),
  include.lowest = TRUE
)

snow$vert_cat <- cut(
  snow$vertical_drop,
  breaks = quantile(snow$vertical_drop,
                    probs = c(0, 0.5, 0.75, 1),
                    na.rm = TRUE),
  labels = c("LowVert", "MedVert", "HighVert"),
  include.lowest = TRUE
)

snow$state_factor <- as.factor(snow$state)

# Build transactions
trans_cols <- c("price_cat", "snow_cat", "vert_cat", "state_factor")
trans <- as(snow[, trans_cols], "transactions")

cat("Transactions summary:\n")
print(summary(trans))

rules <- apriori(
  trans,
  parameter = list(supp = 0.05, conf = 0.6, minlen = 2)
)

rules_lift <- subset(rules, lift > 1)
rules_sorted <- sort(rules_lift, by = "lift")

cat("\nTop association rules (lift > 1):\n")
inspect(head(rules_sorted, 10))





cat("# 11. Reflection (example text)      #\n")

cat("Reflection 1 (dataset insight):\n")
cat("Working with this snowboarding-adjacent resort dataset changed how I think\n")
cat("about 'expensive' mountains. I expected high prices to be mostly about brand\n")
cat("and location, but my models and plots suggest they track terrain and snow\n")
cat("quality more than I thought. Resorts with big vertical drops and consistently\n")
cat("high snowfall cluster in the high-price range, and the association rules show\n")
cat("a strong 'recipe' of high snow + high vertical -> high ticket prices.\n\n")

cat("Reflection 2 (concept insight):\n")
cat("The concept that surprised me most was multiple hypothesis testing. At first\n")
cat("it felt nit-picky to 'correct' p-values, but when I ran several correlations\n")
cat("at once, I saw how easy it would be to cherry-pick 'significant' results just\n")
cat("by chance. Bonferroni and BH corrections helped me separate stable\n")
cat("relationships from noisy ones, and combined with the randomness shuffle and\n")
cat("CLT simulation, this project showed me how statistics helps protect us from\n")
cat("overinterpreting noise.\n\n")




cat("# 12. Data Tour as GPT Chat (Template)     #\n")


data_tour_prompt <- "
You are a friendly data guide. We are exploring a US ski/snowboard resort dataset
with weekend ticket prices, snowfall, elevation, number of runs, and state.
Start by asking me what the dataset is about, then walk me through:

1) Basic summaries of key variables (ticket_price, snowfall_avg, elevation_top).
2) One visualization (e.g., histogram of ticket_price).
3) A hypothesis test comparing ticket prices for high-snow vs low-snow resorts.
4) A short 3-4 sentence conclusion about what matters most for price.

Explain at a Data 101 level and ask occasional clarification questions.
"

cat("Example GPT Metaprompt for the data tour:\n\n")
cat(data_tour_prompt, "\n")

cat("\n\n=== End of Script ===\n")
