## =========================
## Data 101 HW — Mountain Resorts (Epic vs Ikon)
## One script for all 4 sections (base R only)
## =========================

## --- Load and create easy, syntactic aliases for the columns we need
raw <- read.csv("2024_epic_ikon_resorts_stats.csv", check.names = FALSE)

df <- within(raw, {
  pass    <- `2024-2025_pass`
  len2324 <- `2023-2024_length_days`
  snow_in <- `historical_average_snowfall_inches`
  top_ft  <- `top_elevation_ft`
})

## Utility: sample SD using only base arithmetic (avoids sd())
sample_sd <- function(x){
  x <- x[!is.na(x)]
  n <- length(x); m <- mean(x)
  sqrt(mean((x - m)^2) * n/(n-1))
}

cat("\n==============================\n(1) Hypothesis Testing\nIkon vs Epic on 2023–2024 season length (days)\n==============================\n")

## --- Filter to Epic/Ikon and drop NA outcome
d1 <- subset(df, pass %in% c("Epic","Ikon") & !is.na(len2324), select = c(pass, len2324))

## Group means and sizes (allowed: tapply, mean, nrow)
g_means <- tapply(d1$len2324, d1$pass, mean)
g_ns    <- tapply(d1$len2324, d1$pass, function(z) nrow(as.data.frame(z[!is.na(z)])))
overall_mean <- mean(d1$len2324)

## Split vectors
ikon <- subset(d1, pass=="Ikon")$len2324
epic <- subset(d1, pass=="Epic")$len2324
m1 <- mean(ikon); n1 <- nrow(as.data.frame(ikon))
m2 <- mean(epic); n2 <- nrow(as.data.frame(epic))
s1 <- sample_sd(ikon)
s2 <- sample_sd(epic)

## z-test for difference in means (two-sided)
se   <- sqrt((s1^2)/n1 + (s2^2)/n2)
zval <- (m1 - m2) / se
p_z  <- 2 * pnorm(-abs(zval))

diff_days <- m1 - m2
diff_pct  <- 100 * diff_days / overall_mean

## Permutation test (label shuffling, ≥ 2000)
set.seed(42)
B <- 2000
labs <- d1$pass
x    <- d1$len2324
perm_diffs <- replicate(B, {
  shuf <- sample(labs)
  mean(x[shuf=="Ikon"]) - mean(x[shuf=="Epic"])
})
p_perm <- mean(abs(perm_diffs) >= abs(diff_days))

## Print results
cat("Group means (days):\n")
print(g_means)
cat("Group sizes:\n")
print(g_ns)
cat(sprintf("\nObserved diff (Ikon - Epic): %.2f days (%.1f%% of overall mean)\n", diff_days, diff_pct))
cat(sprintf("z = %.3f, p(z) = %.4g\n", zval, p_z))
cat(sprintf("Permutation p = %.4g  (B = %d)\n", p_perm, B))

## Headline helper (you’ll paste the numbers into your write-up)
if (p_z < 0.05) {
  cat(sprintf("\nHeadline suggestion: 'Ikon outlasted Epic by %.1f days in 2023–24 (p = %.3g; perm p = %.3g)'\n",
              diff_days, p_z, p_perm))
} else {
  cat(sprintf("\nHeadline suggestion: 'No clear season-length edge: Ikon vs Epic differed by %.1f days (p = %.3g)'\n",
              diff_days, p_z))
}

## =========================
cat("\n==============================\n(2) Confidence Interval (95%%) for mean historical snowfall (in)\n==============================\n")

x2 <- df$snow_in
x2 <- x2[!is.na(x2)]
n  <- length(x2)
m  <- mean(x2)
s  <- sample_sd(x2)
z  <- qnorm(0.975)
moe <- z * s / sqrt(n)
lo  <- m - moe
hi  <- m + moe

cat(sprintf("n = %d, mean = %.2f in, sd = %.2f in\n", n, m, s))
cat(sprintf("95%% CI: [%.2f, %.2f] inches\n", lo, hi))

## Headline helper
band <- hi - lo
cat(sprintf("Headline suggestion: 'Average snowfall ≈ %.1f in (95%% CI [%.1f, %.1f], width ≈ %.1f in)'\n",
            m, lo, hi, band))

## =========================
cat("\n==============================\n(3) Chi-Square Test of Independence: HighSnow × LongSeason (median splits)\n==============================\n")

## Define binary categories by medians (na.rm = TRUE)
hs_med <- median(df$snow_in, na.rm = TRUE)
ls_med <- median(df$len2324, na.rm = TRUE)

HighSnow  <- as.integer(df$snow_in  >= hs_med)
LongSeason<- as.integer(df$len2324 >= ls_med)

## Remove rows with missing bins
keep <- !is.na(HighSnow) & !is.na(LongSeason)
tab <- table(HighSnow[keep], LongSeason[keep])
print(tab)

chi <- chisq.test(tab, correct = FALSE)
cat(sprintf("Chi-square = %.3f, df = %d, p = %.4g\n",
            unname(chi$statistic), unname(chi$parameter), unname(chi$p.value)))

## Headline helper
if (chi$p.value < 0.05) {
  cat("Headline suggestion: 'Deeper snow is linked to longer seasons (χ² significant)'\n")
} else {
  cat("Headline suggestion: 'No clear association: HighSnow vs LongSeason (χ² not significant)'\n")
}

## =========================
cat("\n==============================\n(4) Bayesian Reasoning (Odds form)\nH: LongSeason (top quartile Q3 of len2324)\nE: HighSnow (top quartile Q3 of snowfall)\n==============================\n")

## Define H and E by Q3 thresholds
q_len  <- quantile(df$len2324, probs = 0.75, na.rm = TRUE)
q_snow <- quantile(df$snow_in,  probs = 0.75, na.rm = TRUE)

H <- as.integer(df$len2324 >= q_len)
E <- as.integer(df$snow_in  >= q_snow)

ok <- !is.na(H) & !is.na(E)
H <- H[ok]; E <- E[ok]

## Prior odds = P(H)/P(~H)
pH <- mean(H)
prior_odds <- pH/(1 - pH)

## Likelihood ratio = P(E|H) / P(E|~H)
pE_given_H   <- mean(E[H==1])
pE_given_not <- mean(E[H==0])
LR <- pE_given_H / pE_given_not

## Posterior odds and prob
post_odds <- prior_odds * LR
post_prob <- post_odds / (1 + post_odds)

cat(sprintf("Prior odds (LongSeason): %.3f\n", prior_odds))
cat(sprintf("Likelihood ratio (HighSnow): %.3f\n", LR))
cat(sprintf("Posterior odds: %.3f  ->  Posterior probability: %.3f\n", post_odds, post_prob))

## Headline helper
if (LR > 1) {
  cat(sprintf("Headline suggestion: 'High snowfall multiplies the odds of an extra-long season by ~%.2f×'\n", LR))
} else {
  cat("Headline suggestion: 'High snowfall does not increase odds of an extra-long season'\n")
}

cat("\n===== End of script =====\n")

