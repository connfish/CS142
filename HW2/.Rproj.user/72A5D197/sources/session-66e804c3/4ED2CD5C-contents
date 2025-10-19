# 0) Read cleanly and standardize names we’ll use
raw <- read.csv("2024_epic_ikon_resorts_stats.csv", check.names = FALSE)
# Create easy, syntactic aliases so we don't have to keep using backticks
df <- within(raw, {
  pass     <- `2024-2025_pass`
  len2324  <- `2023-2024_length_days`
})

# 1) Filter to Epic/Ikon and drop NAs on the outcome
d <- subset(df, pass %in% c("Epic","Ikon") & !is.na(len2324), select = c(pass, len2324))

# 2) Descriptives
g_means <- tapply(d$len2324, d$pass, mean)
g_ns    <- tapply(d$len2324, d$pass, function(z) nrow(as.data.frame(z[!is.na(z)])))
overall_mean <- mean(d$len2324)

# 3) z-test (manual, base ops only)
ikon <- subset(d, pass=="Ikon")$len2324
epic <- subset(d, pass=="Epic")$len2324

m1 <- mean(ikon); n1 <- nrow(as.data.frame(ikon))
m2 <- mean(epic); n2 <- nrow(as.data.frame(epic))

# sample sd without sd()
s1 <- sqrt(mean((ikon - m1)^2) * n1/(n1-1))
s2 <- sqrt(mean((epic - m2)^2) * n2/(n2-1))

se   <- sqrt((s1^2)/n1 + (s2^2)/n2)
zval <- (m1 - m2) / se
p_z  <- 2 * pnorm(-abs(zval))

diff_days <- m1 - m2
diff_pct  <- 100 * diff_days / overall_mean

# 4) Permutation test (≥ 1000 shuffles)
set.seed(42)
B <- 2000
obs <- diff_days
labs <- d$pass
x    <- d$len2324
perm_diffs <- replicate(B, {
  shuf <- sample(labs)
  mean(x[shuf=="Ikon"]) - mean(x[shuf=="Epic"])
})
p_perm <- mean(abs(perm_diffs) >= abs(obs))

# 5) Quick printout
cat("\nIkon mean:", m1, " Epic mean:", m2, 
    "\nDiff (days):", diff_days, sprintf("(%.1f%% of overall mean)", diff_pct),
    "\nZ:", zval, " p(z):", p_z, 
    "\nPermutation p:", p_perm, "\n")
