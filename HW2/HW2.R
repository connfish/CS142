
## Data 101 HW — Mountain Resorts (Epic vs Ikon)


setwd("C:/Users/Connor/OneDrive/Documents/Data 101/HW2")

raw <- read.csv("2024_epic_ikon_resorts_stats.csv", check.names = FALSE)


df <- within(raw, {
  pass    <- `2024-2025_pass`                
  len2324 <- `2023-2024_length_days`          
  snow_in <- `historical_average_snowfall_inches`
  top_ft  <- `top_elevation_ft`
  trails  <- trails
  acres   <- skiable_acres
  country <- country
  st_abbr <- state_or_province_abbreviation
})

sample_sd <- function(x){x<-x[!is.na(x)]; n<-length(x); m<-mean(x); sqrt(mean((x-m)^2)*n/(n-1))}


#1
d1 <- subset(df, pass %in% c("Epic","Ikon") & !is.na(len2324), select = c(pass,len2324))
ikon <- subset(d1, pass=="Ikon")$len2324
epic <- subset(d1, pass=="Epic")$len2324
m1 <- mean(ikon); m2 <- mean(epic)
n1 <- nrow(as.data.frame(ikon)); n2 <- nrow(as.data.frame(epic))
s1 <- sample_sd(ikon); s2 <- sample_sd(epic)
se <- sqrt((s1^2)/n1 + (s2^2)/n2)
z  <- (m1-m2)/se
p_z <- 2*pnorm(-abs(z))
overall_mean <- mean(d1$len2324)
diff_days <- m1 - m2
diff_pct  <- 100*diff_days/overall_mean
set.seed(42); B <- 2000
labs <- d1$pass; x <- d1$len2324
perm_diffs <- replicate(B,{sh<-sample(labs); mean(x[sh=="Ikon"])-mean(x[sh=="Epic"])})
p_perm <- mean(abs(perm_diffs) >= abs(diff_days))
cat(sprintf("Ikon mean=%.2f (n=%d) | Epic mean=%.2f (n=%d)\n", m1,n1,m2,n2))
cat(sprintf("Diff=%.2f days (%.1f%% of overall mean %.2f)\n", diff_days, diff_pct, overall_mean))
cat(sprintf("z=%.3f, p=%.4g | permutation p=%.4g (B=%d)\n", z, p_z, p_perm, B))



#2
cat("\n(2) 95% CI for mean snowfall (in)\n")
x2 <- df$snow_in; x2 <- x2[!is.na(x2)]
n <- length(x2); m <- mean(x2); s <- sample_sd(x2)
zcrit <- qnorm(.975); moe <- zcrit*s/sqrt(n)
lo <- m - moe; hi <- m + moe; width <- hi - lo
cat(sprintf("n=%d, mean=%.2f\", sd=%.2f\" | 95%% CI=[%.2f\", %.2f\"], width=%.2f\"\n", n,m,s,lo,hi,width))

#3
cat("\n(3) Chi-Square: pass × region (East/West/International)\n")
d3 <- subset(df, pass %in% c("Epic","Ikon"), select = c(pass,country,st_abbr))

east_states <- c("ME","NH","VT","MA","CT","RI","NY","NJ","PA","MD","DE","DC","VA","WV",
                 "NC","SC","GA","FL","OH","MI","IN","IL","WI","MN","IA","MO","KY","TN")
west_states <- c("WA","OR","CA","AK","HI","ID","MT","WY","UT","CO","NV","AZ","NM")

Region3 <- ifelse(d3$country!="United States","International",
                  ifelse(d3$st_abbr %in% east_states,"East",
                         ifelse(d3$st_abbr %in% west_states,"West","East")))   # fallback keeps 3 groups

tab <- table(d3$pass, Region3)   
print(tab)
chi <- chisq.test(tab, correct=FALSE)
cat(sprintf("Chi-square=%.3f, df=%d, p=%.4g\n", unname(chi$statistic), unname(chi$parameter), unname(chi$p.value)))
prop_by_brand <- prop.table(tab, margin=1)
cat("\nRow-wise proportions (region shares within brand):\n"); print(round(prop_by_brand,3))


#4
cat("\n(4) Bayes: H=LongSeason (Q3); E in {HighSnow (Q3), HighAltitude (Q3)}\n")
q_len  <- quantile(df$len2324, probs=.75, na.rm=TRUE)
q_snow <- quantile(df$snow_in,  probs=.75, na.rm=TRUE)
q_alt  <- quantile(df$top_ft,   probs=.75, na.rm=TRUE)

H <- as.integer(df$len2324 >= q_len)
E_snow <- as.integer(df$snow_in >= q_snow)
E_alt  <- as.integer(df$top_ft  >= q_alt)

ok_snow <- !is.na(H) & !is.na(E_snow)
ok_alt  <- !is.na(H) & !is.na(E_alt)

pH <- mean(H[!is.na(H)]); prior_odds <- pH/(1-pH)

pE_H_snow   <- mean(E_snow[ok_snow][H[ok_snow]==1])
pE_not_snow <- mean(E_snow[ok_snow][H[ok_snow]==0])
LR_snow <- pE_H_snow / pE_not_snow
post_odds_snow <- prior_odds * LR_snow
post_prob_snow <- post_odds_snow / (1 + post_odds_snow)

pE_H_alt   <- mean(E_alt[ok_alt][H[ok_alt]==1])
pE_not_alt <- mean(E_alt[ok_alt][H[ok_alt]==0])
LR_alt <- pE_H_alt / pE_not_alt
post_odds_alt <- prior_odds * LR_alt
post_prob_alt <- post_odds_alt / (1 + post_odds_alt)

cat(sprintf("Prior: P(H)=%.3f, odds=%.3f\n", pH, prior_odds))
cat(sprintf("HighSnow:   LR=%.3f, post_odds=%.3f, post_P=%.3f\n", LR_snow, post_odds_snow, post_prob_snow))
cat(sprintf("HighAltitude: LR=%.3f, post_odds=%.3f, post_P=%.3f\n", LR_alt, post_odds_alt, post_prob_alt))
