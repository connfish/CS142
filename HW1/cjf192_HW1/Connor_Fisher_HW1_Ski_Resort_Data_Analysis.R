# Homework 1 — European Ski Resorts 


# 0) Load data
getwd()              # sanity check
list.files() 
data <- read.csv("European_Ski_Resorts.csv")

# Quick dataset checks
nrow(data)
min(data$DayPassPriceAdult, na.rm=TRUE)
max(data$DayPassPriceAdult, na.rm=TRUE)
mean(data$DayPassPriceAdult, na.rm=TRUE)

# Simple color helpers (base R only)
cols3  <- c("#4E79A7", "#F28E2B", "#E15759")
cols4  <- c("#4E79A7", "#59A14F", "#F28E2B", "#E15759")
cols6  <- c("#4E79A7","#59A14F","#F28E2B","#E15759","#B07AA1","#9C755F")
cols10 <- c("#4E79A7","#59A14F","#F28E2B","#E15759","#B07AA1","#9C755F","#76B7B2","#EDC948","#AF7AA1","#FF9DA7")

# 1) Mean Price by Ski Area Size
small  <- subset(data, TotalSlope <= 60)
medium <- subset(data, TotalSlope > 60 & TotalSlope <= 150)
large  <- subset(data, TotalSlope > 150)

means_size <- c(mean(small$DayPassPriceAdult, na.rm=TRUE),
                mean(medium$DayPassPriceAdult, na.rm=TRUE),
                mean(large$DayPassPriceAdult, na.rm=TRUE))
barplot(means_size,
        names.arg=c("Small (<=60km)","Medium (61-150km)","Large (>150km)"),
        main="Mean Price by Ski Area Size", ylab="Mean DayPass (EUR)",
        col=cols3)

# 2) Altitude Premium
lowH  <- subset(data, HighestPoint <= 1600)
midH  <- subset(data, HighestPoint > 1600 & HighestPoint <= 2400)
highH <- subset(data, HighestPoint > 2400)

means_alt <- c(mean(lowH$DayPassPriceAdult, na.rm=TRUE),
               mean(midH$DayPassPriceAdult, na.rm=TRUE),
               mean(highH$DayPassPriceAdult, na.rm=TRUE))
barplot(means_alt,
        names.arg=c("Low (<=1600m)","Mid (1601-2400m)","High (>2400m)"),
        main="Altitude Premium: Price by Highest Point", ylab="Mean DayPass (EUR)",
        col=cols3)

# 3) Price vs Night Skiing
mean_price_night <- tapply(data$DayPassPriceAdult, data$NightSki, mean, na.rm=TRUE)
barplot(mean_price_night,
        main="Price vs Night Skiing", ylab="Mean Ticket Price (EUR)",
        col=cols4[1:length(mean_price_night)], ylim=c(0,50))

# 4) Lift Capacity vs Price
lowC  <- subset(data, LiftCapacity <= 15000)
midC  <- subset(data, LiftCapacity > 15000 & LiftCapacity <= 35000)
highC <- subset(data, LiftCapacity > 35000)

means_cap <- c(mean(lowC$DayPassPriceAdult, na.rm=TRUE),
               mean(midC$DayPassPriceAdult, na.rm=TRUE),
               mean(highC$DayPassPriceAdult, na.rm=TRUE))
barplot(means_cap,
        names.arg=c("LowCap (<=15k)","MidCap (15-35k)","HighCap (>35k)"),
        main="Lift Capacity vs Price", ylab="Mean DayPass (EUR)",
        col=cols3)

# 5) Scatter: Price vs Highest Point with line of fit
plot(data$HighestPoint, data$DayPassPriceAdult,
     main="Price vs Highest Point", xlab="Highest Point (m)", ylab="DayPass (EUR)",
     col="#4E79A7", pch=19, ylim=c(0,50))
fit <- lm(DayPassPriceAdult ~ HighestPoint, data=data)
abline(fit, col="red", lwd=2)

# 6) Mean Price by Country (Top 10 by mean)
mean_price_by_country <- tapply(data$DayPassPriceAdult, data$Country, mean, na.rm=TRUE)
mean_price_by_country <- sort(mean_price_by_country, decreasing=TRUE)
top10 <- head(mean_price_by_country, 10)
barplot(top10, las=2, main="Top 10 Countries by Mean DayPass Price",
        ylab="Mean DayPass (EUR)", col=cols10)

# 7) Mean Resort Highest Point (Top 6 Countries by Count)
counts_by_country <- sort(table(data$Country), decreasing=TRUE)
top6 <- names(counts_by_country)[1:6]

mean_height_top6 <- tapply(
  subset(data, Country %in% top6)$HighestPoint,
  subset(data, Country %in% top6)$Country,
  mean, na.rm=TRUE
)
mean_height_top6 <- sort(mean_height_top6, decreasing=TRUE)

barplot(mean_height_top6,
        las=2,
        main="Mean Resort Highest Point (Top 6 Countries by Count)",
        ylab="Mean Highest Point (m)",
        col=cols6,
        ylim=c(0, max(mean_height_top6, na.rm=TRUE) * 1.15))
#Mosaic plot
# Define Price bands 
data$PriceBand <- "Mid" 
data$PriceBand[data$DayPassPriceAdult <= 30] <- "Low" 
data$PriceBand[data$DayPassPriceAdult > 50] <- "High"
# Define slope size bands 
data$SlopeSize <- "Medium" 
data$SlopeSize[data$TotalSlope <= 60] <- "Small" 
data$SlopeSize[data$TotalSlope > 150] <- "Large" 
tab_slope_price <- table(data$SlopeSize, data$PriceBand) 
mosaicplot(tab_slope_price, main="Ski Area Size vs Price Band", 
           xlab="Slope Size", ylab="Price Band", color=TRUE)