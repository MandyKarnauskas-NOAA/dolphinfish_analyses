# M. Karnauskas 09/20/2022
# Compilation of dolphinfish commercial and recreational landings for U.S. EEZ

rm(list = ls())

# download data from NMFS FOSS site -------------------------
# https://www.fisheries.noaa.gov/foss/f?p=215:200:660844904940::NO:RP:: 
# data set --> commercial and rec 
# years --> 1980: 2021
# region --> NMFS Regions
# species --> Dolphinfish, Dolphinfish**
# report format --> totals by year/region
# Run report and download

setwd("C:/Users/mandy.karnauskas/Desktop/dolphinfish_analyses/data")

d <- read.table("foss_landings_comrec_USEEZ.csv", header = T, sep = ",")
head(d)

# take commas out of file --------------------------
d$Pounds <- as.numeric(gsub(",", "", d$Pounds))
d$Dollars <- as.numeric(gsub(",", "", d$Dollars))
d$Metric.Tons <- as.numeric(gsub(",", "", d$Metric.Tons))
head(d)

# compile by year for sectors and regions ---------------
tab <- tapply(d$Pounds, list(d$Year, d$Region.Name, d$Collection), sum, na.rm = T)
tab[is.na(tab)] <- 0

com <- data.frame(tab[,,1])
rec <- data.frame(tab[,,2])
rec$MN <- rec$Middle.Atlantic + rec$New.England
com$MN <- com$Middle.Atlantic + com$New.England
head(rec)
head(com)

dat <- data.frame(cbind(rec$Gulf, com$Gulf, rec$South.Atlantic, com$South.Atlantic, rec$MN, com$MN))
yrs <- as.numeric(rownames(rec))
dat
yrs

# select years ---------------------
dat <- dat[which(yrs == 1990):which(yrs == 2021), ]

# define colors and plot -------------------
cols <- c("#0000FF60", "#0000FF99", "#FF000060", "#FF000099", "#00FF0060", "#00FF0099")

png(filename = "../plots/dolphin_catch_USEEZ.png", units="in", width = 8, height = 6, pointsize=12, res=72*2)

par(mar = c(4, 4, 1, 1), mgp = c(2.3, 1, 0))

b <- barplot(t(dat)/10^6, names.arg = 1990:2021, col = cols, ylim = c(0,40), # density = dens,
             space = 0, border = NA, las = 2, ylab = "total landings (millions of pounds)", 
             main = "Dolphin landings in the U.S. EEZ")

mtext(side = 3, line = -1.15, "Source: NMFS FOSS commerical and recreational landings statistics", cex = 0.9)

legend(12,38, c("Mid-Atlantic to New England EEZ", "South Atlantic EEZ", "U.S. Gulf of Mexico EEZ"), 
       col = cols[c(6, 4, 2)], pch = 15, pt.cex = 2, bty = "n", y.intersp = 1.3)
legend(12, 31, c("Commercial", "Recreational"), col = c("#00000099", "#00000050"), 
       pch = 15, pt.cex = 2, bty = "n", y.intersp = 1.3)
axis(1, at = b, lab = rep("", length(b)), pos = 0)
abline(h=0)

dev.off()

# end ------------------------