############################################################################################
# M. Karnauskas revised 09-23-2022
# code for sorting through dolphin catches reported in Sea Around Us database
# data accessed 09-28-2022 from Sea Around Us (https://www.seaaroundus.org/data/#/eez)

# Search by EEZ for global catches and then Atlantic high seas by region

# For EEZ data: 
# https://www.seaaroundus.org/data/#/topic/biodiversity
# in Biodiversity by Taxon, search EEZ --> All regions --> Species --> Common dolphinfish
# Global catches of Common dolphinfish (Coryphaena hippurus) by EEZ --> Download Data

# For high seas data: https://www.seaaroundus.org/data/#/highseas 
# Click on individual regions and then download
# Catches by Taxon in the non-EEZ waters of the Atlantic, Western Central
# Catches by Taxon in the non-EEZ waters of the Atlantic, Northwest
# Catches by Taxon in the non-EEZ waters of the Atlantic, Northeast
# Catches by Taxon in the non-EEZ waters of the Atlantic, Eastern Central
# Catches by Taxon in the non-EEZ waters of the Atlantic, Southwest
# Catches by Taxon in the non-EEZ waters of the Atlantic, Southeast
############################################################################################

rm(list = ls())
library(pals)

# input high seas compilations --------------------------------

hslis <- dir("data/")[grep("HighSeas", dir("data/"))]

h1 <- read.csv(paste0("data/", hslis[1]))
h2 <- read.csv(paste0("data/", hslis[2]))
h3 <- read.csv(paste0("data/", hslis[3]))
h4 <- read.csv(paste0("data/", hslis[4]))
h5 <- read.csv(paste0("data/", hslis[5]))
h6 <- read.csv(paste0("data/", hslis[6]))

# look at WCA catches --------------------

hwca <- h3[which(h3$common_name == "Common dolphinfish"), ]

apply(hwca[1:13], 2, table)
hwca$pounds <- hwca$tonnes * 1000 * 2.20462
hwca$fishing_entity <- droplevels(hwca$fishing_entity)
hwca$gear_type <- droplevels(hwca$gear_type)

par(mar = c(10, 5, 1, 1), mfrow = c(4, 2))
barplot(tapply(hwca$pounds, hwca$area_name, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$area_type, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$year, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$fishing_entity, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$fishing_sector, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$catch_type, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$reporting_status, sum, na.rm = T), las = 2)
barplot(tapply(hwca$pounds, hwca$gear_type, sum, na.rm = T), las = 2)

# plot fishing entities by year for WCA -----------------

par(mar = c(5, 5, 1, 1), mfrow = c(1, 1))
tab <- tapply(hwca$pounds, list(hwca$year, hwca$fishing_entity), sum, na.rm = T)
matplot(as.numeric(rownames(tab)), tab/10^6, type = "l", col = glasbey(ncol(tab)), lty = 1, lwd = 2, 
        xlab = "year", ylab = "total catch (millions of pounds)", 
        main = "High seas Western Central Atlantic dolphin catch")
legend("topleft", colnames(tab), col = glasbey(ncol(tab)), lty = 1, lwd = 2, cex = 0.8, bty = "n")

# compile high seas for all Atl regions -----------------

hs <- rbind(h1, h2, h3, h4, h5, h6)
unique(hs$common_name)[grep("dolphin", unique(hs$common_name))]
hs <- hs[which(hs$common_name == "Common dolphinfish"), ]

apply(hs[1:13], 2, table)
hs$pounds <- hs$tonnes * 1000 * 2.20462
hs$fishing_entity <- droplevels(hs$fishing_entity)
hs$gear_type <- droplevels(hs$gear_type)

par(mar = c(12, 5, 1, 1), mfrow = c(4, 2))
barplot(tapply(hs$pounds, hs$area_name, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$area_type, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$year, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$fishing_entity, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$fishing_sector, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$catch_type, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$reporting_status, sum, na.rm = T), las = 2)
barplot(tapply(hs$pounds, hs$gear_type, sum, na.rm = T), las = 2)

# plot catches by area, entity and year --------------------------------

par(mfrow = c(2, 1), mar = c(8, 5, 2, 1))
tab4 <- tapply(hs$pounds, list(hs$area_name, hs$fishing_entity), sum, na.rm = T)
tab4[which(is.na(tab4))] <- 0
colnames(tab4)[grep("Grenadines", colnames(tab4))] <- "St. Vincent/Gren."
barplot(matrix(tab4/10^6, nrow = 6), las = 2, legend.text = rownames(tab4), col = 1:6, names.arg = colnames(tab4), 
        main = "dolphin catches by fishing entity and area", 
        ylab = "total dolphin catch\n(millions of pounds)") 

tab <- tapply(hs$pounds, list(hs$year, hs$area_name), sum, na.rm = T)
matplot(as.numeric(rownames(tab)), tab/10^6, type = "l", col = 1:6, lty = 1, lwd = 2, 
        xlab = "year", ylab = "total dolphin catch\n(millions of pounds)", 
        main = "dolphin catches by year and area")
legend("topleft", colnames(tab), col = 1:6, lty = 1, lwd = 2, cex = 0.8, bty = "n")

par(mfrow = c(1, 1), mar = c(5, 5, 2, 1))
tab <- tapply(hs$pounds, list(hs$year, hs$fishing_entity), sum, na.rm = T)
matplot(as.numeric(rownames(tab)), tab/10^6, type = "l", col = glasbey(ncol(tab)), lty = 1, lwd = 2, 
        xlab = "year", ylab = "total dolphin catch (millions of pounds)", 
        main = "dolphin catches by year and fishing entity for all regions")
legend("topleft", colnames(tab), col = glasbey(ncol(tab)), lty = 1, lwd = 2, cex = 0.8, bty = "n", ncol = 1)

hs$year <- factor(hs$year, levels = 1950:2019)
tab <- tapply(hs$pounds, list(hs$year, hs$area_name), sum, na.rm = T)
tab[is.na(tab)] <- 0

# write summary to merge with EEZ data --------------------

# write.csv(tab, file = "data/high_seas_by_year.csv")


# download EEZ data ----------------------------------------

rm(list = ls())
#d <- read.csv("data/SAU_dolphin.csv", header = T, sep = ",")  # old version of data downloaded 05/05/21
d <- read.csv("data/SAU Taxa 600006 v50-0.csv", header = T, sep = ",")
head(d)
dim(d)

# check data fields and do basic summaries-------------------
apply(d[1:13], 2, table)
yrs <- as.numeric(names(table(d$year)))

par(mfrow = c(2, 2))
barplot(tapply(d$tonnes, d$fishing_sector, sum, na.rm = T))
barplot(tapply(d$tonnes, d$catch_type, sum, na.rm = T))
barplot(tapply(d$tonnes, d$reporting_status, sum, na.rm = T))
par(mar= c(10, 2, 1, 1))
barplot(tapply(d$tonnes, d$gear_type, sum, na.rm = T), las = 2)

# convert tonnes to pounds -----------------------------------
d$lbs <- d$tonnes * 1000 * 2.20462
d$sect <- NA
d$sect[which(d$fishing_sector != "Recreational")] <- "Commercial"
d$sect[which(d$fishing_sector == "Recreational")] <- "Recreational"
table(d$fishing_sector, d$sect)

# pick apart high seas and Venezuelan LL -----------------------
dhs <- d[which(d$area_type == "high_seas"), ]
dhs$fishing_entity <- droplevels(dhs$fishing_entity)
tapply(dhs$lbs, dhs$fishing_entity, sum, na.rm = T)
sort(tapply(dhs$lbs, dhs$fishing_entity, sum, na.rm = T))

par(mar = c(4, 8, 1, 1), mfrow = c(1, 1))
barplot(sort(tapply(dhs$lbs, dhs$fishing_entity, sum, na.rm = T)/10^6), horiz = T, las = 2)
lis <- sort(tapply(dhs$lbs, dhs$fishing_entity, sum, na.rm = T), decreasing = T)[1:10]; lis
#dhs <- dhs[which(dhs$fishing_entity %in% lis), ]

par(mar = c(4, 4, 1, 1))
matplot(yrs, tapply(dhs$lbs, list(dhs$year, dhs$fishing_entity), sum, na.rm = T)/10^6, ylim = c(0, 60),
        type = "p", pch = rep(c(19, 1), 2), col = glasbey(20), cex = 1.2)
matplot(yrs, tapply(dhs$lbs, list(dhs$year, dhs$fishing_entity), sum, na.rm = T)/10^6, 
        type = "l", lty = 1, col = glasbey(20), add = T)
legend("topleft", levels(dhs$fishing_entity), col = glasbey(20), lty = 1, pch = rep(c(19, 1), 2))

dven <- d[which(d$fishing_entity == "Venezuela" & d$area_type == "high_seas"), ]

# separate EEZs by region manually --------------------------------------

d <- d[which(d$area_type != "high_seas"), ]  # taking out high seas for now
d$fishing_entity <- droplevels(d$fishing_entity)
table(d$area_type)

WCA <- c("Bahamas", "Barbados", "Cayman Isl. (UK)", "Dominica", "Dominican Republic", "Grenada", "Aruba (Netherlands)", 
           "Haiti", "Guadeloupe  (France)", "Jamaica", "Martinique (France)", "Montserrat (UK)", "Nicaragua (Caribbean)", 
           "Puerto Rico (USA)", "Saint Kitts & Nevis", "Saint Lucia", "Saint Vincent & the Grenadines", "Trinidad & Tobago", 
           "US Virgin Isl.", "St Barthelemy (France)", "St Martin (France)", "Curaçao (Netherlands)", "Bonaire (Netherlands)", 
           "Saba and Sint Eustatius (Netherlands)", "Sint Maarten (Netherlands)", "Honduras (Caribbean)", "Colombia (Caribbean)", 
           "Mexico (Atlantic)", "Turks & Caicos Isl. (UK)", "Guyana", "Venezuela", "Antigua & Barbuda", "Belize", "British Virgin Isl. (UK)", 
           "French Guiana", "Cuba", "Anguilla (UK)", "Suriname", "Costa Rica (Caribbean)", "Guatemala (Caribbean)", "Panama (Caribbean)", 
           "Bermuda (UK)", "CuraÃ§ao (Netherlands)") 
           
Bra <- c("Brazil (mainland)", "Fernando de Noronha (Brazil)", "St Paul and St. Peter Archipelago (Brazil)")

Can <-  c("Canada (East Coast)", "Saint Pierre & Miquelon (France)")

USA <-  c("USA (East Coast)", "USA (Gulf of Mexico)")
           
NEA <- c("Canary Isl. (Spain)", "Spain (mainland; Med and Gulf of Cadiz)", "Spain (Northwest)", 
         "Spain (mainland, Med and Gulf of Cadiz)", 
         "France (Atlantic Coast)", "Portugal (mainland)", "Azores Isl. (Portugal)")
        
ECA <- c("Madeira Isl. (Portugal)", "Cape Verde", "Equatorial Guinea", "Benin", 
          "Sao Tome & Principe ", "Sao Tome & Principe", "Morocco (Central)", "Ghana", 
          "Côte d'Ivoire", "CÃ´te d'Ivoire", "Guinea", "Liberia", "Sierra Leone", "Nigeria", "Togo", 
          "Guinea-Bissau", "Senegal", "Cameroon", "Gambia", "Mauritania", "Morocco (South)", 
          "Gabon", "Congo; R. of", "Congo, R. of", "Congo (ex-Zaire)")

SEA <- c("Angola", "Namibia", "South Africa (Atlantic and Cape)",  
           "Ascension Isl. (UK)", "Saint Helena (UK)", "Tristan da Cunha Isl. (UK)")

SWA <- c("Trindade & Martim Vaz Isl. (Brazil)", "Uruguay")

Med <- c("Italy (mainland)", "Libya", "Malta", "Syria", "Sicily (Italy)","Sardinia (Italy)", "Balearic Islands (Spain)", "Cyprus (North)", "Cyprus (South)", 
         "Gaza Strip", "Greece (without Crete)", "Crete (Greece)", "Lebanon", "Turkey (Mediterranean Sea)", "Egypt (Mediterranean)", "Israel (Mediterranean)", "Tunisia", 
         "Algeria", "Albania", "Croatia", "Montenegro", "Morocco (Mediterranean)", "France (Mediterranean)", "Corsica (France)")

# add variable to specify region --------------------

d$reg <- "other oceans"
d$reg[which(d$area_type == "high_seas" & d$fishing_entity == "Venezuela")] <- "high seas (Venzuela fleet)"
d$reg[which(d$area_name %in% WCA)] <- "Western Central EEZs"
d$reg[which(d$area_name %in% Bra)] <- "Brazil EEZ"
d$reg[which(d$area_name %in% Can)] <- "Canadian EEZ"
d$reg[which(d$area_name %in% USA)] <- "United States EEZ"
d$reg[which(d$area_name %in% NEA)] <- "Northeast EEZs"
d$reg[which(d$area_name %in% ECA)] <- "Eastern Central EEZs"
d$reg[which(d$area_name %in% SEA)] <- "Southeast EEZs"
d$reg[which(d$area_name %in% SWA)] <- "Southwest EEZs"
d$reg[which(d$area_name %in% Med)] <- "Mediterranean Sea EEZs"

dcan <- d[which(d$reg == "Canadian EEZ"),]
tapply(dcan$lbs, dcan$year, sum, na.rm = T)
#1957         1962         2007         2008         2009         2010         2011         2012         2013         2014         2015         2016 
#3.657663e+02 4.467291e+03 1.394812e+01 2.078243e+01 2.297765e-01 4.409240e+04 1.543234e+04 4.355331e+05 9.490974e+04 2.167130e+06 2.642347e+06 2.030556e+06 

# check region designation ----------------------------------
othlis <- unique(d$area_name[which(d$reg == "other oceans")])
othlis[grep("USA", othlis)]
othlis[grep("France", othlis)]
othlis[-c(grep("France", othlis), grep("USA", othlis), grep("Pacific", othlis))]

unique(d$area_name[which(d$reg == "high seas (Venzuela fleet)")])
unique(d$area_name[which(d$reg == "Western Central EEZs")])
unique(d$area_name[which(d$reg == "Brazil EEZ")])
unique(d$area_name[which(d$reg == "Canadian EEZ")])
unique(d$area_name[which(d$reg == "United States EEZ")])
unique(d$area_name[which(d$reg == "Northeast EEZs")])
unique(d$area_name[which(d$reg == "Eastern Central EEZs")])
unique(d$area_name[which(d$reg == "Southeast EEZs")])
unique(d$area_name[which(d$reg == "Southwest EEZs")])
unique(d$area_name[which(d$reg == "Mediterranean Sea EEZs")])

tapply(d$tonnes, d$area_name, sum, na.rm = T)
sort(tapply(d$tonnes, d$area_name, sum, na.rm = T), decreasing = T)

par(mar = c(5, 16, 1, 1))
barplot(sort(tapply(d$tonnes, d$area_name, sum, na.rm = T), decreasing = T)[1:30], horiz = T, las = 2)

# look at reporting status --------------------------

d1 <- d[which(d$reg != "other oceans"), ]
tapply(d1$lbs, list(d1$reg, d1$reporting_status), sum, na.rm = T)

par(mar = c(5, 12, 1, 1))
barplot(tapply(d1$lbs, list(d1$reporting_status, d1$reg), sum, na.rm = T)/10^6, horiz = T, las = T, col = c(4, 2), 
        legend.text =  c("Reported data (FAO)", "Unreported data"), args.legend = list(x = "bottomright"),
        xlab = "millions of pounds")

tapply(d$lbs, d$reg, sum, na.rm = T)
sort(tapply(d$lbs, d$reg, sum, na.rm = T))

par(mar = c(5, 16, 1, 1))
barplot(sort(tapply(d$lbs, d$reg, sum, na.rm = T))/10^6, las = 2, horiz = T, 
        xlab = "millions of pounds landed (1950 - 2016)")

# summarize catch by regional EEZs ---------------------------
tab_glob <- tapply(d$lbs, list(d$year, d$reg), sum, na.rm = T)
head(tab_glob)
tab3 <- t(tab_glob)
tab3 <- tab3[order(rowSums(tab3, na.rm = T), decreasing = T), ]
tab_glob <- t(tab3)
head(tab_glob)

par(mar = c(5, 4, 4, 3), mgp = c(3, 1, 0))
matplot(yrs, tab_glob/10^6, las = 2, type = "l", lty = 1, lwd = 3, col = glasbey(ncol(tab)), 
        xlab = "", ylab = "total catch (millions of pounds)", main = "Global EEZ dolphin catches (commercial + recreational)")
mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("topleft", colnames(tab), col = glasbey(ncol(tab)), lty = 1, lwd = 3, bty = "n")

head(tab_glob)
tab <- tab_glob[, -which(colnames(tab_glob) == "other oceans")]
head(tab)

matplot(yrs[1:67], tab[1:67,]/10^6, las = 2, type = "b", pch = 19, cex = 0.5, lty = 1, col = glasbey(ncol(tab)),   
        xlab = "", ylab = "total catch (millions of pounds)", axes = F,
        main = "Dolphin catches from the Atlantic EEZs (commercial + recreational)")
matplot(yrs[1:67], tab[1:67,]/10^6, las = 2, type = "l", pch = 19, lty = 1, lwd = 2, add = T, col = glasbey(ncol(tab)))
mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
axis(1, at = yrs[seq(1, 90, 2)], las = 2); axis(2); box()
legend("left", colnames(tab), col = glasbey(ncol(tab)), pch = 19, lty = 1, lwd = 2, bty = "n")

hs <- read.csv("data/high_seas_by_year.csv")
names(hs)[1] <- "year"

head(tab)
head(hs)
table(rownames(tab) == hs$year)

tab[which(is.na(tab))] <- 0

tab_atl <- cbind(tab, hs[2:7])
head(tab_atl)

matplot(yrs, tab_atl/10^6, las = 2, type = "l", lwd = 2, col = glasbey(ncol(tab_atl)), 
        lty = c(rep(1, 9), rep(2, 6)), 
        xlab = "", ylab = "total catch (millions of pounds)", main = "All Atlantic dolphin catches (commercial + recreational)")
mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("topleft", colnames(tab_atl), col = glasbey(ncol(tab_atl)), 
       lty = c(rep(1, 9), rep(2, 6)), lwd = 3, bty = "n")

nwa <- tab_atl$`Canadian EEZ` + tab_atl$Atlantic..Northwest
nea <- tab_atl$`Mediterranean Sea EEZs` + tab_atl$`Northeast EEZs` + tab_atl$Atlantic..Northeast
eca <- tab_atl$`Eastern Central EEZs` + tab_atl$Atlantic..Eastern.Central
wca <- tab_atl$`United States EEZ` + tab_atl$`Western Central EEZs` + tab_atl$Atlantic..Western.Central
swa <- tab_atl$`Brazil EEZ` + tab_atl$`Southwest EEZs` + tab_atl$Atlantic..Southwest
sea <- tab_atl$`Southeast EEZs` + tab_atl$Atlantic..Southeast

tab2 <- data.frame(cbind(wca, nwa, nea, eca, sea, swa))
labs <- c("Western Central", "Northwest", "Northeast (incl. Mediterranean)", "Eastern Central", "Southeast", "Southwest")

matplot(yrs, tab2/10^6, las = 2, type = "l", lwd = 2, col = 1:6, 
        lty = c(rep(1, 9), rep(2, 6)), 
        xlab = "", ylab = "total catch (millions of pounds)", 
        main = "All Atlantic dolphin catches (commercial + recreational)")
mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("topleft", labs, col = 1:6, 
       lty = c(rep(1, 9), rep(2, 6)), lwd = 3, bty = "n")


# analyze WCA ----------------------------------
dc <- d[which(d$area_name %in% WCA), ]
dc$area_name <- droplevels(dc$area_name)
dc1 <- dc[which(dc$year >= 1990), ]

tapply(dc1$tonnes, dc1$area_name, sum, na.rm = T)
sort(tapply(dc1$tonnes, dc1$area_name, sum, na.rm = T), decreasing = T)

par(mar = c(5, 10, 1, 1)) 
barplot(sort(tapply(dc1$lbs, dc1$area_name, sum, na.rm = T)/10^6, decreasing = T), 
        cex.names = 0.7, horiz = T, las = 1, xlab = "millions of pounds landed 1950-2016")

tab <- tapply(dc$lbs, list(dc$year, dc$area_name), sum, na.rm = T)
tab

par(mar = c(3, 3, 1, 1))
colSums(tab, na.rm = T)
matplot(yrs, tab, type = "l")

matplot(yrs[41:length(yrs)], tab[41:length(yrs), ], type = "l")

hist(colMeans(tab, na.rm = T))
hist(colMeans(tab[40:67, ], na.rm = T), breaks = 30)

which(colMeans(tab, na.rm = T) > 300000)
tab2 <- tab[, which(colMeans(tab, na.rm = T) > 300000)]

tab3 <- t(tab2)
tab3 <- tab3[order(rowMeans(tab3, na.rm = T), decreasing = T), ]
tab2 <- t(tab3)
head(tab3)

matplot(yrs, tab2, type = "l", col = glasbey(ncol(tab2)), lty = 1)
legend("topleft", colnames(tab2), col = glasbey(ncol(tab2)), lty = 1, cex = 0.8, bty = "n")

which(yrs == 1990)
par(mar = c(5, 5, 4, 1))
matplot(yrs[41:length(yrs)], tab2[41:length(yrs), ]/10^6, type = "l", lwd = 2, pch = 19, col = glasbey(ncol(tab2)), lty = 1, xlab = "", 
        ylab = "total catch (millions of pounds)", 
        main = "Total dolphin catch (commercial + recreational)\nfor top Western Central Atlantic territories")
matplot(yrs[41:length(yrs)], tab2[41:length(yrs), ]/10^6, type = "p", pch = 19, col = glasbey(ncol(tab2)), lty = 1, add = T)
mtext(side = 3, line = 0, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("topleft", colnames(tab2), ncol = 1, col = glasbey(16), lty = 1, pch = 19, bty = "n")

# analyze ECA ----------------------------------
de <- d[which(d$area_name %in% ECA), ]
de$area_name <- droplevels(de$area_name)

tapply(de$tonnes, de$area_name, sum, na.rm = T)
sort(tapply(de$tonnes, de$area_name, sum, na.rm = T), decreasing = T)

par(mar = c(5, 10, 1, 1)) 
barplot(sort(tapply(de$lbs, de$area_name, sum, na.rm = T)/10^6, decreasing = T), 
        cex.names = 0.7, horiz = T, las = 1, xlab = "millions of pounds landed 1950-2016")

tab <- tapply(de$lbs, list(de$year, de$area_name), sum, na.rm = T)
tab

par(mar = c(3, 3, 1, 1))
colSums(tab, na.rm = T)
matplot(yrs, tab, type = "l")

matplot(yrs[41:length(yrs)], tab[41:length(yrs), ], type = "l")
tab3 <- t(tab)
tab3 <- tab3[order(rowMeans(tab3[,60:70], na.rm = T), decreasing = T), ]
tab2 <- t(tab3)
head(tab3)

matplot(yrs, tab2, type = "l", col = glasbey(ncol(tab2)), lty = 1)
legend("topleft", colnames(tab2), col = glasbey(ncol(tab2)), lty = 1, cex = 0.8, bty = "n")

which(yrs == 1990)
par(mar = c(5, 5, 4, 1))
matplot(yrs[51:length(yrs)], tab2[41:length(yrs), ]/10^6, type = "l", lwd = 2, pch = 19, col = glasbey(ncol(tab2)), lty = 1, xlab = "", 
        ylab = "total catch (millions of pounds)", 
        main = "Total dolphin catch (commercial + recreational) for Eastern Central Atlantic countries")
matplot(yrs[51:length(yrs)], tab2[41:length(yrs), ]/10^6, type = "p", pch = 19, col = glasbey(ncol(tab2)), lty = 1, add = T)
mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("topleft", colnames(tab2), ncol = 1, col = glasbey(16), lty = 1, pch = 19, bty = "n")

delis <- c("Ghana", "CÃ´te d'Ivoire", "Gabon", "Sao Tome & Principe")
d2 <- d[which(d$area_name %in% delis & d$year > 2010), ]
d2$gear_type <- droplevels(d2$gear_type)

par(mfrow = c(2, 2), mar = c(10, 7, 1, 1))
barplot(tapply(d2$lbs, d2$catch_type, sum, na.rm = T), las = 2)
barplot(tapply(d2$lbs, d2$gear_type, sum, na.rm = T), las = 2)
barplot(tapply(d2$lbs, d2$reporting_status, sum, na.rm = T), las = 2)
barplot(tapply(d2$lbs, d2$fishing_sector, sum, na.rm = T), las = 2)

# analyze all WCA catches ----------------------------------

dwc <- d[which(d$reg == "Western Central EEZs" | d$reg == "United States EEZ" | d$reg == "Canadian EEZ"), ]

tab_wc <- tapply(dwc$lbs, list(dwc$year, dwc$reg), sum, na.rm = T)
head(tab_wc)
head(hs)
table(rownames(tab_wc) == hs$year)

high_seas <- hs$Atlantic..Western.Central
high_seas[high_seas == 0] <- NA
tab_wc <- cbind(tab_wc, high_seas)

par(mar = c(5, 4, 4, 3), mgp = c(3, 1, 0))
matplot(yrs, tab_wc/10^6, las = 2, type = "l", lty = 1, lwd = 3, col = glasbey(ncol(tab_wc)), 
        xlab = "", ylab = "total catch (millions of pounds)", main = "Western Central Atlantic dolphin catches (commercial + recreational)")
mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("topleft", colnames(tab_wc), col = glasbey(ncol(tab_wc)), lty = 1, lwd = 3, bty = "n")

tab_wc[is.na(tab_wc)] <- 0
tab_wc1 <- tab_wc[which(yrs %in% 1990:2019), ]
tab_wc1 <- tab_wc1[, c(2, 3, 1, 4)]
colnames(tab_wc1)
colnames(tab_wc1)[2] <- "Greater Caribbean EEZs"
colnames(tab_wc1)[4] <- "Western Central Atlantic high seas"

par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
barplot(t(tab_wc1)/10^6, col = 3:6, las = 2, legend.text = colnames(tab_wc1),
        args.legend = list(x = "top", bty = "n"), ylim = c(0, 60), 
        xlab = "", ylab = "total catch (millions of pounds)", 
        main = "Total dolphin catches in the Western Central Atlantic Region")
abline(h = 0)

tab_wc1[tab_wc1 == 0] <- NA
par(mar = c(3, 4, 1, 1))
matplot(1990:2019, tab_wc1/10^6, las = 2, type = "l", lty = 1, lwd = 3, col = 3:6, ylim = c(0, 40), 
        xlab = "", ylab = "total catch (millions of pounds)", axes = F)
matplot(1990:2019, tab_wc1/10^6, las = 2, type = "p", pch = 15, col = 3:6, add = T)
axis(1, at = 1990:2019, las = 2); axis(2, las = 2)
#mtext(side = 3, line = 0.5, "Source: Pauly et al (2020) Sea Around Us Concepts, Design and Data (www.seaaroundus.org)", cex = 0.8)
legend("top", colnames(tab_wc1)[4:1], col = 6:3, lty = 1, lwd = 3, bty = "n")

# write.csv(tab_wc1, file = "data/WCA_catches.csv")

# end -------------------------------------
