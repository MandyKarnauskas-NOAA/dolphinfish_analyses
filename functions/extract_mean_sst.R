# M. Karnauskas 09-23-2022
# code for extracting daily average SST over a specified area
# currently only works with OISST data set 
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.html
# date must be in "YYYY-MM-DD" format
# min and max longitude are in degrees West

extractMeanSST <- function (lonmin, lonmax, latmin, latmax, stdate, endate)  { 
  
  lonmin <- lonmin + 360
  lonmax <- lonmax + 360

urla <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst%5B(", 
               stdate, "T12:00:00Z):1:(", endate, "T12:00:00Z", ")%5D%5B(0.0):1:(0.0)%5D%5B(", 
               latmin, "):1:(", latmin, ")%5D%5B(", 
               lonmin, "):1:(", lonmin, ")%5D")
download.file(url=urla, destfile="trash.csv")
test <- read.csv(file="trash.csv", header = T, skip = 1)
datelis <- test$UTC
df = data.frame(matrix(vector(), 0, 2,
                       dimnames=list(c(), c("date", "SSTav"))), stringsAsFactors = T)

if (length(datelis) > 99)  {  
  subs <- seq(1, length(datelis), 100)  }  else  {  
  subs <- seq(1, length(datelis), 2)  }

for (i in 1: length(subs))  {
  if (i != length(subs))  {
  urlb <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst%5B(", 
                 datelis[subs[i]], "):1:(", datelis[subs[i+1]-1], ")%5D%5B(0.0):1:(0.0)%5D%5B(", 
                 latmin, "):1:(", latmax, ")%5D%5B(", 
                 lonmin, "):1:(", lonmax, ")%5D")    } 
  if (i == length(subs))  { 
    urlb <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst%5B(", 
                   datelis[subs[i]], "):1:(", datelis[length(datelis)], ")%5D%5B(0.0):1:(0.0)%5D%5B(", 
                   latmin, "):1:(", latmax, ")%5D%5B(", 
                   lonmin, "):1:(", lonmax, ")%5D")   } 
  download.file(url=urlb, destfile="trash.csv")
  test <- read.csv(file="trash.csv", header = T, skip = 1)
  
  tab <- tapply(test$degree_C, test$UTC, mean, na.rm = T)
  d1 <- data.frame(cbind(names(tab), tab))
  df <- rbind(df, d1)
    }
  
  df$year <- substr(df$V1, 1, 4)
  df$month <- substr(df$V1, 6, 7)
  df$day <- substr(df$V1, 9, 10)  
  
  final <- data.frame(df, row.names = NULL)
  names(final)[1:2] <- c("DATE", "avSST")

return(final)
} 
