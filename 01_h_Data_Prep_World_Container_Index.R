### Container Prices ###

### WORK IN PROGRESS ###

# Clear previous code
rm(list=ls())

if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("data.table")) install.packages("data.table.") 

library(tidyverse)
library(data.table)

# The world container prices are hard to access, as a result, the world container index (assessed by Drewry) has to be typed in manually. The data is retrived from the following 
# website ("https://infogram.com/world-container-index-1h17493095xl4zj"). Of course, as usual, this does not allow for automation, but having manually retrieved data is better
# than no data. The World Container Index helps us to explain part of the variation of the fruit and vegetable prices, as container freight prices are key price determinants.

# The world container index on the given website is given weekly, starting from the 10 March of 2016 up to the actual week. To avoid typing in hundreds of dates, I program
# an for-loop that produces the weekly dates from the 10th of March of up to the actual week.

dif_days <- as.numeric(as.Date("2022-01-20") - as.Date("2016-03-10")) + 7

date_begin <- as.Date("2016-03-10")

a <- as.data.frame(matrix(0, ncol=1, nrow=(dif_days/7)))

a$V1 <- as.Date(a$V1, origin = "2016-03-10")

dates <- date_begin

for(i in 2:(dif_days/7)) {
  dates <- dates + 7
  a$V1[i] <- dates
}

# The second column is filled with the world container index values. Around 295 values are typed in manually:

# Last manual update 20.01.2021 

wci <- c("700.57", "674.41", "666.27", "849.08", "868.06", "839.24", "721.94", "763.50",
         "1135.57", "1094.67", "1039.32", "935.17", "1229.05", "1143.47", "1008.59", "924.75",
         "1203.94", "1394.39", "1316.94", "1210.75", "1182.77", "1374.39", "1250.59", "1189.33",
         "1090.14", "1352.26", "1391.26", "1439.64", "1410.12", "1368.65", "1235.63", "1247.48",
         "1329.42", "1319.33", "1497.68", "1478.32", "1437.39", "1365.50", "1392.96", "1408.38",
         "1388.12", "1507.12", "1501.72", "1769.73", "1743.24", "1799.48", "1768.76", "1738.98", 
         "1717.18", "1572.45","1492.86", "1496.31", "1476.90", "1390.91", "1350.61", "1365.92", 
         "1523.86", "1457.2", "1449.91", "1426.7", "1612.83", "1557.38", "1500.87", "1500.87",
         "1466.36", "1471.18", "1464.32", "1390.95", "1380.96", "1549.72", "1517.23", "1421.40",
         "1406.54", "1591.35", "1567.66", "1540.70", "1502.10", "1430.69", "1401.60", "1385.17",
         "1317.53", "1292.06", "1282.10", "1222.37", "1207.46", "1517.10", "1374.82", "1230.15", 
         "1207.87", "1147.47", "1157.21", "1133.96", "1195.21", "1211.29", "1410", "1407.58", "1440.99",
         "1440.99", "1465.72", "1539.16", "1512", "1508", "1497", "1465", "1374.6", "1331", "1272",
         "1172", "1219", "1192", "1179", "1157", "1415", "1382", "1367", "1352", "1387", "1427",
         "1414", "1391", "1378", "1468", "1439", "1487", "1495", "1697", "1692", "1760", "1754", "1754",
         "1773", "1753", "1725", "1721", "1668", "1636", "1666", "1666", "1207", "1799", "1805", "1721",
         "1708", "1643", "1623", "1627", "1571", "1793", "1690", "1690", "1766", "1720",
         "1664", "1683", "1606", "1478", "1440", "1323", "1293", "1324", "1395", "1334",
         "1330", "1342", "1379", "1394", "1364", "1310", "1291", "1371", "1351", "1351",
         "1353", "1372", "1309", "1373", "1321", "1405", "1463", "1456", "1454", "1407",
         "1454", "1387", "1271", "1262", "1228", "1196", "1242", "1238", "1406", "1448", "1400", "1348", "1367",
         "1442", "1515", "1592", "1712", "1832", "1781", "1791", "1725", "1733", "1673", "1673", "1576", "1540", "1486",
         "1483", "1585", "1525", "1530", "1530", "1504", "1492", "1446", "1486", "1542", "1593",
         "1576", "1687", "1715", "1866", "1855", "2032", "2023", "2002", "1996", "1995", "2059", "2089", 
         "2177", "2251", "2464", "2453", "2603", "2644", "2605", "2583", "2587", "2587", "2615", "2628", "2703","2703",
         "3060", "3436", "3451", "4244", "4285", "4359", "5221", "5238", "5340", "5252", "5229",
         "5191", "5250", "5238", "5121", "5027", "4943", "4872", "4883", "4911", "4833", "4913",
         "4984", "5472", "5727", "6135", "6257", "6257", "6727", "6957", "8062", "8399", "8796", "8883",
         "8986", "9330", "9371", "9421", "9613", "9818", "9987", "10084", "10375", "10377", "10310",
         "9900", "9865", "9669", "9669", "9195", "9193", "9146", "9186", "9051", "9262", "9292", "9304","9304", "9409", "9545",
         "9698")

world_container_index <- cbind(a, wci)

world_container_index <- world_container_index %>%
  rename(date = V1) %>%
  mutate(wci = as.numeric(wci))
 
## Creating the rows from the 01.01.2016 to the 10.03.2016, where data is available ##

num_of_lost_weeks <- floor(as.numeric(as.Date("2016-03-03") -  as.Date("2015-12-01")) / 7)

new_date <- as.Date("2016-03-03")

df_dates <- new_date

for(j in 1:num_of_lost_weeks){
  new_dates <- new_date - 7*j
  df_dates <- c(new_dates,df_dates)
}

wci <- c(rep(NA, num_of_lost_weeks + 1))

old_dates <- data.frame(df_dates, wci)

colnames(old_dates)[1] <- "date"

#old_dates <- old_dates[-10,]

world_container_index <- rbind(world_container_index, old_dates)

world_container_index <- world_container_index %>%
  arrange(date)

world_container_index <- imputeTS::na_ma(world_container_index, k=10,weighting = "simple")

#world_container_index$week <- strftime(world_container_index$date, format = "%V")

fwrite(world_container_index, "output/world_container_index_without_lags.csv")



