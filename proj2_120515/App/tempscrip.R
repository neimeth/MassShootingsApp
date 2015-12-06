Data2015g[312, 4] = "Phoenix, AZ"
Data2015g[312, 6] = -112.074
Data2015g[312, 7] = 33.44838
save.image(file="Project2_UD.RData")
Data2015g$Killed <- as.integer(Data2015g$Killed)
Data2015g$Wounded <- as.integer(Data2015g$Wounded)
Data2015g$lon <- as.numeric(Data2015g$lon)
Data2015g$lat <- as.numeric(Data2015g$lat)


Data2015g[312, 7] = 33.44838

DataYear <- as.data.frame(DataYear)
DataMonth <- as.data.frame(DataMonth)
