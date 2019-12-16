data = read.csv(file = "C:\\Users\\me4.1\\Box Sync\\Documents\\FALL 2019\\MATP\\Term Project\\FileName (1).csv", skip = 1, header = TRUE, sep = ",", na = c("", "NA"))
library(dplyr)
#library(chron) #gotta filter out some dates, mate
data = data[2:8762,]

data = select (data, -c("NO2"))
data2 = na.omit(data)


data2$PM.2.5 = as.numeric(data2$PM.2.5)
data2$Particle.Count = as.numeric(data2$Particle.Count)
data2$Black.Carbon = as.numeric(data2$Black.Carbon)
data2$WS = as.numeric(data2$WS)
data2$WD = as.numeric(data2$WD)
data2$TEMP = as.numeric(data2$TEMP)
data2$RH = as.numeric(data2$RH)
data2$BP = as.numeric(data2$BP)

data2$Date_new = as.Date(data2$Date, format = "%m/%d/%Y")
#data2$Time_new = format(strptime(data2$Time, "%I:%M %p"), format="%H:%M:%S")
#data2$dateTime = as.POSIXct(paste(data2$Date_new, data2$Time_new), format = "%m/%d/%Y %H:%M")

data2_1 = data2%>% filter(Date_new <= "2019-07-20")
data2_2 = data2%>% filter(Date_new >= "2019-08-18")

data3 <- rbind(data2_1, data2_2)

data3$Date <-NULL
data3$Time <- NULL
data3$TEMP <-NULL
data3$BP <- NULL
data3$RH <- NULL
data4<- na.omit(data3)

#for preliminary analysis
plot(data2$WD, data2$Particle.Count, main = "Nanoparticle Concentration vs Wind Direction", xlab = "Degrees from North", ylab = "# per cubic centimeter")
plot(data2$WD, data2$PM.2.5, main = "PM 2.5 Concentration vs Wind Direction", xlab = "Degrees from North",ylab = "ug/cu.m.")
plot(data2$WD, data2$Black.Carbon, main = "Black Carbon Concentration vs Wind Direction", xlab = "Degrees from North",ylab = "ug/cu.m.")

plot(data2$WS, data2$Particle.Count, main = "Nanoparticle Concentration vs Wind Speed", xlab = "MPH", ylab = "# per cubic centimeter")
plot(data2$WS, data2$PM.2.5, main = "PM 2.5 Concentration vs Wind Speed", xlab = "MPH", ylab = "ug/cu.m.")
plot(data2$WS, data2$Black.Carbon, main = "Black Carbon Concentration vs Wind Speed", xlab = "MPH", ylab = "ug/cu.m.")

#proof of normality not working under any circumstances
#Shapiro test is valid as the data sets are roughly the same size
normal_PC = sample(data4$Particle.Count, 5000)
shapiro.test(normal_PC)

normal_BC = sample(data4$Black.Carbon, 5000)
shapiro.test(normal_BC)

normal_PM25 = sample(data4$PM.2.5, 5000)
shapiro.test(normal_PM25)

#Setting up normalization vectors and data frames for Kmeans

data4$Xvec = data4$WS*cos(data4$WD/180*pi)
data4$Yvec = data4$WS*sin(data4$WD/180*pi)

PM25WD <- data4[,c(5,1)]
PCWD <-data4[,c(5,2)]
BCWD <- data4[,c(5,3)]

PM25WS <- data4[,c(4,1)]
PCWS <- data4[,c(4,2)]
BCWS <- data4[,c(4,3)]

PM25X <-data4[,c(7,1)]
PCX <-data4[,c(7,2)]
BCX <- data4[,c(7,3)]

PM25Y <-data4[,c(8,1)]
PCY <-data4[,c(8,2)]
BCY <- data4[,c(8,3)]

resultBCWD <- kmeans(BCWD[c(1,2)], 15)
resultBCWD$size
resultBCWD$centers
BCWDresult <- as.data.frame(resultBCWD$centers)
plot(BCWD[,], col = resultBCWD$cluster, main = "K Means of Black Carbon Concentration vs. Wind Direction", xlab = "Degrees from North",ylab = "ug/cu.m.")

resultBCWS <- kmeans(BCWS[c(1,2)], 15)
resultBCWS$size
resultBCWS$centers
BCWSresult <- as.data.frame(resultBCWS$centers)
plot(BCWS[,], col = resultBCWS$cluster, main = "K Means of Black Carbon Concentration vs. Wind Speed", xlab = "MPH",ylab = "ug/cu.m.")

resultPCWD <- kmeans(PCWD[c(1,2)], 15)
resultPCWD$size
resultPCWD$centers
PCWDresult <- as.data.frame(resultPCWD$centers)
plot(PCWD[,], col = resultPCWD$cluster, main = "K Means of Nanoparticle Concentration vs. Wind Direction", xlab = "Degrees from North",  ylab = "#/cu.cm.")

resultPCWS <- kmeans(PCWS[c(1,2)], 15)
resultPCWS$size
resultPCWS$centers
PCWSresult <- as.data.frame(resultPCWS$centers)
plot(PCWS[,], col = resultPCWS$cluster, main = "K Means of Nanoparticle Concentration vs. Wind Speed", xlab = "MPH", ylab = "#/cu.cm.")

resultPM25WS <- kmeans(PM25WS[c(1,2)], 15)
resultPM25WS$size
resultPM25WS$centers
PM25WSresult <- as.data.frame(resultPM25WS$centers)
plot(PM25WS[,], col = resultPM25WS$cluster, main = "K Means of PM 2.5 Concentration vs. Wind Speed", xlab = "MPH", ylab = "ug/cu.m.")

resultPM25WD <- kmeans(PM25WD[c(1,2)], 15)
resultPM25WD$size
resultPM25WD$centers
PM25WDresult <- as.data.frame(resultPM25WD$centers)
plot(PM25WD[,], col = resultPM25WD$cluster, main = "K Means of PM 2.5 Concentration vs. Wind Direction", xlab = "Degrees from North", ylab = "ug/cu.m.")

resultPM25X <- kmeans(PM25X[c(1,2)], 15)
resultPM25X$size
resultPM25X$centers
PM25Xresult <- as.data.frame(resultPM25X$centers)
plot(PM25X[,], col = resultPM25X$cluster, main = "K Means of PM 2.5 Concentration Compensated for the X Vector", xlab = "MPH", ylab = "ug/cu.m.")

resultPM25Y <- kmeans(PM25Y[c(1,2)], 15)
resultPM25Y$size
resultPM25Y$centers
PM25Yresult <- as.data.frame(resultPM25Y$centers)
plot(PM25Y[,], col = resultPM25Y$cluster, main = "K Means of PM 2.5 Concentration Compensated for the Y Vector", xlab = "MPH", ylab = "ug/cu.m.")

resultPCX <- kmeans(PCX[c(2,1)], 15)
resultPCX$size
resultPCX$centers
PCXresult <- as.data.frame(resultPCX$centers)
plot(PCX[,], col = resultPCX$cluster, main = "K Means of Nanoparticle Counts Compensated for the X Vector", xlab = "MPH", ylab = "#/cu.cm.")

resultPCY <- kmeans(PCY[c(1,2)], 15)
resultPCY$size
resultPCY$centers
PCYresult <- as.data.frame(resultPCY$centers)
plot(PCY[,], col = resultPCY$cluster, main = "K Means of Nanoparticle Counts Compensated for the Y Vector", xlab = "MPH", ylab = "#/cu.cm.")

resultBCX <- kmeans(BCX[c(1,2)], 15)
resultBCX$size
resultBCX$centers
BCXresult <- as.data.frame(resultBCX$centers)
plot(BCX[,], col = resultBCX$cluster, main = "K Means of Black Carbon Concentration Compensated for the X Vector", xlab = "MPH", ylab = "ug/cu.m.")

resultBCY <- kmeans(BCY[c(1,2)], 15)
resultBCY$size
resultBCY$centers
BCYresult <- as.data.frame(resultBCY$centers)
plot(BCY[,], col = resultBCY$cluster, main = "K Means of Black Carbon Concentration Compensated for the Y Vector", xlab = "MPH", ylab = "ug/cu.m.")