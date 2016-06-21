library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=DRAGONFIRE\\\\Sahan,1433;database=PDNSR;trusted_connection=true')
res <- sqlQuery(dbhandle, 'select * from PDNSR.dbo.PDNStuRes;')

#Convert Date field from string to Date format
res$Date <- as.Date(as.character(res$Date),format="%Y-%m-%d")

#seperate records for itemId = 182
selectedItem <- res[res[, "ItemID"] == 1347,]

#seperate only date and quantity
selectedItemDateandQ <- data.frame(selectedItem$Date,selectedItem$Quantity)

#get the sum of quantities if there are more than one record for one day
selectedItemDateandQSA <- aggregate(x = selectedItemDateandQ[2], FUN = sum, by = list(selectedItemDateandQ$selectedItem.Date))

#renaming colums
colnames(selectedItemDateandQSA)[1] <- "Date"
colnames(selectedItemDateandQSA)[2] <- "Quantity"

plot(selectedItemDateandQSA, type="l")
title("itemID = 182")

#fill in missing dates with quantity 0
alldates = seq(min(selectedItemDateandQSA$Date), max(selectedItemDateandQSA$Date), 1)
dates0 = alldates[!(alldates %in% selectedItemDateandQSA$Date)]
data0 = data.frame(Date = dates0, Quantity = 0)
data = rbind(selectedItemDateandQSA, data0)

#sort by date
data = data[order(data$Date),]

plot(data, type = "l")

#converting to time series and forecasting
#as done in https://rpubs.com/ryankelly/tsa6

TS <- ts(data$Quantity, frequency = 365)
tsF <- HoltWinters(TS, beta=FALSE, gamma = FALSE)
library(forecast)
tsF1 <- forecast.HoltWinters(tsF, h=20)
plot.forecast(tsF1)
View(tsF1)
