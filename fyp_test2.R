library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=DRAGONFIRE\\\\Sahan,1433;database=PDNSR;trusted_connection=true')
res <- sqlQuery(dbhandle, 'SELECT Date, CustomerID, ItemID FROM [PDNSR].[dbo].[PDNStuRes] order by Date, CustomerID, ItemID;')
res2 <- sqlQuery(dbhandle, 'select * from PDNSR.dbo.PDNStuRes;')

#Finding item id with most records
itemIDs <- res2$ItemID
unique(itemIDs)
library(plyr)
table(itemIDs)
itemIDsT <- as.data.frame(table(itemIDs))

itemIDsT <- itemIDsT[order(-itemIDsT$Freq),]

#mynames <- as.character(sort(unique(res$ItemID)))
mynames <- as.character(itemIDsT[1:15,1])
df <- data.frame(matrix(ncol = length(mynames), nrow = 0))
colnames(df) <- mynames


prevDate <- as.Date(res$Date[1])
prevCustomerID <- res$CustomerID[1]
temprow <- rep(F, length(mynames))
temprow[match(res$ItemID[1], mynames)] <- TRUE

for(i in 2:100000){
  currentDate <- as.Date(res$Date[i])
  currentCustID <- res$CustomerID[i]
  currentItemID <- res$ItemID[i]
  
  if(prevDate == currentDate & prevCustomerID == currentCustID){
    temprow[match(currentItemID, mynames)] <- TRUE
    #print(currentItemID)
  } else {
    prevDate <- currentDate
    prevCustomerID <- currentCustID
    
    df = rbind(df, temprow)
    temprow <- rep(F, length(mynames))
    
    #print("change")
    #print(currentItemID)
    temprow[match(currentItemID, mynames)] <- TRUE
  }
}

colnames(df) <- mynames

write.csv(file="df.csv", x=df)

for(i in names(df)){
  df[[i]] <- as.factor(df[[i]])
}

library(arules)

#rules <- apriori(df)
#inspect(rules)

rules <- apriori(df, parameter = list(minlen=2, supp=0.85, conf=0.95))
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
#which(redundant)
rules.pruned <- rules.sorted[!redundant]

library(arulesViz)
plot(rules)
plot(rules.pruned[1:100], method="graph", control=list(type="items"))

inspect(rules.pruned)