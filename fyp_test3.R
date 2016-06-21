#Finding item id with most records
itemIDs <- res$ItemID
unique(itemIDs)
library(plyr)
table(itemIDs)
itemIDsT <- as.data.frame(table(itemIDs))
View(itemIDsT)