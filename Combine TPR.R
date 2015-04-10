STARTYEAR <- 2005; ENDYEAR <- 2014
YEAR <- STARTYEAR:ENDYEAR
WDPATH <- "D:/Baidu Cloud/Crosby MBA Program 2015 Spring/BA 8600 - Consulting/R programing/"

## Set the working directory according the "WD" constant
setwd(WDPATH)


## Creat a "CSR" folder under the working directory to store the generated CSR
## files
dir.create("./CSR", showWarnings = FALSE)

filePath <- paste("./TPR/WR TPR ", YEAR[1], ".csv", sep = "")

ori.TPR <- read.csv(filePath, stringsAsFactors=FALSE)
ori.TPR$Year <- YEAR[1]

for (i in 2:length(YEAR)){
        filePath <- paste("./TPR/WR TPR ", YEAR[i], ".csv", sep = "")
        yearlyTPR <- read.csv(filePath, stringsAsFactors=FALSE)
        yearlyTPR$Year <- YEAR[i]
        ori.TPR <- rbind(ori.TPR, yearlyTPR)
}

write.csv(ori.TPR, file = "./CSR/Original TPR.csv", row.names = F)

workingTPR <- ori.TPR[, 1:17]
for (i in 1:length(YEAR)){
        tpr = vector()
        for (n in 1:nrow(ori.TPR)){
                if (ori.TPR[n, 40] == YEAR[i]){
                        tpr[n] <- ori.TPR[n, 39]
                }
                else{
                        tpr[n] <- NA
                }
        }
        workingTPR[,as.character(YEAR[i])] <- tpr
}

workingTPR <- workingTPR[order(workingTPR[,1]),]
write.csv(workingTPR, file = "./CSR/Combined TPR.csv", row.names = F)


combineTPR <- workingTPR[,c(1, 4, 5, 18:27)]
dt <- data.table(combineTPR)
setkeyv(dt, c("Player.Code", "Last.Name", "First.Name"))
sumDT <- dt[, lapply(.SD, na.omit), by = key(dt)]
combineTPR <- data.frame(sumDT)
combineTPR$Sum.TPR <- rowSums(combineTPR[, 4:13], na.rm = T)
combineTPR <- combineTPR[order(-combineTPR$Sum.TPR),]
write.csv(combineTPR, file = "./CSR/Cumulated TPR.csv", row.names = F)

x<-combineTPR[combineTPR$Player.Code %in% names(which(table(combineTPR$Player.Code)!=1)),]
x<-x[order(x$Player.Code),]
write.csv(x, file = "./CSR/Name Changed.csv", row.names = F)
