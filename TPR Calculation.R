## Check the package availability. Automatically install the required package if
## it hasn't been installed. It may take a little time.
if (!"data.table" %in% (installed.packages())) {
        install.packages("data.table")
}

## Load in the library used for this project
library(data.table)

## Set the constant used for this program. The STARTYEAR is the start year of
## the available WR data, the ENDYEAR is the end year of the available WR data.
## The DATAPATH is the folder paths for each year's data. I recommend to put all
## the data folders under a "data" folder under the working directory. 
STARTYEAR <- 2005; ENDYEAR <- 2014
YEAR <- STARTYEAR:ENDYEAR
DATAPATH <- c("./data/collegefootballdata.org-2005-1.2.0/",
              "./data/collegefootballdata.org-2006-1.5.0/",
              "./data/collegefootballdata.org-2007-1.5.0/", 
              "./data/collegefootballdata.org-2008-1.5.0/", 
              "./data/collegefootballdata.org-2009-1.5.0/", 
              "./data/collegefootballdata.org-2010-1.5.0/", 
              "./data/collegefootballdata.org-2011-1.5.0/", 
              "./data/collegefootballdata.org-2012/", 
              "./data/collegefootballdata.org-2013/", 
              "./data/collegefootballdata.org-2014/")

## WD is the path of your working directory, where you put the "data" folder and
## the "Function script.R" file. Please change the path according the setup of 
## your computer. Be careful about the format. If you are not sure what is the
## right format, you can use the file.path function. Be sure to use ?file.path
## call the help page and read how to use it.
WDPATH <- "D:/Baidu Cloud/Crosby MBA Program 2015 Spring/BA 8600 - Consulting/R programing/"

## Set the working directory according the "WD" constant
setwd(WDPATH)

## source the "Function script.R" from the working directory. Read in the 
## algorithm functions used for the calculation
source("./Function script.R")

## Creat a "TPR" folder under the working directory to store the generated TPR
## files
dir.create("./TPR", showWarnings = FALSE)

## Loop through all the data by years and automatically generate TPR file for
## each year under the folder "TPR" that created earlier.
for (i in 1:length(DATAPATH)){
        play <- read.csv(paste(DATAPATH[i], "play.csv", sep = ""))
        pass <- read.csv(paste(DATAPATH[i], "pass.csv", sep = ""))
        player <- read.csv(paste(DATAPATH[i], "player.csv", sep = ""))
        team <- read.csv(paste(DATAPATH[i], "team.csv", sep = ""))
        conference <- read.csv(paste(DATAPATH[i], "conference.csv", sep = ""))
        qb <- read.csv(paste("./data/QB_EPA/QB_EPA_", YEAR[i], ".csv", sep = ""))
        output <- TPR(play, pass, player, team, conference, qb)
        filePath <- paste("./TPR/WR TPR ", YEAR[i], ".csv", sep = "")
        write.csv(output, file = filePath, row.names = F)
}