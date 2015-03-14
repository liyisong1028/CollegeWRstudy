## Create wrstats function to calculate the wide receivers' yearly stats from 
## pass, player, team, conference tables. The weights arguments set weights for 
## receptions, yards and touchdown, and the f.point and t.limit set the minimum 
## targets and fantasy points required for receivers to be included 
## in the analysis. The default of weights sets to 1 for each reception, 0.1 for 
## each yard, and 3 for each touchdown. You can specify the weights by assigning
## the argument with a numeric vecter with length of 3. The default of minimum 
## targets is 10, and default of minimum fantasy points is 0.

wrstats <- function(pass, player, team, conference, weights = c(1, 0.1, 3), 
                    t.limit = 10, f.limit = 0) {
        wrPlayer <- player[player$Position == "WR",]
        wrPlayer <- merge(wrPlayer, team, by = "Team.Code", all.x = T)
        names(wrPlayer)[ncol(wrPlayer)-1] <- "Team.Name"
        wrPlayer <- merge(wrPlayer, conference, by = "Conference.Code", all.x = T)
        names(wrPlayer)[ncol(wrPlayer)-1] <- "Conference.Name"
        dt <- data.table(pass[,5:9])
        setkey(dt, "Receiver.Player.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        wrPlayer <- merge(wrPlayer, sumDT, by.x = "Player.Code", 
                          by.y = "Receiver.Player.Code")
        names(wrPlayer)[(ncol(wrPlayer)-3):ncol(wrPlayer)] <-
                c("Targets", "Receptions", "Yards", "Touchdowns")
        wrPlayer$Fantasy.Points <- wrPlayer$Receptions * weights[1] + 
                wrPlayer$Yards * weights[2] + wrPlayer$Touchdowns * weights[3]
        wrPlayer <- subset(wrPlayer, wrPlayer$Fantasy.Points >= f.limit & 
                                   wrPlayer$Targets >= t.limit &
                                   wrPlayer$Subdivision == "FBS")
        output <- wrPlayer[order(-wrPlayer$Fantasy.Points),]
        return(output)
}

## Create ypt function to calculate the adjusted stats because of yards per target from the wide receiver stats data. 
ypt <- function(wr, tier1 = 1, tier2 = 2, adjust1 = 0.025, adjust2 = 0.05){
        Yards.per.Target <- wr$Yards / wr$Targets
        std <- sd(Yards.per.Target)
        mean.ypt <- mean(Yards.per.Target)
        adjust.rate <- vector()
        for (i in 1:length(Yards.per.Target)){
                if (Yards.per.Target[i] > (mean.ypt + tier2 * std)) {
                        adjust.rate[i] = adjust2 
                }
                else if (Yards.per.Target[i] > (mean.ypt + tier1 * std) & 
                                 Yards.per.Target[i] <= (mean.ypt + tier2 * std)){
                        adjust.rate[i] = adjust1
                }
                else if (Yards.per.Target[i] < (mean.ypt - tier1 * std) &
                                 Yards.per.Target[i] >= (mean.ypt - tier2 * std)){
                        adjust.rate[i] = -adjust1
                }
                else if (Yards.per.Target[i] < (mean.ypt - tier2 * std)){
                        adjust.rate[i] = -adjust2
                }
                else{
                        adjust.rate[i] = 0
                }
        }
        Adjusted.YPT.Receptions <- wr$Receptions * adjust.rate
        Adjusted.YPT.Yards <- wr$Yards * adjust.rate
        Adjusted.YPT.Touchdowns <- wr$Touchdowns * adjust.rate
        Adjusted.YPT.Fantasy.Points <- wr$Fantasy.Points * adjust.rate
        output <- data.frame(wr$Player.Code, Yards.per.Target, 
                             Adjusted.YPT.Receptions, Adjusted.YPT.Yards, 
                             Adjusted.YPT.Touchdowns, Adjusted.YPT.Fantasy.Points)
        return(output)
}

## Create thirdDown function to calculate the wide receivers' third down conversion stats from play, pass, player tables.
thirddown <- function(play, pass, wr, weights = c(1, 0.1, 3), third = 0, 
                      forth = 0, adjust = 0.05){
        play$Unique.Code <- paste(play$Game.Code, play$Play.Number, sep = "_")
        pass$Unique.Code <- paste(pass$Game.Code, pass$Play.Number, sep = "_")
        passplay <- merge(pass, play[,c(9,10,15)],by = "Unique.Code")
        passingdown <- (passplay$Down == 3 & passplay$Distance >= third)|
                (passplay$Down == 4 & passplay$Distance >= forth)
        passconversion <- passplay[passplay$X1st.Down == 1 & passingdown, ]
        dt <- data.table(passconversion[,c(6,8,9,10)])
        setkey(dt, "Receiver.Player.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        setnames(sumDT, 2:4, c("Adjusted.ThirdDown.Receptions", 
                               "Adjusted.ThirdDown.Yards", 
                               "Adjusted.ThirdDown.Touchdowns"))
        output <- data.frame(subset(sumDT, sumDT$Receiver.Player.Code %in% 
                                            wr$Player.Code))
        output$Adjusted.ThirdDown.Receptions <- output[,2] * adjust
        output$Adjusted.ThirdDown.Yards <- output[,3] * adjust
        output$Adjusted.ThirdDown.Touchdowns <- output[,4] * adjust
        output$Adjusted.ThirdDown.Fantasy.Points <- output[,2] * weights[1] +
                output[,3] * weights[2] + output[,4] * weights[2]
        return(output)
}

## Create defensegame function to calculate the pass defense stats of every game. Part of the defense algorithm code, don't need to be called individually.
defensegame <- function(play, pass){
        play$Unique.Code <- paste(play$Game.Code, play$Play.Number, sep = "_")
        pass$Unique.Code <- paste(pass$Game.Code, pass$Play.Number, sep = "_")
        passd <- merge(pass, play[,c(6, 15)],by = "Unique.Code")
        dt <- data.table(passd[,c(2, 7:14)])
        setkeyv(dt, c("Defense.Team.Code","Game.Code"))
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        output <- data.frame(sumDT)
        return(output)
}

## Create defense function to calculate defense adjustment rate of every team for every game with the stat of that game excluded. Part of the defense algorithm code, don't need to be called individually.

defensegameypp <- function(play, pass, g.limit = 1, tier1 = 1, tier2 = 2, 
                           adjust1 = 0.1, adjust2 = 0.2){
        df <- defensegame(play, pass)
        dt <- data.table(df[,c(1,3,5)])
        setkey(dt, "Defense.Team.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        sumDF <- data.frame(sumDT)
        sumDF$Yards.per.Pass <- sumDF$Yards / sumDF$Attempt
        std <- sd(sumDF$Yards.per.Pass)
        mean.ypp <- mean(sumDF$Yards.per.Pass)
        df <- merge(df, sumDF, by = "Defense.Team.Code", all.x = T)
        df$Adjusted.Attempt <- df$Attempt.y - df$Attempt.x
        df$Adjusted.Yards <- df$Yards.y - df$Yards.x
        df$Adjusted.Yards.per.Pass <- df$Adjusted.Yards / df$Adjusted.Attempt
        df[is.na(df$Adjusted.Yards.per.Pass), 15] <- mean.ypp
        adjust.rate <- vector()
        for (i in 1:nrow(df)){
                if (df[i, 15] > (mean.ypp + tier2 * std)){
                        adjust.rate[i] = -adjust2
                }
                else if (df[i, 15] > (mean.ypp + tier1 * std) & 
                                 df[i, 15] <= (mean.ypp + tier2 * std)){
                        adjust.rate[i] = -adjust1
                }
                else if (df[i, 15] < (mean.ypp - tier1 * std) & 
                                 df[i, 15] >= (mean.ypp - tier2 * std)){
                        adjust.rate[i] = adjust1
                }
                else if (df[i, 15] < (mean.ypp - tier2 * std)){
                        adjust.rate[i] = adjust2
                }
                else{
                        adjust.rate[i] = 0
                }
        }
        df$Adjust.Rate <- adjust.rate
        output <- df[,c(1:2,16)]
        return(output)
}

## Create defense function to calculate the adjusted WR stats according to the defense team
defense <- function(wr, play, pass, weights = c(1, 0.1, 3), g.limit = 1, tier1 = 1, tier2 = 2, 
                    adjust1 = 0.1, adjust2 = 0.2) {
        df <- defensegameypp(play, pass, g.limit, tier1, tier2, adjust1, adjust2)
        passd <- merge(pass, play[,c(1, 2, 6)],by = c("Game.Code", "Play.Number"))
        passdadj <- merge(passd, df, by = c("Defense.Team.Code", "Game.Code"), all.x = T)
        passdadj$Adjusted.Defense.Receptions <- passdadj$Completion * passdadj$Adjust.Rate
        passdadj$Adjusted.Defense.Yards <- passdadj$Yards * passdadj$Adjust.Rate
        passdadj$Adjusted.Defense.Touchdown <- passdadj$Touchdown * passdadj$Adjust.Rate
        dt <- data.table(passdadj[,c(6, 15:17)])
        setkey(dt, "Receiver.Player.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        output <- data.frame(sumDT)
        output$Adjusted.Defense.Fantasy.Point <- output[,2] * weights[1] + 
                output[,3] * weights[2] + output[,4] * weights[3]
        return(output)
}

## Create the TPR function to call all the algorithm, and output the final WR stats. No need to change the parameters right now, default should be fine.
TPR <- function(play, pass, player, team, conference, weights = c(1, 0.1, 3), 
                t.limit = 10, f.limit = 0, g.limit = 1, ypt.tier1 = 1, 
                ypt.tier2 = 2, ypt.adjust1 = 0.025, ypt.adjust2 = 0.05, 
                third = 0, forth = 0, t.adjust = 0.05, d.tier1 = 1, d.tier2 = 2, 
                d.adjust1 = 0.1, d.adjust2 = 0.2){
        wr <- wrstats(pass, player, team, conference, weights, t.limit, f.limit)
        yptadj <- ypt(wr, ypt.tier1, ypt.tier2, ypt.adjust1, ypt.adjust2)
        tadj <- thirddown(play, pass, wr, weights, third, forth, t.adjust)
        dadj <- defense(wr, play, pass, weights, g.limit, d.tier1, d.tier2, 
                        d.adjust1, d.adjust2)
        final <- merge(wr,yptadj, by.x = "Player.Code", by.y = "wr.Player.Code", all.x = T)
        final <- merge(final, tadj, by.x = "Player.Code", by.y = "Receiver.Player.Code", all.x = T)
        final <- merge(final, dadj, by.x = "Player.Code", by.y = "Receiver.Player.Code", all.x = T)
        final$TPR <- final$Fantasy.Points + final$Adjusted.YPT.Fantasy.Points + 
                final$Adjusted.ThirdDown.Fantasy.Points + 
                final$Adjusted.Defense.Fantasy.Point
        final <- final[order(-final$TPR),]
        rownames(final) <- NULL
        return(final)
}