## help function cutting take a vector, tiers and adjusts to cut vector by tiers
## of standard deviation, and return the adjust rate by giving the highest tier
## with value adjust2, and lowest tier with negative value adjust2. The reverse
## argument will indicate whether to assign the adjust rate with opposite value.
## The na.replace will indicate the adjust rate assigned to NAs.
cutting <- function(benchmark, vec, tier1 = 1, tier2 = 2, adjust1 = 0.1, 
                    adjust2 = 0.2, reverse = F, na.replace = 0) {
        std <- sd(benchmark, na.rm = T)
        mn <- mean(benchmark, na.rm = T)
        adjust.rate = vector()
        if (reverse){
                adjust1 <- -adjust1
                adjust2 <- -adjust2
        }
        for (i in 1:length(vec)){
                if (is.na(vec[i])){
                        adjust.rate[i] = na.replace
                }
                else if (vec[i] > (mn + tier2 * std)) {
                        adjust.rate[i] = adjust2 
                }
                else if (vec[i] > (mn + tier1 * std) & 
                                 vec[i] <= (mn + tier2 * std)){
                        adjust.rate[i] = adjust1
                }
                else if (vec[i] < (mn - tier1 * std) &
                                 vec[i] >= (mn - tier2 * std)){
                        adjust.rate[i] = -adjust1
                }
                else if (vec[i] < (mn - tier2 * std)){
                        adjust.rate[i] = -adjust2
                }
                else{
                        adjust.rate[i] = 0
                }
        }
        return(adjust.rate)
}

## Create wrstats function to calculate the wide receivers' yearly stats from 
## pass, player, team, conference tables. The weights arguments set weights for 
## receptions, yards and touchdown, and the f.point and t.limit set the minimum 
## targets and fantasy points required for receivers to be included 
## in the analysis. The default of weights sets to 1 for each reception, 0.1 for 
## each yard, and 3 for each touchdown. You can specify the weights by assigning
## the argument with a numeric vecter with length of 3. The default of minimum 
## targets is 10, and default of minimum fantasy points is 0.

wrstats <- function(pass, player, team, conference, weights = c(1, 0.1, 3)) {
        wrPlayer <- player[player$Position == "WR",]
        wrPlayer <- merge(wrPlayer, team, by = "Team.Code", all.x = T)
        names(wrPlayer)[ncol(wrPlayer)-1] <- "Team.Name"
        wrPlayer <- merge(wrPlayer, conference, by = "Conference.Code", all.x = T)
        names(wrPlayer)[ncol(wrPlayer)-1] <- "Conference.Name"
        dt <- data.table(pass[,5:9])
        setkey(dt, "Receiver.Player.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        wrPlayer <- merge(wrPlayer, sumDT, by.x = "Player.Code", 
                          by.y = "Receiver.Player.Code", all.x = T)
        names(wrPlayer)[(ncol(wrPlayer)-3):ncol(wrPlayer)] <-
                c("Targets", "Receptions", "Yards", "Touchdowns")
        wrPlayer$Fantasy.Points <- wrPlayer$Receptions * weights[1] + 
                wrPlayer$Yards * weights[2] + wrPlayer$Touchdowns * weights[3]
        wrPlayer <- subset(wrPlayer, wrPlayer$Subdivision == "FBS" &
                                   !is.na(wrPlayer$Player.Code))
        wrPlayer[is.na(wrPlayer$Targets),18:22] <- 0
        output <- wrPlayer[order(-wrPlayer$Fantasy.Points),]
        return(output)
}

## Create ypt function to calculate the adjusted stats because of yards per target from the wide receiver stats data. 
ypt <- function(wr, tier1 = 1, tier2 = 2, adjust1 = 0.025, adjust2 = 0.05,
                t.limit = 10){
        eligible.wr <- subset(wr, wr$Targets >= t.limit)
        benchmark <- eligible.wr$Yards / eligible.wr$Targets
        Yards.per.Target <- wr$Yards / wr$Targets
        Adjust.Rate <- cutting(benchmark, Yards.per.Target, tier1, tier2,
                                      adjust1, adjust2)
        wr$Adjusted.YPT.Receptions <- wr$Receptions * Adjust.Rate
        wr$Adjusted.YPT.Yards <- wr$Yards * Adjust.Rate
        wr$Adjusted.YPT.Touchdowns <- wr$Touchdowns * Adjust.Rate
        wr$Adjusted.YPT.Fantasy.Points <- wr$Fantasy.Points * Adjust.Rate
        output <- wr[, c(1,23:26)]
        return(output)
}

## Create thirdDown function to calculate the wide receivers' third down conversion stats from play, pass, player tables.
thirddown <- function(play, pass, wr, weights = c(1, 0.1, 3), third = 0, 
                      forth = 0, adjust = 0.10){
        passplay <- merge(pass, play[,c(1,2,9,10)],by = c("Game.Code", "Play.Number"))
        passingdown <- (passplay$Down == 3 & passplay$Distance >= third)|
                (passplay$Down == 4 & passplay$Distance >= forth)
        passconversion <- passplay[passplay$X1st.Down == 1 & passingdown, ]
        dt <- data.table(passconversion[,c(5,7,8,9)])
        setkey(dt, "Receiver.Player.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        setnames(sumDT, 2:4, c("Adjusted.ThirdDown.Receptions", 
                               "Adjusted.ThirdDown.Yards", 
                               "Adjusted.ThirdDown.Touchdowns"))
        output <- merge(wr, data.frame(sumDT), by.x = "Player.Code", 
                        by.y = "Receiver.Player.Code", all.x = T)
        output <- output[,c(1,23:25)]
        output[is.na(output$Adjusted.ThirdDown.Receptions),2] <- 0
        output[is.na(output$Adjusted.ThirdDown.Yards),3] <- 0
        output[is.na(output$Adjusted.ThirdDown.Touchdowns),4] <- 0
        output$Adjusted.ThirdDown.Receptions <- output[,2] * adjust
        output$Adjusted.ThirdDown.Yards <- output[,3] * adjust
        output$Adjusted.ThirdDown.Touchdowns <- output[,4] * adjust
        output$Adjusted.ThirdDown.Fantasy.Points <- output[,2] * weights[1] +
                output[,3] * weights[2] + output[,4] * weights[3]
        return(output)
}

## defensegame function to calculate the pass defense stats of every game. Part of the defense algorithm code, don't need to be called individually.
defensegame <- function(play, pass, team, conference){
        passd <- merge(pass, play[,c(1:2, 6)], by = c("Game.Code", "Play.Number"))
        dt <- data.table(passd[,c(1, 6:13)])
        setkeyv(dt, c("Defense.Team.Code","Game.Code"))
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        output <- data.frame(sumDT)
        return(output)
}

## defensegameypp function to calculate defense adjustment rate of every team for every game with the stat of that game excluded. Part of the defense algorithm code, don't need to be called individually.
defenseypp <- function(play, pass, team, conference, one.game = -0.1, tier1 = 1,
                           tier2 = 2, adjust1 = 0.1, adjust2 = 0.2){
        df <- defensegame(play, pass, team, conference)
        dt <- data.table(df[,c(1,3,5)])
        setkey(dt, "Defense.Team.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        sumDF <- data.frame(sumDT)
        names(sumDF)[2:3] <- c("Total.Attempt", "Total.Yards")
        sumDF$Yards.per.Pass <- sumDF$Total.Yards / sumDF$Total.Attempt
        teamc <- merge(team, conference[,c(1,3)], by = "Conference.Code", 
                       all.x = T)
        sumDF <- merge(sumDF, teamc, by.x = "Defense.Team.Code", 
                        by.y = "Team.Code", all.x = T)
        FBSdefense <- sumDF[sumDF$Subdivision == "FBS", 4]
        df <- merge(df, sumDF, by = "Defense.Team.Code", all.x = T)
        Adjusted.Attempt <- df$Total.Attempt - df$Attempt
        Adjusted.Yards <- df$Total.Yards - df$Yards
        Adjusted.ypp <- Adjusted.Yards / Adjusted.Attempt
        df$Adjust.Rate <- cutting(FBSdefense, Adjusted.ypp, tier1, tier2, 
                                  adjust1, adjust2, reverse = T, 
                                  na.replace = one.game)
        output <- df[,c(1:2, 16)]
        return(output)
}

## Create defense function to calculate the adjusted WR stats according to the defense team
defense <- function(play, pass, team, conference, weights = c(1, 0.1, 3),
                    one.game = -0.1,  tier1 = 1, tier2 = 2, adjust1 = 0.1,
                    adjust2 = 0.2) {
        df <- defenseypp(play, pass, team, conference, one.game, tier1, tier2, 
                         adjust1, adjust2)
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

## QB function calculates the adjusted points for every receivers based on the qb EPA
QB <- function(pass, qb, weights = c(1, 0.1, 3), tier1 = 1, tier2 = 2, 
               adjust1 = 0.1, adjust2 = 0.2){
        qb <- subset(qb, !is.na(qb$Player.Code) & !is.na(qb$EPA))
        adjust.rate <- cutting(qb$EPA, qb$EPA, tier1, tier2, adjust1, 
                               adjust2, reverse = T)
        qb$Adjust.Rate <- adjust.rate
        qbepa <- subset(qb, select = c(Player.Code, EPA, Adjust.Rate))
        passqb <- merge(pass, qbepa, by.x = "Passer.Player.Code", 
                        by.y = "Player.Code", all.x = T)
        passqb[is.na(passqb$Adjust.Rate), "Adjust.Rate"] <- 0.0
        passqb$Adjusted.QB.Receptions <- passqb$Completion * passqb$Adjust.Rate
        passqb$Adjusted.QB.Yards <- passqb$Yards * passqb$Adjust.Rate
        passqb$Adjusted.QB.Touchdown <- passqb$Touchdown * passqb$Adjust.Rate
        dt <- data.table(passqb[,c(5, 15:17)])
        setkey(dt, "Receiver.Player.Code")
        sumDT <- dt[, lapply(.SD,sum), by = key(dt)]
        output <- data.frame(sumDT)
        output$Adjusted.QB.Fantasy.Point <- output[,2] * weights[1] + 
                output[,3] * weights[2] + output[,4] * weights[3]
        return(output)
}


## Create the TPR function to call all the algorithm, and output the final WR stats. No need to change the parameters right now, default should be fine.
TPR <- function(play, pass, player, team, conference, qb, weights = c(1, 0.1, 3), 
                t.limit = 0, ypt.tier1 = 1, ypt.tier2 = 2, ypt.adjust1 = 0.025, 
                ypt.adjust2 = 0.05, third = 0, forth = 0, t.adjust = 0.10, 
                one.game = -0.1, d.tier1 = 1, d.tier2 = 2, d.adjust1 = 0.1, 
                d.adjust2 = 0.2, qb.tier1 = 1, qb.tier2 = 2, qb.adjust1 = 0.1, 
                qb.adjust2 = 0.2){
        wr <- wrstats(pass, player, team, conference, weights)
        yptadj <- ypt(wr, ypt.tier1, ypt.tier2, ypt.adjust1, ypt.adjust2, t.limit)
        tadj <- thirddown(play, pass, wr, weights, third, forth, t.adjust)
        dadj <- defense(play, pass, team, conference, weights, one.game, 
                        d.tier1, d.tier2, d.adjust1, d.adjust2)
        qbadj <- QB(pass, qb, weights, qb.tier1, qb.tier2, qb.adjust1, qb.adjust2)
        final <- merge(wr, yptadj, by = "Player.Code", all.x = T)
        final <- merge(final, tadj, by = "Player.Code", all.x = T)
        final <- merge(final, dadj, by.x = "Player.Code", 
                       by.y = "Receiver.Player.Code", all.x = T)
        final <- merge(final, qbadj, by.x = "Player.Code", 
                       by.y = "Receiver.Player.Code", all.x = T)
        final[is.na(final$Adjusted.Defense.Receptions),31:38] <- 0
        final$TPR <- final$Fantasy.Points + final$Adjusted.YPT.Fantasy.Points + 
                final$Adjusted.ThirdDown.Fantasy.Points + 
                final$Adjusted.Defense.Fantasy.Point + 
                final$Adjusted.QB.Fantasy.Point
        final <- final[order(-final$TPR),]
        rownames(final) <- NULL
        return(final)
}