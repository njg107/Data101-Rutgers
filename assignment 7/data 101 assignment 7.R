sightings <- read.csv("Sightings.csv")#Read Sightings Csv.

#png(filename="Hello.png")
#dev.off()

evening <- subset(sightings, sightings$timing == 'Evening' & !is.na(sightings$tigercount))

morning <- subset(sightings, sightings$timing == 'Morning' & !is.na(sightings$tigercount))

boxplot(evening$tigercount, evening$leopardcount, evening$elephantcount,
        main = "Average Number of Animal Spotted in the Evening", 
        col = c("#33FFFF", "green", "yellow", "orange", "red"),
        xlab = "Animal",
        ylab = "Frequency",
        names = c("Tiger", "Leopard", "Elephant")
)

boxplot(morning$tigercount, morning$leopardcount, morning$elephantcount,
        main = "Average Number of Animal Spotted in the Morning", 
        col = c("#33FFFF", "green", "yellow", "orange", "red"),
        xlab = "Animal",
        ylab = "Frequency",
        names = c("Tiger", "Leopard", "Elephant")
)

#---------------------------------------------------------------------
tigerMorMean <- mean(morning$tigercount)
tigerEveMean <- mean(evening$tigercount)

tigerMorSD <- sd(morning$tigercount)
tigerEveSD <- sd(evening$tigercount)

tigerMorLength <- length(morning$tigercount)
tigerEveLength <- length(evening$tigercount)

bothSD <- sqrt((tigerMorSD^2) / tigerMorLength + (tigerEveSD^2) / tigerEveLength)

Zscore <- (tigerMorMean - tigerEveMean) / bothSD

p <- (1 - pnorm(Zscore)) / 2
#---------------------------------------------------------------------
leopardMorMean <- mean(morning$leopardcount)
leopardEveMean <- mean(evening$leopardcount)

leopardMorSD <- sd(morning$leopardcount)
leopardEveSD <- sd(evening$leopardcount)

leopardMorLength <- length(morning$leopardcount)
leopardEveLength <- length(evening$leopardcount)

bothSD <- sqrt((leopardMorSD^2) / leopardMorLength + (leopardEveSD^2) / leopardEveLength)

Zscore <- (leopardMorMean - leopardEveMean) / bothSD

p <- (1 - pnorm(Zscore)) / 2
#---------------------------------------------------------------------
elephantMorMean <- mean(morning$elephantcount)
elephantEveMean <- mean(evening$elephantcount)

elephantMorSD <- sd(morning$elephantcount)
elephantEveSD <- sd(evening$elephantcount)

elephantMorLength <- length(morning$elephantcount)
elephantEveLength <- length(evening$elephantcount)

bothSD <- sqrt((elephantMorSD^2) / elephantMorLength + (elephantEveSD^2) / elephantEveLength)

Zscore <- (elephantMorMean - elephantEveMean) / bothSD

p <- (1 - pnorm(Zscore)) / 2