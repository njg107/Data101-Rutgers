#family vs history
familydata <- subset(Movies2022F.4, Movies2022F.4$genre == "Family")
historydata <- subset(Movies2022F.4, Movies2022F.4$genre == "History")

#ratings of family genre
familyscores <- familydata$imdb_score

#ratings of history genre
historyscores <- historydata$imdb_score

# standard deviation of two samples.
familySD <- sd(familyscores)
historySD <- sd(historyscores)

#length of family and history datasets
familylen <- length(familyscores)
historylen <- length(historyscores)

#standard deviation of two score sets
SDFamHis <- sqrt(familySD^2/familylen + historySD^2/historylen)
SDFamHis

#means of two samples
familyMean <- mean(familyscores)
historyMean <- mean(historyscores)
familyMean
historyMean

#z score
zeta <- (historyMean - familyMean)/SDFamHis

#plot red lines
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zeta, col='red')

#get p
p = 2*pnorm(q=zeta, lower.tail=FALSE) #1-pnorm(zeta)
p



#action vs comedy
actiondata <- subset(Movies2022F.4, Movies2022F.4$genre == "Action")
comedydata <- subset(Movies2022F.4, Movies2022F.4$genre == "Comedy")

#ratings of family genre
actionscores <- actiondata$imdb_score

#ratings of history genre
comedyscores <- comedydata$imdb_score

# standard deviation of two samples.
actionSD <- sd(actionscores)
comedySD <- sd(comedyscores)

#length of family and history datasets
actionlen <- length(actionscores)
comedylen <- length(comedyscores)

#standard deviation of two score sets
SDactcom <- sqrt(actionSD^2/actionlen + comedySD^2/comedylen)
SDdramcom

#means of two samples
actionMean <- mean(actionscores)
comedyMean <- mean(comedyscores)
actionMean
comedyMean

#z score
zeta <- (actionMean - comedyMean)/SDdramcom

#plot red lines
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zeta, col='red')

#get p
p = 2*pnorm(q=zeta, lower.tail=FALSE) #1-pnorm(zeta)
p





#drama vs comedy
dramadata <- subset(Movies2022F.4, Movies2022F.4$genre == "Drama")
comedydata <- subset(Movies2022F.4, Movies2022F.4$genre == "Comedy")

#ratings of family genre
dramascores <- dramadata$imdb_score

#ratings of history genre
comedyscores <- comedydata$imdb_score

# standard deviation of two samples.
dramaSD <- sd(dramascores)
comedySD <- sd(comedyscores)

#length of family and history datasets
dramalen <- length(dramascores)
comedylen <- length(comedyscores)

#standard deviation of two score sets
SDdramcom <- sqrt(dramaSD^2/dramalen + comedySD^2/comedylen)
SDdramcom

#means of two samples
dramaMean <- mean(dramascores)
comedyMean <- mean(comedyscores)
dramaMean
comedyMean

#z score
zeta <- (dramaMean - comedyMean)/SDdramcom

#plot red lines
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zeta, col='red')

#get p
p = 2*pnorm(q=zeta, lower.tail=FALSE) #1-pnorm(zeta)
p





