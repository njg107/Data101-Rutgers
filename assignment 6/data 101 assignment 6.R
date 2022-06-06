ranNum <- sample(1:nrow(Movies2022F.4),nrow(Movies2022F.4))
ranNum[1:5]
imdb_score<-Movies2022F.4$imdb_score[ranNum]
genre<-Movies2022F.4$genre
Permuted_Movies<-data.frame(genre, imdb_score)
mean(Movies2022F.4[Movies2022F.4$genre=='History', ]$imdb_score) -mean(Movies2022F.4[Movies2022F.4$genre=='Family', ]$imdb_score)
mean(Permuted_Movies[Permuted_Movies$genre=='History', ]$imdb_score)-mean(Permuted_Movies[Permuted_Movies$genre=='Family', ]$imdb_score)

HistoryScores <- Permuted_Movies[Permuted_Movies$genre=='History', ]$imdb_score[ranNum]
FamilyScores <- Permuted_Movies[Permuted_Movies$genre=='Family', ]$imdb_score[ranNum]
original <- diff(tapply(FamilyScore, HistoryScores, mean))

permutation.test <- function(HistoryScores, FamilyScore, n){
  distribution=c()
  result=0
  for(i in 1:n){
    distribution[i]=diff(by(FamilyScore, sample(HistoryScores, length(HistoryScores), FALSE), mean))
  }
  result=sum(abs(distribution) >= abs(original))/(n)
  return(list(result, distribution))
}

test1 <- permutation.test(HistoryScores, FamilyScore, 10000)
hist(test1[[2]], breaks=50, col='grey', main="Permutation Distribution", las=1, xlab='')
abline(v=original, lwd=3, col="red")

test1[[1]]