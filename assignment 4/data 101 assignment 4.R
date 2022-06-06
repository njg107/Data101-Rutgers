hist(Movies2022F.4$imdb_score, breaks = 100, main = 'imdb score histogram', xlab = 'imdb score')
boxplot(Movies2022F.4$imdb_score~Movies2022F.4$Budget, xlab = "budget", ylab = "imdb score", col = 'yellow')
boxplot(Movies2022F.4$imdb_score~Movies2022F.4$genre, xlab = "genre", ylab = "imdb score", col = 'blue', border = 'pink')
boxplot(Movies2022F.4$imdb_score~Movies2022F.4$content, xlab = 'content', ylab = 'imdb score', col = 'blue', border = 'orange')



mosaicplot(Movies2022F.4$content~Movies2022F.4$genre,xlab = 'CONTENT',ylab = 'GENRE', main = "content vs genre",border="black")
mosaicplot(Movies2022F.4$content~Movies2022F.4$Budget,xlab = 'CONTENT',ylab = 'BUDGET', main = "content vs budget",border="black")
mosaicplot(Movies2022F.4$Gross~Movies2022F.4$genre,xlab = 'GROSS',ylab = 'GENRE', main = "gross vs genre",border="black")
