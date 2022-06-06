getwd()
GTP <- moody2022_new$SCORE*moody2022_new$PARTICIPATION  
boxplot(GTP ~ moody2022_new$GRADE, xlab = "Letter Grade", ylab = "Score Times Participation")

boxplot(moody2022_new$SCORE~moody2022_new$GRADE, xlab = "letter grade", ylab = "score")

plot(moody2022_new$SCORE, moody2022_new$PARTICIPATION, xlab = 'score', ylab = 'participation')

boxplot(moody2022_new$PARTICIPATION~moody2022_new$TEXTING_IN_CLASS, xlab = 'texting', ylab = 'participation')
