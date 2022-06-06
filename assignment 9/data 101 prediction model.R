moody <- read.csv("M2022train.csv")#Read Csv.
#-----Splitting-The-Data--------------------------

dt = sort(sample(nrow(moody), nrow(moody)*.8))

train <- moody[dt,]
test <- moody[-dt,]

#-----------------Stats----------------------
SubP <- subset(train, train$Major == 'Statistics')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubP$Score ~ SubP$Grade,
main = "Score for Each Grade Attribute Ssss", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubP$Grade ~ SubP$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

#-----------------Psychology----------------------
SubP <- subset(train, train$Major == 'Psychology')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubF$Score ~ SubF$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubF$Grade ~ SubF$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

#-----------------CS----------------------

SubP <- subset(train, train$Major == 'CS')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubP$Score ~ SubP$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubP$Grade ~ SubP$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

#-----------------Economics----------------------

SubP <- subset(train, train$Major == 'Economics')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubP$Score ~ SubP$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubP$Grade ~ SubP$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

#-----------------Overview-Of-Graphs-----------------------------
mosaicplot(train$Grade ~ train$Major,
main = "Major and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Major Attributes")

boxplot(train$Score ~ train$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(train$Grade ~ train$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

#---------------------Prediction-Model-------------------------------

library(rpart)
library(rpart.plot)
library(CrossValidation)

moody <- M2022train
testMoody <- M2022testSNoGrade.csv

dt = sort(sample(nrow(moody), nrow(moody)*.8))

train <- moody[dt,]
test <- moody[-dt,]


#------------------Model--------------------------------

Predict <- test
decision <- rep('R',nrow(Predict))

decision[test$Score > 90] = 'A'
decision[test$Score < 85 & test$Score > 70] = 'B'
decision[test$Score < 70 & test$Score > 55] = 'C'
decision[test$Score < 55 & test$Score > 40] = 'D'
decision[test$Score < 40]  = 'F'
decision[test$Score > 85 & test$Major != 'Economics' ] = 'A'
decision[test$Score < 85 & test$Score > 70 & test$Major != 'Economics'] = 'B'
decision[test$Score < 70 & test$Score > 55 & test$Major != 'Economics'] = 'C'
decision[test$Score < 55 & test$Score > 40 & test$Major != 'Economics'] = 'D'
decision[test$Score < 40 & test$Major != 'Economics'] = 'F'
decision[test$Score >= 90 & test$Major != 'CS' ] = 'A'
decision[test$Score < 90 & test$Score > 85 & test$Major != 'CS'] = 'B'
decision[test$Score < 85 & test$Score > 70 & test$Major != 'CS'] = 'C'
decision[test$Score < 70 & test$Score > 50 & test$Major != 'CS'] = 'D'
decision[test$Score < 50 & test$Major != 'CS'] = 'F'

Predict$GradePredict <- decision
error <- mean(Predict$Grade != Predict$GradePredict)
error
#-------------------------------------------------------

#---------------Writing-Test-Data-File------------------

Submission <- M2022submissionS

Predict <- Submission
decision <- rep('R',nrow(Predict))

decision[Predict$Score > 90] = 'A'
decision[Predict$Score < 85 & Predict$Score > 70] = 'B'
decision[Predict$Score < 70 & Predict$Score > 55] = 'C'
decision[Predict$Score < 55 & Predict$Score > 40] = 'D'
decision[Predict$Score < 40]  = 'F'
decision[Predict$Score > 85 & Predict$Major != 'Economics' ] = 'A'
decision[Predict$Score < 85 & Predict$Score > 70 & Predict$Major != 'Economics'] = 'B'
decision[Predict$Score < 70 & Predict$Score > 55 & Predict$Major != 'Economics'] = 'C'
decision[Predict$Score < 55 & Predict$Score > 40 & Predict$Major != 'Economics'] = 'D'
decision[Predict$Score < 40 & Predict$Major != 'Economics'] = 'F'
decision[Predict$Score >= 90 & Predict$Major != 'CS' ] = 'A'
decision[Predict$Score < 90 & Predict$Score > 85 & Predict$Major != 'CS'] = 'B'
decision[Predict$Score < 85 & Predict$Score > 70 & Predict$Major != 'CS'] = 'C'
decision[Predict$Score < 70 & Predict$Score > 50 & Predict$Major != 'CS'] = 'D'
decision[Predict$Score < 50 & Predict$Major != 'CS'] = 'F'

Predict$Grade <- decision

Submission <- Predict[, c("X", "Grade")]

write.csv(Submission, file = "FinalSubmission.csv", row.names=FALSE, quote=FALSE)

#--------------------------------------------------------