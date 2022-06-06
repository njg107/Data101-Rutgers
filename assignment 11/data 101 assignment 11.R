library(rpart)
library(rpart.plot)
library("randomForest")
library("e1071")

earnings <- Earnings_Train2022.1

#png(filename="Hello.png")
#dev.off()

#Columns: GPA(num), Number_Of_Professional_Connections(Num), Major(Num), Graduation_Year(Time), Number_Of_Credits(Num), Number_Of_Parking_Tickets(Num), Earnings(Num)

stemSub <- subset(earnings, earnings$Major == 'Buisness')

#---------------New-Columns-------------------------

#boxplot for relation between earnings and number of professional connections
earnings$Involvement <- earnings$Number_Of_Professional_Connections / (2021 - earnings$Graduation_Year)

decision <- rep(0,nrow(earnings))

decision[earnings$Major == "Humanities"] <- 1
decision[earnings$Major == "Vocational"] <- 2
decision[earnings$Major == "Professional"] <- 3
decision[earnings$Major == "Buisness"] <- 4
decision[earnings$Major == "Other"] <- 5

earnings$MajorNum <- decision

#--------------------------------------------------

#boxplot for relation between earnings and major

boxplot(earnings$Earnings ~ earnings$Major,
        col = c("#33FFFF", "green", "yellow", "orange", "red"),
        xlab = "Majors",
        ylab = "Earning")
#scatterplot for relation between earnings and gpa

plot(earnings$Earnings ~ earnings$GPA,
     xlab = "GPA",
     ylab = "Earning")
#scatterplot for relation between earnings and number of profesional connections
plot(stemSub$Earnings ~ stemSub$Number_Of_Professional_Connections,
     xlab = "Number of Professional Connections",
     ylab = "Earning")
#scatterplot for relation between earnings and professional experience
plot(earnings$Earnings ~ earnings$Involvement,
     xlab = "Involvement",
     ylab = "Experience")

#----------------------------------------------------



dt = sort(sample(nrow(earnings), nrow(earnings)*.8))

train <- earnings[dt,]
test <- earnings[-dt,]

#----------------------------------------------------

#---------------code-for-Decision-Tree------------------------

tree <- rpart(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year + Number_Of_Credits + Number_Of_Parking_Tickets, data = train, method = "anova")
rpart.plot(tree)

test$NewEarnings <- predict(tree, newdata = test)

error <- mean((test$NewEarnings - test$Earnings)^2)
error

#---------------------------------------------------

#---------------code-for-Linear-Regression--------------------

trainLm <- lm(Earnings ~ GPA + MajorNum + Number_Of_Parking_Tickets + Number_Of_Credits + Involvement,  data = train)
predLm <-  predict(trainLm, newdata = test)

error <- mean((predLm - test$Earnings)^2)
error

#---------------------------------------------------

#---------------Random-Forest-model-----------------------

trainRf <- randomForest(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year + Number_Of_Credits + Number_Of_Parking_Tickets,  data = train)
predRf <-  predict(trainRf, newdata = test)

error <- mean((predRf - test$Earnings)^2)
error

#---------------------------------------------------

#-------------------------SVM-----------------------

trainSvm = svm(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Number_Of_Parking_Tickets + Number_Of_Credits + Graduation_Year, data = train)
predSvm <-  predict(trainSvm, newdata = test)

error <- mean((predSvm - test$Earnings)^2)
error

#---------------------------------------------------

#---------------writing-to-file-and-final-submission--------------------------

id <- `earning_submission.(1)`
data <- Earnings_Test_Students
id$Earnings <- predict(trainSvm, newdata = data)

write.csv(id, file = "FinalSubmission.csv", row.names=FALSE, quote=FALSE)

#---------------------------------------------------