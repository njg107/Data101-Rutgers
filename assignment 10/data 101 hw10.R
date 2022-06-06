library(rpart)
library(rpart.plot)
install.packages("devtools")

summary(M2022train)

Dtree1 <- rpart(Grade ~ Score, data = M2022train,control=rpart.control(minsplit = 50, minbucket = 50)) 
rpart.plot(Dtree)

Dtree2 <- rpart(Grade ~ Score, data = M2022train, control = rpart.control(minbucket = 50))
rpart.plot(Dtree2)

Dtree3 <- rpart(Grade ~ Score, data = M2022train, control = rpart.control(minsplit = 50))
rpart.plot(Dtree3)

cross_validate(M2022train,Dtree1, 1, 0.8)

prediction<- predict(Dtree1, newdata=M2022testSNoGrade ,type="class")
M2022submissionS$Grade <- prediction
write.csv(M2022submissionS, file = "mysubmission.csv",row.names=FALSE)