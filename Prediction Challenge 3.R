#Prediction Challenge 3
  train <- read.csv("C:/Users/lpnhu/Downloads/Earnings_Numeric_Train-2023-1.csv")
library(rpart)
library(rpart.plot)
library(Metrics)
  
tree <- rpart(Earnings~.,data=train)
rpart.plot::rpart.plot(tree)
predict1 <- predict(tree, newdata = train)
predict1
mean((predict1-train$Earnings)^2)#MSE = 93608.69


#EDA
plot(train$GPA,train$Earnings) #No Particular relationship
plot(train$Number_Of_Professional_Connections, train$Earnings) #Seems to have some correlation
plot(train$Number_Of_Credits, train$Earnings)#No relationship
plot(train$Number_Of_Parking_Tickets, train$Earnings) #No correlation
boxplot(Earnings~Major, data= train)#Some relationship

tapply(train$Earnings,train$Major,mean)
library(randomForest)
 tree1 <- randomForest::randomForest(Earnings~., data= train)
 predict2 <- predict(tree1, newdata= train)
 mean((predict2-train$Earnings)^2) #MSE = 25914.06
 
 #Random subsetting based on Major
 unique(train$Major)
 business <- subset(train[train$Major== 'Buisness',])
 stem <- subset(train[train$Major== 'STEM',])
 human <- subset(train[train$Major=='Humanities',])
 voca <- subset(train[train$Major=='Vocational',])
 prof <- subset(train[train$Major== 'Professional',])
 other <- subset(train[train$Major=='Other',])
 
 #Creating new attributes
 train$gross <- train$Earnings - train$Number_Of_Parking_Tickets
 tree <- rpart(Earnings~.,data=train)
 rpart.plot::rpart.plot(tree)
 predict1 <- predict(tree, newdata = train)
 predict1
 mean((predict1-train$Earnings)^2)#MSE = 118933.2; adding attributes doesn't help

  #Regression for _business_  people
 
 plot(business$Earnings~business$GPA) #Some strong relationship?
 plot(business[business$Graduation_Year %% 2 == 0,]$Earnings~business[business$Graduation_Year %% 2 == 0,]$GPA) #STRONG relationship
 plot(business[business$Graduation_Year %% 2 != 0,]$Earnings~business[business$Graduation_Year %% 2 != 0,]$GPA) #STRONG relationship
 plot(business$Earnings~business$Number_Of_Professional_Connections) #all scattered
 plot(business$Earnings~business$Height) #all scattered
 plot(business$Earnings~business$Number_Of_Credits) #all scattered
 plot(business$Earnings~business$Number_Of_Parking_Tickets)
 
 
 
businesseven <- train[train$Graduation_Year %% 2 == 0 & train$Major == 'Buisness',]
businessodd <-train[train$Graduation_Year %% 2 != 0 & train$Major == 'Buisness',]

plot(businessodd$Earnings~businessodd$GPA)
#Interestingly, higher GPA, lower income
plot(businesseven$Earnings, businesseven$GPA)
#Inverse relationship for people who graduated in even years
b1 <- lm(Earnings ~ GPA, data = businesseven)
b2 <- lm(Earnings ~ GPA, data = businessodd)
p1 <- predict(b1,businesseven)
mean((p1 - businesseven$Earnings)^2) #VERT LOW MSE!
p2 <- predict(b2,businessodd)
mean((p2-businessodd$Earnings)^2) #VERT LOW MSE!

#Regression for _humanistic_ people
plot(human$Earnings~ human$GPA) #strong positive relationship!
plot(human$Earnings~human$Number_Of_Professional_Connections) #scattered
plot(human$Earnings~human$Graduation_Year) #scattered; not meaningful
plot(human$Earnings~human$Height) #scattered
plot(human$Earnings~human$Number_Of_Credits) #scattered
plot(human$Earnings~human$Number_Of_Parking_Tickets) #scattered; not meaningful


h1 <- lm(Earnings~GPA, data = human)
p3 <- predict(h1,human)
mean((p3-human$Earnings)^2) #MSE = 101.3284

#Regression for _professional_ people
plot(prof$Earnings~prof$GPA)#Strong, negative relationship
plot(prof$Earnings~prof$Number_Of_Professional_Connections) #scattered
plot(prof$Earnings~prof$Graduation_Year) #scattered
plot(prof$Earnings~prof$Height) #scattered
plot(prof$Earnings~prof$Number_Of_Credits) #scattered
plot(prof$Earnings~prof$Number_Of_Parking_Tickets) #scattered

pp1 <- lm(Earnings~GPA, data = prof)
p4 <- predict(pp1,prof)
mean((p4 - prof$Earnings)^2) #MSE = 100.2743

prof$num1 <- prof$GPA^2
pp2 <- lm(Earnings~GPA + num1, data = prof)
p4 <- predict(pp2,prof)
mean((p4 - prof$Earnings)^2) #MSE = 100.1156

#Regression for _STEM_ people
plot(stem$Earnings~stem$GPA)#Strong, negative relationship
plot(stem$Earnings~stem$Number_Of_Professional_Connections) #scattered
plot(stem$Earnings~stem$Graduation_Year) #scattered
plot(stem$Earnings~stem$Height) #scattered
plot(stem$Earnings~stem$Number_Of_Credits) #scattered
plot(stem$Earnings~stem$Number_Of_Parking_Tickets) #scattered

s1 <- lm(Earnings~GPA, data = stem)
p5 <- predict(s1, stem)
mean((p5 - stem$Earnings)^2) #MSE = 98.94409

#Regression for _vocational_ people
plot(voca$Earnings~voca$GPA)#Strong, positive relationship
plot(voca$Earnings~voca$Number_Of_Professional_Connections) #scattered
plot(voca$Earnings~voca$Graduation_Year) #scattered
plot(voca$Earnings~voca$Height) #scattered
plot(voca$Earnings~voca$Number_Of_Credits) #scattered
plot(voca$Earnings~voca$Number_Of_Parking_Tickets) #scattered

v1 <- lm(Earnings~GPA, data = voca)
p6 <- predict(v1, voca)
mean ((p6-voca$Earnings)^2) #MSE = 99.68931

voca$num1 <- voca$GPA^2
v2 <- lm(Earnings~GPA + num1, data = voca)
p66 <- predict(v2,voca)
mean((p66-voca$Earnings)^2) #MSE = 99.64727

#Regression for _other_ people
plot(other$Earnings~other$GPA)#? relationship at the BOTTOM
plot(other$Earnings~other$Number_Of_Professional_Connections) #Some correlation
plot(other$Earnings~other$Graduation_Year) #scattered
plot(other$Earnings~other$Height) #scattered
plot(other$Earnings~other$Number_Of_Credits) #scattered
plot(other$Earnings~other$Number_Of_Parking_Tickets) #scattered


o1 <- lm(Earnings~Number_Of_Professional_Connections, data = other)
p7 <- predict(o1, other)
mean((p7-other$Earnings)^2) #MSE is still very high


other$num1 <- other$Number_Of_Professional_Connections^2
plot(other$Earnings~other$num1)#Strong relationship1!
other$num2 <- other$Number_Of_Professional_Connections^3
plot(other$Earnings~other$num2)# Somewhat Identical to plot before squared
other$num3 <- other$Number_Of_Professional_Connections^4
other$num10 <- other$Number_Of_Professional_Connections^10

o2 <- lm(Earnings~Number_Of_Professional_Connections + num1, data = other)
p77 <- predict(o2,other)
mean((p77-other$Earnings)^2) #MSE = 97.15373

o3 <- lm(Earnings~Number_Of_Professional_Connections + num1 + num2, data = other)
p777 <- predict(o3, other)
mean((p777- other$Earnings)^2) #MSE = 97.07904

o4 <- lm(Earnings~Number_Of_Professional_Connections + num1 + num2 + num3, data = other)
p7777 <- predict(o4, other)
mean((p7777- other$Earnings)^2) #MSE = 96.93742

o10 <- lm(Earnings~Number_Of_Professional_Connections + num1 + num2 + num3 +num10, data = other)
p7777 <- predict(o10, other)
mean((p7777- other$Earnings)^2) #MSE = 96.91043

#Model

business <- subset(train[train$Major== 'Business',])

stem <- subset(train[train$Major== 'STEM',])

human <- subset(train[train$Major=='Humanities',])

voca <- subset(train[train$Major=='Vocational',])
voca$num1 <- voca$GPA^2

prof <- subset(train[train$Major== 'Professional',])
prof$num1 <- prof$GPA^2

other <- subset(train[train$Major=='Other',])
other$num1 <- other$Number_Of_Professional_Connections^2
other$num2 <- other$Number_Of_Professional_Connections^3
other$num3 <- other$Number_Of_Professional_Connections^4
other$num10 <- other$Number_Of_Professional_Connections^10

businesseven <- train[train$Graduation_Year %% 2 == 0 & train$Major == 'Buisness',]
businessodd <-train[train$Graduation_Year %% 2 != 0 & train$Major == 'Buisness',]


b1 <- lm(Earnings ~ GPA, data = businesseven)
b2 <- lm(Earnings ~ GPA, data = businessodd)
h1 <- lm(Earnings~GPA, data = human)
pp2 <- lm(Earnings~GPA + num1, data = prof)
s1 <- lm(Earnings~GPA, data = stem)
v2 <- lm(Earnings~GPA + num1, data = voca)
o10 <- lm(Earnings~Number_Of_Professional_Connections + num1 + num2 + num3 +num10, data = other)


#Prediction Challenge
test <- read.csv("C:/Users/lpnhu/Downloads/Earnings_Numeric_Test_2023-students.csv")


business <- subset(test[test$Major== 'Buisness',])
businesseven <- test[test$Graduation_Year %% 2 == 0 & test$Major =='Buisness',]
businessodd <-test[test$Graduation_Year %% 2 != 0 & test$Major =='Buisness',]
stem <- subset(test[test$Major== 'STEM',])
human <- subset(test[test$Major=='Humanities',])
voca <- subset(test[test$Major=='Vocational',])
voca$num1 <- voca$GPA^2
prof <- subset(test[test$Major== 'Professional',])
prof$num1 <- prof$GPA^2
other <- subset(test[test$Major=='Other',])
other$num1 <- other$Number_Of_Professional_Connections^2
other$num2 <- other$Number_Of_Professional_Connections^3
other$num3 <- other$Number_Of_Professional_Connections^4
other$num10 <- other$Number_Of_Professional_Connections^10


p1 <- predict(b1,businesseven)
p2 <- predict(b2,businessodd)
p3 <- predict(h1,human)
p4 <- predict(pp2,prof)
p5 <- predict(s1, stem)
p66 <- predict(v2,voca)
p7777 <- predict(o10, other)

decision <- rep(0,nrow(test))
decision[test$Major == 'Buisness'& test$Graduation_Year %% 2 == 0] <- p1
decision[test$Major == 'Buisness'& test$Graduation_Year %% 2 !=0] <- p2
decision[test$Major == 'Humanities'] <- p3
decision[test$Major == 'Professional'] <- p4
decision[test$Major == 'STEM'] <-p5
decision[test$Major == 'Vocational'] <- p66
decision[test$Major == 'Other'] <- p7777

submission <- read.csv("C:/Users/lpnhu/Downloads/submission.csv")
submission$Predicted <- decision
write.csv(submission,file = "submissionprediction3.csv", row.names = FALSE)


#Cross-validation

  v <- sample(1:nrow(train))
  v[1:5]
  trainScrambled <- train[v, ]
  
  n <- 1000
  trainSample <- trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled),]
  testSample <- trainScrambled[1:n,]
  
  #Training data
  business <- subset(trainSample[trainSample$Major== 'Business',])
  
  stem <- subset(trainSample[trainSample$Major== 'STEM',])
  
  human <- subset(trainSample[trainSample$Major=='Humanities',])
  
  voca <- subset(trainSample[trainSample$Major=='Vocational',])
  voca$num1 <- voca$GPA^2
  
  prof <- subset(trainSample[trainSample$Major== 'Professional',])
  prof$num1 <- prof$GPA^2
  
  other <- subset(trainSample[trainSample$Major=='Other',])
  other$num1 <- other$Number_Of_Professional_Connections^2
  other$num2 <- other$Number_Of_Professional_Connections^3
  other$num3 <- other$Number_Of_Professional_Connections^4
  other$num10 <- other$Number_Of_Professional_Connections^10
  
  businesseven <- trainSample[trainSample$Graduation_Year %% 2 == 0 & trainSample$Major == 'Buisness',]
  businessodd <-trainSample[trainSample$Graduation_Year %% 2 != 0 & trainSample$Major == 'Buisness',]
  
  
  b1 <- lm(Earnings ~ GPA, data = businesseven)
  b2 <- lm(Earnings ~ GPA, data = businessodd)
  h1 <- lm(Earnings~GPA, data = human)
  pp2 <- lm(Earnings~GPA + num1, data = prof)
  s1 <- lm(Earnings~GPA, data = stem)
  v2 <- lm(Earnings~GPA + num1, data = voca)
  o10 <- lm(Earnings~Number_Of_Professional_Connections + num1 + num2 + num3 +num10, data = other)
  
  #TEST
  business <- subset(testSample[testSample$Major== 'Business',])
  
  stem <- subset(testSample[testSample$Major== 'STEM',])
  
  human <- subset(testSample[testSample$Major=='Humanities',])
  
  voca <- subset(testSample[testSample$Major=='Vocational',])
  voca$num1 <- voca$GPA^2
  
  prof <- subset(testSample[testSample$Major== 'Professional',])
  prof$num1 <- prof$GPA^2
  
  other <- subset(testSample[testSample$Major=='Other',])
  other$num1 <- other$Number_Of_Professional_Connections^2
  other$num2 <- other$Number_Of_Professional_Connections^3
  other$num3 <- other$Number_Of_Professional_Connections^4
  other$num10 <- other$Number_Of_Professional_Connections^10
  
  businesseven <- trainSample[trainSample$Graduation_Year %% 2 == 0 & trainSample$Major == 'Buisness',]
  businessodd <-trainSample[trainSample$Graduation_Year %% 2 != 0 & trainSample$Major == 'Buisness',]
  
  
  p1 <- predict(b1, newdata = businesseven)
  p2 <- predict(b2, newdata = businessodd)
  p3 <- predict(h1, newdata = human)
  p4 <- predict(pp2, newdata = prof)
  p5 <- predict(s1, newdata = stem)
  p66 <- predict(v2, newdata = voca)
  p7777 <- predict(o10, newdata = other)
  
  decision <- rep(0,nrow(testSample))
  decision[testSample$Major == 'Buisness'& testSample$Graduation_Year %% 2 == 0] <- p1
  decision[testSample$Major == 'Buisness'& testSample$Graduation_Year %% 2 !=0] <- p2
  decision[testSample$Major == 'Humanities'] <- p3
  decision[testSample$Major == 'Professional'] <- p4
  decision[testSample$Major == 'STEM'] <-p5
  decision[testSample$Major == 'Vocational'] <- p66
  decision[testSample$Major == 'Other'] <- p7777
  MSE <- mean((decision-testSample$Earnings)^2)
print(MSE)
