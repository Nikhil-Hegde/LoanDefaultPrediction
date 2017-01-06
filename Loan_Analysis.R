# Data Loading
library(caret)
data <- read.csv("C:/Users/nikhil.h/Desktop/Loan_Data.csv")
 
# Feature Engineering 
data <- data[-which(data$Current.Loan.Amount==99999999),]
  # Replacing Higher Credit score of greater than 850 
  # Handling NA values in Revenue
# Handling of NA in credit score 
data$Score <- 0
data$Score <- as.numeric(data$Score)
for( i in 1:length(data$Loan.ID)){
  if(!is.na(data$Credit.Score[i]) && (data$Credit.Score[i] > 850)){
    data$Credit.Score[i] <- data$Credit.Score[i]/10}
  if((data$Number.of.Credit.Problems[i] < 2) && (data$Bankruptcies[i] < 2)){
    data$Score <- 1}
  if(is.na(data$Annual.Income[i])){
    Score <- subset(data, data$Years.of.Credit.History == data$Years.of.Credit.History[i],select=1:19)
    data$Annual.Income[i] <- round(median(Score$Annual.Income,na.rm = TRUE))
  }
  if(is.na(data$Credit.Score[i])){
    Score <- subset(data, data$Number.of.Open.Accounts == data$Number.of.Open.Accounts[i],select=1:19)
    data$Credit.Score[i] <- round(median(Score$Credit.Score,na.rm = TRUE))
  }
}

data$Credit.Band <- "X"
data$Credit.Band <- as.character(data$Credit.Band)

  # Variable creation for Credit Score
for( i in 1:length(data$Loan.ID)){
  if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<= 850) & (data$Credit.Score[i]>=800) ){
    data$Credit.Band[i] <- "A"}
  else if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<=799) & (data$Credit.Score[i]>=750)){
      data$Credit.Band[i] <- "B"}
  else if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<=749) & (data$Credit.Score[i]>=700)){
        data$Credit.Band[i] <- "C"}
  else if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<=699) & (data$Credit.Score[i]>=650)){
          data$Credit.Band[i] <- "D"}
  else if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<=649) & (data$Credit.Score[i]>=550)){
            data$Credit.Band[i] <- "E"}
  else if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<=549) & (data$Credit.Score[i]>=500)){
              data$Credit.Band[i] <- "F"}
  else if(!is.na(data$Credit.Score[i]) & (data$Credit.Score[i]<=499) & (data$Credit.Score[i]>=300)){
                  data$Credit.Band[i] <- "G"}
  else data$Credit.Band[i] <- "H"
}

data <- data[!is.na(data$Tax.Liens),]
data$Credit.Band <- as.factor(data$Credit.Band)
data$Maximum.Open.Credit <- as.numeric(data$Maximum.Open.Credit)
data$Current.Credit.Balance <- as.numeric(data$Current.Credit.Balance)
data$Tax.Liens <- as.numeric(data$Tax.Liens)
data$Number.of.Credit.Problems <- as.numeric(data$Number.of.Credit.Problems)
data$Monthly.Debt <- as.numeric(data$Monthly.Debt)
# Bankruptcies and Number.of.Credit.Problems of highly correlated. In case of Bankruptcies their are 217 
# NA values. I think it is better to drop Bankrupticies as required information can be captured from 
# Number.of.Credit.Problems.

# Deleting variables 
data <- subset(data, select=-c(Bankruptcies,Months.since.last.delinquent))

# Removing remaining NA
data <- data[complete.cases(data),]

# Splitting the data as train and test dataset 
smp_size <- floor(0.75 * nrow(data))

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]


### Model 1
library(randomForest)
fit <- randomForest(as.factor(Loan.Status) ~ Current.Loan.Amount+Score+Credit.Score+Years.in.current.job+Annual.Income+Monthly.Debt+Years.of.Credit.History+Maximum.Open.Credit
                      ,data = train, importance = TRUE,ntree =2000)

Prediction <- predict(fit,test)
submit <- data.frame(Loan.ID = test$Loan.ID,Loan.Status= Prediction)
write.csv(submit,file = "secondforest.csv",row.names = FALSE)
write.csv(test, file = "test_file.csv",row.names = FALSE)

### Model 2
set.seed(123)
library(party)
fit <- cforest( as.factor(Loan.Status) ~ Current.Loan.Amount+Credit.Band+Years.in.current.job+Home.Ownership+Annual.Income+Monthly.Debt+Years.of.Credit.History+Maximum.Open.Credit+Number.of.Open.Accounts
               ,data = train, controls=cforest_unbiased(ntree=1000, mtry=5, maxdepth=8))

#summary(data)
#write.csv(data,file = "C:/Users/nikhil.h/Desktop/loan.csv")
