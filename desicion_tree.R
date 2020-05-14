sum(is.na(bank))
summary(bank)
head(bank)
bank_nb<-bank
bank_nb<-unique(bank_nb)
unique(bank_nb)
#identifing removing outliers 
boxplot(bank_nb$balance)
OutVals2= boxplot(bank_nb$balance)$out
OutVals2
bank_nb[which(bank_nb$balance %in% OutVals2),]
bank_nb <-bank_nb[-which(bank_nb$balance %in% OutVals2),]
boxplot(bank_nb$balance)#boxplot for balance after outliers
boxplot(bank_nb$day)#boxplot for day
boxplot(bank_nb$duration)#boxplot for day
OutVals4= boxplot(bank_nb$duration)$out
OutVals4
bank_nb[which(bank_nb$duration %in% OutVals4),]
bank_nb <-bank_nb[-which(bank_nb$duration %in% OutVals4),]
boxplot(bank_nb$duration)#box plot for duration
boxplot(bank_nb$campaign)
OutVals5= boxplot(bank_nb$campaign)$out
OutVals5
bank_nb[which(bank_nb$campaign %in% OutVals5),]
bank_nb <-bank_nb[-which(bank_nb$campaign %in% OutVals5),]
boxplot(bank_nb$campaign)#box plot for capaign
boxplot(bank_nb$pdays)
OutVals6= boxplot(bank_nb$pdays)$out
OutVals6
bank_nb[which(bank_nb$pdays %in% OutVals6),]
bank_nb <-bank_nb[-which(bank_nb$pdays %in% OutVals6),]
boxplot(bank_nb$pdays)#box plot for pdays
par(mfrow=c(3,2))
boxplot(bank_nb$balance)
boxplot(bank_nb$campaign)
boxplot(bank_nb$day)
boxplot(bank_nb$duration)
str(bank_nb)
View(bank_nb)
#correlation
corrr<-cor(corrrr)
corrplot(corrr, method="color")
#dividing the data set into two parts
ran <- sample(1:nrow(bank_nb),0.8 * nrow(bank_nb))
train_bank_nb<-bank_nb[ran,]
test_bank_nb<-bank_nb[-ran,]
#80 percent train set 
dim(train_bank_nb)
#20 percent test set 
dim(test_bank_nb)
#Building desicion tree 
fit <- rpart(deposit~., data = bank_nb, method = 'class')
summary(fit)
View(summary(fit))
par(mfrow=c(1,1))
rpart.plot(fit)
predict_bank<-predict(fit,test_bank_nb,type='class')
table_mat <- table(test_bank_nb$deposit, predict_bank)
table_mat
confusionMatrix(table_mat)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#fitting the model   for test data set 
fit2<- rpart(deposit~., data = test_bank_nb, method = 'class')
summary(fit2)
rpart.plot(fit2,extra = 100)
predict_bank2<-predict(fit2,train_bank_nb,type='class')
table_mat2 <- table(train_bank_nb$deposit, predict_bank2)
table_mat2
accuracy_Train <- sum(diag(table_mat2)) / sum(table_mat2)
accuracy_Train
#prepruning the data for train set 
printcp(fit)
hr_model_preprun <- rpart(deposit ~ ., data = train_bank_nb, method = "class", control = rpart.control(cp = 0),maxdepth = 5,minsplit = 100)
summary(hr_model_preprun)
View(summary(hr_model_preprun))
rpart.plot(hr_model_preprun)
predict_bank_preprun<-predict(hr_model_preprun,test_bank_nb,type='class')
table_mat3 <- table(test_bank_nb$deposit, predict_bank_preprun)
table_mat3
accuracy_Train_preprun <- sum(diag(table_mat3)) / sum(table_mat3)
accuracy_Train_preprun
confusionMatrix(table_mat3)
#prepruning the data for test set
hr_model_preprun2 <- rpart(deposit ~ ., data = test_bank_nb, method = "class", control = rpart.control(cp = 0),maxdepth = 5,minsplit = 100)
summary(hr_model_preprun2)
rpart.plot(hr_model_preprun2)
predict_bank_preprun2<-predict(hr_model_preprun2,train_bank_nb,type='class')
table_mat56 <- table(train_bank_nb$deposit, predict_bank_preprun2)
table_mat56
accuracy_Test_preprun <- sum(diag(table_mat56)) / sum(table_mat56)
accuracy_Test_preprun
#postpruning for train  data
printcp(fit)
plotcp(fit)
pruned.tree <- prune(fit, cp = 0.01)
rpart.plot(pruned.tree)
View(summary(pruned.tree))
predict_bank_pruned<-predict(pruned.tree,test_bank_nb,type='class')
table_mat5 <- table(test_bank_nb$deposit, predict_bank_pruned)
table_mat5
confusionMatrix(table_mat5)
accuracy_Train_pruned <- sum(diag(table_mat5)) / sum(table_mat5)
accuracy_Train_pruned
#post pruning for set set 
pruned.tree2 <- prune(fit2, cp = 0.010)
rpart.plot(pruned.tree2)
summary(pruned.tree2)
predict_bank_pruned2<-predict(pruned.tree2,train_bank_nb,type='class')
table_mat6 <- table(train_bank_nb$deposit, predict_bank_pruned2)
table_mat6
accuracy_Test_pruned <- sum(diag(table_mat6)) / sum(table_mat6)
accuracy_Test_pruned
#accuracy comparison
data.frame(accuracy_Test,accuracy_Train,accuracy_Train_preprun,accuracy_Test_preprun,accuracy_Train_pruned,accuracy_Test_pruned)

