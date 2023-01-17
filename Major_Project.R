#Major Project: Data Mining:
#Author: Akhil_Katha
#Date: 04/19/2022
#Purpose:



#Decision Tree
#Categorize the Data
#from dataset, we can pick a cutoff.  For this, we will define 22.5 as it is in the middle.  Obviously not particularly rigorous in our selection, but we'll go with it for the example.
data_set <- read.csv("ModifiedDataset.csv")
High=ifelse(data_set$Pass.Fail==-1,"Pass","Fail")
class=data.frame(data_set,High)
class=class[,-1]
set.seed(508)
sort<-sample(1:nrow(class),nrow(class)*0.8)
train_dt<-class[sort,]
test_dt<-class[-sort,]
fit<-rpart(High~.,train_dt)
tree.pred_train=predict(fit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(fit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))
accuracy_tune<-function(fit){
  predict_unseen<-predict(fit,test_dt,type='class')
  table_mat<-table(test_dt$High,predict_unseen)
  accuracy_Test<-sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}
#Allow us to change the parameters (minsplit,minbucket,maxdepth,cp) and see how the model changes
control<-rpart.control(minsplit=4,minbucket=2,maxdepth=3,cp=0)
tune_fit<-rpart(High~.,data=test_dt,method='class',control=control)
accuracy_tune(tune_fit)
#Identify the tuned parameters
tune_fit$control
#Pruning the tree as a function of cp (complexity parameter)
pfit<-prune(tune_fit,cp=0)
plot(pfit, uniform=TRUE,main="Pruned Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
tree.pred_train=predict(pfit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(pfit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))


#Bagging
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg<-train(as.factor(High)~.,data=train_dt,method="treebag",trControl=cvcontrol,importance=TRUE,nbagg=50,minsplit=4,minbucket=2,maxdepth=3,cp=0)
train.bagg
plot(varImp(train.bagg))
tree.pred_train=predict(train.bagg,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.bagg,test_dt)
with(test_dt,table(tree.pred_test,High))


#Boosting
#Random Forest
train.rf <- train(as.factor(High) ~ ., data=train_dt,method="rf",trControl=cvcontrol,importance=TRUE)
train.rf
tree.pred_train=predict(train.rf,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.rf,test_dt)
with(test_dt,table(tree.pred_test,High))
caret::varImp(train.rf)
#Random Forest Boosting
train.gbm <- train(as.factor(High) ~ ., data=train_dt,method="gbm",verbose=F,trControl=cvcontrol)
train.gbm
tree.pred_train=predict(train.gbm,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.gbm,test_dt)
with(test_dt,table(tree.pred_test,High))

##------------------------------- end of code -------------------------------##

#High <- subset(data_set, select = Pass.Fail)
#data_set <- subset(data_set, select = -(Pass.Fail))

#Decision Tree
#Categorize the Data
#hist(data_set$Pass.Fail)
#from dataset, we can pick a cutoff.  For this, we will define 22.5 as it is in the middle.  Obviously not particularly rigorous in our selection, but we'll go with it for the example.
#widths(data_set$Pass.Fail) <- list(Pass.Fail = 10)
data_set <- data_set %>% mutate(Pass.Fail = factor(Pass.Fail, levels = c('-1', '1'), 
                                                   labels = c('Pass', 'Fail'))) %>% na.omit()
High= subset(data_set, select = Pass.Fail)
data_set <- subset(data_set, select = -(Pass.Fail))
High <- data.frame(unlist(High))
data_set = data.frame(data_set, unlist(High))
type(data_set$unlist.High)
class=data.frame(data_set)
class=class[,]
set.seed(508)
sort<-sample(1:nrow(class),nrow(class)*0.8)
train_dt<-class[sort,]
test_dt<-class[-sort,]
#train_dt <- subset(train_dt, select = -(Pass.Fail))
fit<-rpart(High~.,data = train_dt, method = 'class')
tree.pred_train=predict(fit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(fit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))
accuracy_tune<-function(fit){
  predict_unseen<-predict(fit,test_dt,type='class')
  table_mat<-table(test_dt$High,predict_unseen)
  accuracy_Test<-sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}
#Allow us to change the parameters (minsplit,minbucket,maxdepth,cp) and see how the model changes
control<-rpart.control(minsplit=4,minbucket=2,maxdepth=3,cp=0)
tune_fit<-rpart(High~.,data=test_dt,method='class',control=control)
accuracy_tune(tune_fit)
#Identify the tuned parameters
tune_fit$control
#Pruning the tree as a function of cp (complexity parameter)
pfit<-prune(tune_fit,cp=0)
plot(pfit, uniform=TRUE,main="Pruned Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
tree.pred_train=predict(pfit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(pfit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))


#Bagging
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg<-train(as.factor(High)~.,data=train_dt,method="treebag",
                  trControl=cvcontrol,importance=TRUE,nbagg=50,
                  minsplit=4,minbucket=2,maxdepth=3,cp=0)
train.bagg
plot(varImp(train.bagg))
tree.pred_train=predict(train.bagg,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.bagg,test_dt)
with(test_dt,table(tree.pred_test,High))


#Boosting
#Random Forest
train.rf <- train(as.factor(High) ~ ., data=train_dt,method="rf",
                  trControl=cvcontrol,importance=TRUE)
train.rf
tree.pred_train=predict(train.rf,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.rf,test_dt)
with(test_dt,table(tree.pred_test,High))
#Random Forest Boosting
train.gbm <- train(as.factor(High) ~ ., data=train_dt,method="gbm",verbose=F,
                   trControl=cvcontrol)
train.gbm
tree.pred_train=predict(train.gbm,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.gbm,test_dt)
with(test_dt,table(tree.pred_test,High))




##------------------------------- end of code -------------------------------##



#data_set <- subset(data_set, select = -(Time))

#Standardizing the data using scale
#pass.fail <- subset(data_set, select = (Pass.Fail))
#data_set <- subset(data_set, select = -(Pass.Fail))

#data_set <- data_set %>% mutate_all(scale)
#data_set$Pass.Fail <- pass.fail 

#correlations <- cor(data_set)
#correlations <- correlations[, colSums(is.na(correlations) < nrow(correlations))]
#highCorr <- findCorrelation(correlations, cutoff = 0.75, names = FALSE)



#data_set1 <- na.omit(data_set)
#data_set <- scale(data_set, scale = FALSE)

#Fetch no. of missing values entries by column
na <- sapply(data_set, function(x) sum(is.na(x)))

na_per <- data_set %>% 
          map(is.na) %>% 
          map(sum) %>% 
          map(~. / nrow(data_set)) %>% 
          bind_cols()
#Location of NAs in vector
na_location <- which(is.na(data_set))
#Count of NAs in the data frame
na_count <- sum(is.na(data_set))
cols_na<- colSums(is.na(data_set))
summary(data_set)
#header <- colnames(data_set)

#for (ele in header) {
 # data_set$ele <- mean(data_set$ele, na.rm = TRUE)
#}

setDT(data_set)[, c("Time", "X5", "X13", "X42", "X49", "X52", "X69", "X97", "X141", "X149", "X178", "X179", "X186", "X189", "X190", "X191", "X192", "X193", "X194", "X226", "X229", "X230", "X231", "X232", "X233", "X234", "X235", "X236", "X237", "X240", "X241", "X242", "X243", "X256", "X257", "X258", "X259", "X260", "X261", "X262", "X263", "X264", "X265", "X266", "X276", "X284", "X313", "X314", "X315", "X322", "X325", "X326", "X327", "X328", "X329", "X330", "X364", "X369", "X370", "X371", "X372", "X373", "X374", "X375", "X378", "X379", "X380", "X381", "X394", "X395", "X396", "X397", "X398", "X399", "X400", "X401", "X402", "X403", "X404", "X414", "X422", "X449", "X450", "X451", "X458", "X461", "X462", "X463", "X464", "X465", "X466", "X481", "X498", "X501", "X502", "X503", "X504", "X505", "X506", "X507", "X508", "X509", "X512", "X513", "X514", "X515", "X528", "X529", "X530", "X531", "X532", "X533", "X534", "X535", "X536", "X537", "X538" ) := NULL]

data_set <- subset(data_set, select = -(Time))

data_set <-as.data.frame(scale(data_set),)

data_set_na <- data_set[which(rowMeans(!is.na(data_set))), which(colMeans(!is.na(data_set)))]



imputed_Data <- mice(data_set, m=5, maxit = 50, method = 'pmm', seed = 500)

#md.pattern(data_set)
dim(data_set)
vis_miss(data_set)

library(naniar)

impute(data_set)

data_set_hmisc <- aregImpute(~., data = data_set, n.impute = 5)

data_set_imp <- missForest(data_set, maxiter = 10, ntree = 300)
#vis_miss(data_set, sort_miss = TRUE, warn_large_data = FALSE)

res_ama <- amelia(data_set, m = 5)
pca_na <- estim_ncpPCA(data_set, method.cv = 'k', verbose = FALSE )
