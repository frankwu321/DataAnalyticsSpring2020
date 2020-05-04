library(readr)
library(stringr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(corrplot)
library(caret)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gains)
library(e1071) 
transport <- read_csv("~/Downloads/tmd-dataset-5-seconds-sliding-window/dataset_5secondWindow%5B1%5D.csv")
summary(transport)
sum(is.na(transport))
sum(duplicated(transport))
table(transport$target)
str(transport)

#Rename the columns 
names(transport) = str_replace(names(transport),'android.sensor.','')
names(transport) = str_replace(names(transport),'#','.')

#Remove time stamp
transport = transport[-1]

#Distribution of target
ggplot(transport, aes(x = target)) + theme(text = element_text(size=30))+
  geom_histogram(colour = 'black', fill = 'white', stat = 'count') + labs(x = 'mode') #+ggtitle('Distribution of target')


#Distribution of accelerometer
accelerometer = melt(transport[1:4])
ggplot(accelerometer) + theme(text = element_text(size=20)) + 
  geom_density(aes(x = value, colour = variable, fill = variable), alpha=0.5) 

#Distribution of gyroscope
gyroscope = melt(transport[5:8])
ggplot(gyroscope) + theme(text = element_text(size=20)) + 
  geom_density(aes(x = value, colour = variable, fill = variable), alpha=0.5)


#Distribution of sound
sound = melt(transport[9:12])
ggplot(sound) + theme(text = element_text(size=20)) + 
  geom_density(aes(x = value, colour = variable, fill = variable), alpha=0.5) 


#Remove sound.mean & sound.min
transport = transport[-c(10,11)]


#Distribution of accelerometer with target
g1 = ggplot(transport,aes(x=target, y=accelerometer.mean, fill=target)) + geom_boxplot() + ggtitle('Distribution of accelerometer.mean with target') + labs(x = 'target') + theme(text = element_text(size=15), legend.position = "none")
g2 = ggplot(transport,aes(x=target, y=accelerometer.std, fill=target)) + geom_boxplot() + ggtitle('Distribution of accelerometer.std with target') + labs(x = 'target')+ theme(text = element_text(size=15), legend.position = "none")
grid.arrange(g1, g2, ncol = 1)

#Distribution of gyroscope with target
g3 = ggplot(transport,aes(x=target, y=gyroscope.mean, fill=target)) + geom_boxplot() + ggtitle('Distribution of gyroscope.mean with target') + labs(x = 'target') + theme(text = element_text(size=15), legend.position = "none")
g4 = ggplot(transport,aes(x=target, y=gyroscope.std, fill=target)) + geom_boxplot() + ggtitle('Distribution of gyroscope.std with target') + labs(x = 'target') + theme(text = element_text(size=15), legend.position = "none")
grid.arrange(g3, g4, ncol = 1)

#Distribution of sound with target
g5 = ggplot(transport,aes(x=target, y=sound.mean, fill=target)) + geom_boxplot() + ggtitle('Distribution of sound.mean with target') + labs(x = 'target') + theme(text = element_text(size=15), legend.position = "none")
g6 = ggplot(transport,aes(x=target, y=sound.std, fill=target)) + geom_boxplot() + ggtitle('Distribution of sound.std with target') + labs(x = 'target') + theme(text = element_text(size=15), legend.position = "none")
grid.arrange(g5, g6, ncol = 1)

#Encode transportation mode into numerical: Still = 0, Walking = 1, Bus = 2, Car = 3, Train = 4
#transport$target[transport$target == 'Still'] = 0
#transport$target[transport$target == 'Walking'] = 1
#transport$target[transport$target == 'Bus'] = 2
#transport$target[transport$target == 'Car'] = 3
#transport$target[transport$target == 'Train'] = 4
#transport$target = as.numeric(transport$target)

#Heatmap of correlation matrix
cor(transport)
corrplot(cor(transport), method = 'color')

#Data standardization 
transport[1:10] = scale(transport[1:10])

summary(transport)

#Model Development

#train & test split (60% & 40%)
transport$target = as.factor(transport$target)

set.seed(2020)
ind = sample(nrow(transport),nrow(transport)*0.6)
transport_train = transport[ind,]
transport_test = transport[-ind,]

######KNN model######
acc_knn = c()
for (i in 1:100){
  acc_knn[i] <- sum(knn(train = transport_train[1:10], test = transport_test[1:10], cl = transport_train$target, k = i) == transport_test$target) / nrow(transport_test)
}
k = c(1:100)
knn_df = cbind.data.frame(index, acc_knn)

#grid search for k
ggplot(knn_df, aes(x=k, y=acc_knn))+geom_line(color = 'red')+geom_point()+theme(text = element_text(size=20))

#optimal k = 1
knn.1 = knn(train = transport_train[1:10], test = transport_test[1:10], cl = transport_train$target, k = 1)
acc_knn.1 = max(acc_knn)
confusionMatrix(knn.1, transport_test$target)

#Lift Chart
gain_knn = gains(as.numeric(transport_test$target), as.numeric(knn.1), groups=10)
knn_lift = plot(c(0,gain_knn$cume.pct.of.total*sum(as.numeric(transport_test$target))) ~ c(0,gain_knn$cume.obs), 
               xlab="# cases", ylab = "Cumulative", main="Lift chart for KNN", type = "l", col = 'red',cex.main=1.75, cex.lab=1.5, cex.axis=1.75)
lines(c(0, sum(as.numeric(transport_test$target))) ~ c(0,nrow(transport_test)),lty=1)

######Support Vector Machine######
#Grid search for optimal gamma and cost
gamma = rep(10:20,each =11)/10
cost = rep(10:20,11)*10
acc_SVM = NA
SVM_df = cbind.data.frame(gamma,cost,acc_SVM)

for (i in 1:nrow(SVM_df)){
  SVM_df$acc_SVM[i] = sum(predict(svm(target~., data=transport_train, 
              method="C-classification", kernal="radial", 
              gamma=SVM_df[i,1], cost=SVM_df[i,2]), transport_test) == transport_test$target)/nrow(transport_test)
}

ggplot(SVM_df, aes(x=gamma, y=acc_SVM, group=as.factor(cost))) +
  geom_line(aes(color=as.factor(cost)))+
  geom_point(aes(color=as.factor(cost)))+theme(text = element_text(size=20))

max(SVM_df$acc_SVM)
gamma.opt = SVM_df$gamma[SVM_df$acc_SVM == max(SVM_df$acc_SVM)]
cost.opt = SVM_df$cost[SVM_df$acc_SVM == max(SVM_df$acc_SVM)]
#Optimal gamma = 1.4, cost = 140
svm.opt = svm(target~., data=transport_train, 
                      method="C-classification", kernal="radial", 
                      gamma=1.4, cost=140)
svm.opt_pred = predict(svm.opt, transport_test)
svm.opt_acc = mean(svm.opt_pred == transport_test$target)
confusionMatrix(svm.opt_pred, transport_test$target)

head(svm.opt$SV, 10)
plot(svm.opt, data=transport_train, accelerometer.std~sound.mean)

#Lift Chart
gain_svm = gains(as.numeric(transport_test$target), as.numeric(svm.opt_pred), groups=10)
svm_lift = plot(c(0,gain_svm$cume.pct.of.total*sum(as.numeric(transport_test$target))) ~ c(0,gain_svm$cume.obs), 
                xlab="# cases", ylab = "Cumulative", main="Lift chart for SVM", type = "l", col = 'red',cex.main=1.75, cex.lab=1.5, cex.axis=1.75)
lines(c(0, sum(as.numeric(transport_test$target))) ~ c(0,nrow(transport_test)),lty=1)

######Random Forest######
#Grid search for optimal mtry
mtry = c(1:10)
acc_rf = NA
RF_df = cbind.data.frame(mtry, acc_rf)
for (i in 1:nrow(RF_df)){
  RF_df$acc_rf[i] = mean(predict(randomForest(target~., data = transport_train, ntree = 500, 
                            mtry = RF_df$mtry[i]), transport_test) == transport_test$target)
}

ggplot(RF_df, aes(x=mtry, y=acc_rf))+geom_line(color = 'red')+geom_point()+theme(text = element_text(size=20))

#Optimal mtry = 9
mtry.opt = RF_df$mtry[RF_df$acc_rf == max(RF_df$acc_rf)]
rf.opt = randomForest(target~., data = transport_train, ntree = 500, 
                      mtry = 9, importance = T)
rf.opt_pred = predict(rf.opt, transport_test)
rf.opt_acc = mean(rf.opt_pred == transport_test$target)
confusionMatrix(rf.opt_pred, transport_test$target)

#Feature's importance
importance(rf.opt)
varImpPlot(rf.opt)

#Lift Chart
gain_rf = gains(as.numeric(transport_test$target), as.numeric(rf.opt_pred), groups=10)
rf_lift = plot(c(0,gain_rf$cume.pct.of.total*sum(as.numeric(transport_test$target))) ~ c(0,gain_rf$cume.obs), 
                xlab="# cases", ylab = "Cumulative", main="Lift chart for RF", type = "l", col = 'red',cex.main=1.75, cex.lab=1.5, cex.axis=1.75)
lines(c(0, sum(as.numeric(transport_test$target))) ~ c(0,nrow(transport_test)),lty=1)

#accuracy comparison
model_name = c('KNN', 'SVM', 'Random.Forest.Classifier')
model_acc = c(acc_knn.1, svm.opt_acc, rf.opt_acc)
model_compare = cbind.data.frame(model_name, model_acc)

ggplot(model_compare, aes(x=model_name, y=model_acc)) + theme(text = element_text(size=20))+
  geom_bar(stat="identity", color="blue", fill="white") + coord_cartesian(ylim = c(0.7, 0.9))+ 
  geom_text(aes(label=round(model_acc,3)), vjust=1.6, color="black", size=10)#+ ggtitle('Acuracy comparison')
