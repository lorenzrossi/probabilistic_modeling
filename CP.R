library(readxl)
library(PerformanceAnalytics)
library(corrplot)
library(ggcorrplot)
library(mgm)
library(gRim)
library(bnlearn)
library(gRapHD)
library(qgraph)
library(Rgraphviz)
library(igraph)
library(gRbase)
library(RBGL)
library(caret)
library(pROC)
library(ROCR)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggpubr)
library(gRapHD)
library(glasso)
library(SIN)
library(gRain)
library(randomForest)
library(randomForestExplainer)
library(e1071)
library(MASS)
library(infotheo)
library(varrank)
library(glmnet)
library(leaps)
library(caret)
library(car)
library(combinat)
library(rpart)
library(ROSE)
library(pcalg)
library(bnlearn)
library(ggm)
library(minet)
library(rpart)
library(rpart.plot) 
library(RColorBrewer)      
library(party)                  
library(partykit)  
library(gbm)

cb <- read.csv('BankruptcyFinal.csv')

barplot(table(cb$Bankrupt.),col=rainbow(2),
        main="Frequency of Bankruptcy",
        xlab="Recession Tag",
        ylab="Number of Recession Events")

cb_balanced <- ROSE(Bankrupt. ~ ., data=cb, seed = 123)$data

#GLASSO FOR VARS BEHAVIOUR

#rho = 0.1
S.cb <- cov.wt(cb_balanced, method="ML")$cov
C.cb  <- cov2cor(S.cb)
res.lasso1 <- glasso(C.cb , rho=0.1)
AM1 <- res.lasso1$wi!=0
diag(AM1)<-FALSE
g.lasso1 <- as(AM1,"graphNEL")
nodes(g.lasso1)<-names(cb_balanced)
glasso.cp1 <- cmod(edgeList(g.lasso1),data=cb_balanced)
plot(as(glasso.cp1 ,"igraph"),vertex.color="red",vertex.label.dist=0.5,alpha=TRUE,
     vertex.label.cex=0.6,vertex.size=8, edge.color="black",edge.size=5,
     layout=layout_with_fr(as(glasso.cp1,'igraph')), vertex.label.family='Helvetica')

#rho 0.2
res.lasso2 <- glasso(C.cb , rho=0.2)
AM2 <- res.lasso2$wi!=0
diag(AM2)<-FALSE
g.lasso2 <- as(AM2,"graphNEL")
nodes(g.lasso2)<-names(cb_balanced)
glasso.cp2 <- cmod(edgeList(g.lasso2),data=cb_balanced)
plot(as(glasso.cp2 ,"igraph"),vertex.color="red",vertex.label.dist=0.5,alpha=TRUE,
     vertex.label.cex=0.6,vertex.size=8, edge.color="black",edge.size=5,
     layout=layout_with_fr(as(glasso.cp2,'igraph')), vertex.label.family='Helvetica')

#rho = 0.3
res.lasso3 <- glasso(C.cb , rho=0.3)
AM3 <- res.lasso3$wi!=0
diag(AM3)<-FALSE
g.lasso3 <- as(AM3,"graphNEL")
nodes(g.lasso3)<-names(cb_balanced)
glasso.cp3 <- cmod(edgeList(g.lasso3),data=cb_balanced)
plot(as(glasso.cp3 ,"igraph"),vertex.color="red",vertex.label.dist=0.5,alpha=TRUE,
     vertex.label.cex=0.6,vertex.size=8, edge.color="black",edge.size=5,
     layout=layout_with_fr(as(glasso.cp3,'igraph')), vertex.label.family='Helvetica')

#rho = 0.4
res.lasso4 <- glasso(C.cb , rho=0.4)
AM4 <- res.lasso4$wi!=0
diag(AM4)<-FALSE
g.lasso4 <- as(AM4,"graphNEL")
nodes(g.lasso4)<-names(cb_balanced)
glasso.cp4 <- cmod(edgeList(g.lasso4),data=cb_balanced)
plot(as(glasso.cp ,"igraph"),vertex.color="red",vertex.label.dist=0.5,alpha=TRUE,
     vertex.label.cex=0.6,vertex.size=8, edge.color="black",edge.size=5,
     layout=layout_with_fr(as(glasso.cp4,'igraph')), vertex.label.family='Helvetica')

#rho 0.5
res.lasso5 <- glasso(C.cb , rho=0.5)
AM5 <- res.lasso5$wi!=0
diag(AM5)<-FALSE
g.lasso5 <- as(AM5,"graphNEL")
nodes(g.lasso5)<-names(cb_balanced)
glasso.cp5 <- cmod(edgeList(g.lasso5),data=cb_balanced)
plot(as(glasso.cp5 ,"igraph"),vertex.color="red",vertex.label.dist=0.5,alpha=TRUE,
     vertex.label.cex=0.6,vertex.size=8, edge.color="black",edge.size=5,
     layout=layout_with_fr(as(glasso.cp5,'igraph')), vertex.label.family='Helvetica')


#minforest

bf<-minForest(cb_balanced)
mbG<-stepw(model=bf,data=cb_balanced)

plot(bf,cex.vert.label=0.6,numIter=6000,col.labels=c("red"), edge.size = 0.5, vert.hl=c(31),col.hl=c("blue"),energy=TRUE)

plot(mbG,cex.vert.label=0.6,numIter=6000,col.labels=c("red"),vert.hl=c(31),col.hl=c("blue"),energy=TRUE)

#MGM + ROC curve

fit_mgm <- mgm(data = cb_balanced,type = c(rep("g",30), "c"), levels = c(rep(1,30),2),k=3,lambdaSel = "CV",
               lambdaFolds=10,ruleReg="AND",overparameterize = T)

qgraph::qgraph(fit_mgm$pairwise$wadj,
               layout = "spring", repulsion = 1.3,
               edge.color = fit_mgm$pairwise$edgecolor,
               nodeNames = colnames(cb_balanced),
               color = c(rep("lightblue",30),"yellow"),
               legend.mode="style1", legend.cex=.15,
               vsize = 3, esize = 15)

pred_obj <- predict(fit_mgm, cb_balanced)

round(pred_obj[["probabilties"]][[31]],2)

Predicted_vs_real<-as.data.frame(pred_obj[["predicted"]][,31])

colnames(Predicted_vs_real)<-c("PRED")

levels(Predicted_vs_real$PRED) <- c("TRUE", "FALSE")

Predicted_vs_real$PRED <- as.factor(Predicted_vs_real$PRED)

conf<-data.frame(lapply(cb_balanced[31], as.factor),Predicted_vs_real)


Confusion_matrix_mgm<-confusionMatrix(conf$Bankrupt.,conf$PRED)
Confusion_matrix_mgm

fourfoldplot(Confusion_matrix_mgm$table,color = c("red","turquoise"),conf.level = 0)

pred_numeric<-prediction(as.numeric(conf$PRED),as.numeric(conf$Bankrupt.))

roc_mgm.perf <- performance(pred_numeric,measure = "tpr", x.measure = "fpr")

phi_Bankruptcy_mgm<-performance(pred_numeric, "phi")

phi_Bankruptcy_mgm@y.values[[1]][2]


df_mgm <- data.frame (roc_mgm.perf@x.values,roc_mgm.perf@y.values)

colnames(df_mgm)<-c("FPR", "TPR")

ggplot(df_mgm,aes(FPR,TPR))+geom_point(color="red")+geom_line(color="red")+ggtitle("ROC CURVE")+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=14,face="bold"))


#LOGIT

set.seed(123)
split_train_test <- createDataPartition(cb_balanced$Bankrupt.,p=0.7,list=FALSE)
cb_train<- cb_balanced[split_train_test,]
cb_test <- cb_balanced[-split_train_test,]

logit = glm(formula = Bankrupt.~., data = cb_balanced, family = binomial)
summary(logit)

logistic.prob<-data.frame(predict(logit,cb_test,type = "response"))
colnames(logistic.prob)<-c("Pred")
logistic.prob<- data.frame(ifelse(logistic.prob > 0.5, 1, 0))
logistic.prob["True"]<-as.factor(cb_test$Bankrupt.)
logistic.prob$Pred <- as.factor(logistic.prob$Pred)


fourfoldplot(table(logistic.prob), color = c("red","turquoise"),conf.level = 0,margin = 1)

caret::confusionMatrix(table(logistic.prob))

pred_log<-prediction(as.numeric(logistic.prob$Pred),as.numeric(logistic.prob$True))

pred_log.perf <- performance(pred_log, measure = "tpr", x.measure = "fpr")

phi_log<-performance(pred_log, "phi")

phi_Bankruptcy_log<-performance(pred_numeric, "phi")
phi_Bankruptcy_log@y.values[[1]][2]

df_log <- data.frame(pred_log.perf@x.values,pred_log.perf@y.values)

colnames(df_log)<-c("FPR", "TPR")

ggplot(df_log,aes(FPR,TPR))+geom_point(color="red")+geom_line(color="red")+ggtitle("ROC CURVE")+theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=14,face="bold"))


#RANDOM FOREST


RF_perf_out<-tuneRF(cb_train[,-31],cb_train[,31], ntree=1000)
RF_perf_out<-data.frame(RF_perf_out)
rfor.cb <-randomForest(Bankrupt.~., data=cb_train, localImp = TRUE, importance=TRUE,proximity=TRUE, mtry=20)
rfor.predict<- data.frame(predict(rfor.cb, cb_test, type = "response"))
rfor.predict$predict.rfor.cb..cb_test..type....response.. <- ifelse(rfor.predict$predict.rfor.cb..cb_test..type....response.. > 0.5, 1,0)

var_imp_rforest<-data.frame(rfor.cb$importance)
colnames(var_imp_rforest)<-c("Variable","Overall")
var_imp_rforest[,1]<-rownames(var_imp_rforest)
rownames(var_imp_rforest)<-seq(1:30)

ggplot(var_imp_rforest, aes(y=reorder(Variable,Overall),x=Overall,color="red")) + 
  geom_point() +
  geom_segment(aes(x=0,xend=Overall,yend=Variable)) +
  scale_color_discrete(name="Variable Group") +
  xlab("Overall importance") +
  ylab("Variable Name") + guides(color = FALSE, size = FALSE) + theme_bw()


rfor.predict["Test"]<-as.factor(cb_test$Bankrupt.)

colnames(rfor.predict)<-c("Predict","Test")

rfor.predict$Predict <- as.factor(rfor.predict$Predict)

rfor_cm <- confusionMatrix(rfor.predict$Predict, rfor.predict$Test)
rfor_cm

fourfoldplot(table(rfor.predict), color = c("red","turquoise"),conf.level = 0)

pred_for<-prediction(as.numeric(rfor.predict$Predict),as.numeric(rfor.predict$Test))

roc_for.perf <- performance(pred_for, measure = "tpr", x.measure = "fpr")

phi_Bankruptcy_for<-performance(pred_for, "phi")
phi_Bankruptcy_for@y.values[[1]][2]


df_for <- data.frame (roc_for.perf@x.values,roc_for.perf@y.values)

colnames(df_for)<-c("FPR", "TPR")

ggplot(df_for,aes(FPR,TPR))+geom_point(color="red")+geom_line(color="red")+ggtitle("ROC CURVE")+theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=14,face="bold"))


#BOOSTING

#Setting the random seed for replication
set.seed(123)

#setting up cross-validation
cvcontrol <- trainControl(method="repeatedcv", number = 10,
                          allowParallel=TRUE)

train.gbm <- train(as.factor(Bankrupt.) ~ ., 
                   data=cb_train,
                   method="gbm",
                   verbose=F,
                   trControl=cvcontrol)

par(mar = c(6, 10, 4.5, 4.5))
summary(train.gbm, cBars = 20,las = 1)


gbm.classTrain <-  predict(train.gbm, 
                           type="raw")
head(gbm.classTrain)


gbm.classTest <-  predict(train.gbm, 
                          newdata = cb_test,
                          type="raw")

gbm.classTest <- data.frame(gbm.classTest)
gbm.classTest["Test"]<-as.factor(cb_test$Bankrupt.)
colnames(gbm.classTest)<-c("Predict","Test")

gbm.classTest$Predict <- as.factor(gbm.classTest$Predict)

gbm_cm <- confusionMatrix(gbm.classTest$Predict, gbm.classTest$Test)
gbm_cm

fourfoldplot(table(gbm.classTest), color = c("red","turquoise"),conf.level = 0)

pred_gbm<-prediction(as.numeric(gbm.classTest$Predict),as.numeric(gbm.classTest$Test))

roc_gbm.perf <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")

phi_Bankruptcy_GBM<-performance(pred_gbm, "phi")
phi_Bankruptcy_GBM@y.values[[1]][2]


df_gbm <- data.frame (roc_gbm.perf@x.values,roc_gbm.perf@y.values)

colnames(df_gbm)<-c("FPR", "TPR")

ggplot(df_gbm,aes(FPR,TPR))+geom_point(color="red")+geom_line(color="red")+ggtitle("ROC CURVE")+
  theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=14,face="bold"))

###CAUSALITY

#PCalg

suff.stat <- list(C=C.cb,n=nrow(cb_balanced))
pc.fit <- pc(suff.stat, gaussCItest,p=ncol(cb_balanced),alpha=0.02)

par(mfrow = c(1,1))
plot(pc.fit)



ida(1,31, C.cb, pc.fit@graph)
ida(3,31, C.cb, pc.fit@graph)
ida(9,31, C.cb, pc.fit@graph)
ida(10,31,C.cb, pc.fit@graph)
ida(11,31,C.cb, pc.fit@graph)
ida(15,31,C.cb, pc.fit@graph)
ida(16,31,C.cb, pc.fit@graph)
ida(21,31,C.cb, pc.fit@graph)
ida(27,31,C.cb, pc.fit@graph)


