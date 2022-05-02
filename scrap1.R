library(readr)
library(Matrix)
library(readr)
library(Hmisc)
library(dplyr)
library(ade4)
library(car)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(ggpubr)
library(olsrr)
library(tidyverse)
library(caret)
library(Metrics)
library(leaps)
library(paran)
library(ROSE)
library(pROC)
library(reshape)
library(class)
library(tidyr)
library(kernlab)
library(RBGL)
library(corrplot)
library(Rgraphviz)
library(igraph)
library(gRbase)
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(ggm)
library(gRim)
library(gRapHD)
library(gRbase)
library(glasso)
library(glmnet)
library(BiocManager)
library(bnlearn)
library(bnstruct)
library(UPG)
library(mvtnorm)
library(matrixcalc)
library(pcalg)
library(glasso)
library(ppcor)
library(randomForest)
library(gbm)
library(pROC)
library(ada)

ar <- read.csv('africa_recession.csv')
vd <- read.csv('VariableDefinitions.csv')

ar <- na.omit(ar)

# Keep/drop columns (TYPE IN THE COL NO.)

summary(ar)

#data visualisation

barplot(table(ar$growthbucket),col=rainbow(2),
        main="Frequency of Recession",
        xlab="Recession Tag",
        ylab="Number of Recession Events")
box()

ar$growthbucket<-as.factor(ar$growthbucket)
summary(ar$growthbucket)

#BN

bn.hc <- hc(ar)
am <- amat(bn.hc)
bn.hc

plot(as(essentialGraph(am),'igraph'), edge.arrow.size = 0.2, vertex.size = 4)

fittedbn <- bn.fit(bn.hc, data = ar)

cv.hc = bn.cv(bn.hc, data = ar, runs = 100, method = "k-fold")
cv.hc
#modello difficile da interpretare

#############

#lasso

ar1 <- ar

ar1$growthbucket<-as.integer(ar1$growthbucket)

x=model.matrix(growthbucket~.-1,data=ar1)
y=ifelse(ar1$growthbucket == "1", 1, 0)

grid = 10^seq(10,-2,length=100)
lasso.mod = glmnet(x,y,alpha = 1, lambda = grid, standardize = TRUE, family = "binomial")
summary(lasso.mod)
plot(lasso.mod)

set.seed(123)
cv.out = cv.glmnet(x,y,alpha = 1, family = "binomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x)
mean((lasso.pred-y)^2)

lasso.coef = predict(lasso.mod,type='coefficients',s=bestlam)
lasso.coef

select_s = which(lasso.coef>0 | lasso.coef<0)-1  
select_s[select_s>0]

ar1= ar1[c(4, 10, 15, 16, 19, 26, 29, 30, 31, 32, 36, 44, 45, 47, 49,50)]
#4, 10, 15, 16, 19, 26, 29, 30, 32, 36, 44, 45, 47, 49,50
#4, 10, 16, 19, 26, 29, 30, 31, 32, 36, 40, 45, 47, 49,50 tentativo originale
#4, 7, 10, 15, 16, 19, 26, 29, 30, 31, 32, 36,38, 44, 45, 47, 49,50 


#GLASSO DA NON METTERE RISULTATI NON ATTENDIBILI
#ar$growthbucket <- as.integer(ar$growthbucket)
#x <- as.matrix(ar)
#s <- var(x)
#
#rhos <- glassopath(s, rholist=NULL, thr=1.0e-4, maxit=1e4, approx=FALSE, 
#                   penalize.diagonal=TRUE, w.init=NULL,wi.init=NULL, trace=1)
#
##rho 0.01
#a<-glasso(s,rho=2000)
#AM <- a$wi!=0 # etimate K matrix
#diag(AM) <-FALSE
#g.lasso <- as(AM,"graphNEL")
#nodes(g.lasso) <- names(ar)
#glasso.ar <- cmod(edgeList(g.lasso),data=ar)
#plot(as(glasso.ar,"igraph"), vertex.size = 4, edge.arrow.size = 0.2)


###

#hist plots

ar1$growthbucket<-as.factor(ar1$growthbucket)
summary(ar1$growthbucket)

recession_plots <- gather(ar1,'feature','n',1:15)

ggplot(recession_plots)+geom_density(aes(n))+ 
  facet_wrap(~feature,scales='free')+labs(title= 'Density plot of predictors')+
  theme_few()+ylab('Distribution of values')+ xlab('')

#Train/Test sampling

set.seed(123)
train = sample(1:nrow(ar1), 0.7*nrow(ar1))
ar_train = ar[train,-16]
ar_test = ar[-train,-16]

###

#BN2
ar1$growthbucket <- ifelse(ar1$growthbucket == 2,
                                c(1), c(0))

bn.hc2 <- hc(ar1)
bn.hc2
am2 <- amat(bn.hc2)

plot(as(essentialGraph(am2),'igraph'), vertex.size = 4, edge.arrow.size = 0.25)

fittedbn2 <- bn.fit(bn.hc2, data = ar1)
print(fittedbn2$growthbucket)
print(fittedbn2$rtfpna)
print(fittedbn2$rwtfpna)
print(fittedbn2$csh_g)
print(fittedbn2$csh_r)

cv.hc = bn.cv(bn.hc2, data = ar1, runs = 10, method = "k-fold")
cv.hc



#logit = glm(formula = growthbucket ~ rtfpna+rwtfpna+csh_g+csh_r, data = ar1, family = binomial)
#summary(logit)

#NAIVE BAYES N

dar <- discretize(ar1, method = "interval", breaks = 10)
bn.nb = naive.bayes(dar, training = "growthbucket")

####


#HC maxmin

# learn the structure using the hill climbing algorithm and the BIC

#ar.mmhc <- mmhc(ar1)
#am3 <- amat(ar.mmhc)
#ar.mmhc
#plot(as(essentialGraph(am3),'igraph'),vertex.size = 4, edge.arrow.size = 0.2)

####

#logit

logit = glm(formula = growthbucket ~., data = ar1, family = binomial)
summary(logit)

roc1 <- predict(logit, ar1, type="link")
test_roc = roc(ar1$growthbucket ~ roc1, plot = TRUE, print.auc = TRUE)

#BOOSTING

#setting up cross-validation
cvcontrol <- trainControl(method="repeatedcv", number = 10,
                          allowParallel=TRUE)




#PC - INSERIRE QUESTO

C.ar <- cov2cor(cov.wt(ar1,method="ML")$cov)
suff.stat <- list(C=C.ar,n=nrow(ar1))
skeleton <- pcalg::skeleton(suff.stat, gaussCItest,p=ncol(ar1),alpha=0.05)
plot(skeleton)

pdag.ar<- udag2pdagRelaxed(skeleton, verbose = 0)
plot(pdag.ar)




#Inference


#corr

#res<-cor(ar2, use = "complete.obs")
#round(res, 3)
#
#symnum(res, abbr.colnames = FALSE)
#
#arm <- as.matrix(ar2)
#
#res2 <- pcor(arm)
#
#corrplot(res2$estimate, method="color",
#         type="upper", order="hclust",
#         addCoef.col = "black", tl.cex = 0.5, 
#         tl.col="black", tl.srt=90,
#         p.mat = res2$p.value, sig.level = 0.01,
#         number.cex = .5, insig = "blank", diag=FALSE)



#IVs



#random forest



#boosting 

#arboost=gbm(growthbucket~.,data=ar1[train,],distribution="gaussian",n.trees=1000,
#            shrinkage=0.01, interaction.depth=4)
#par(mar = c(5, 8, 1, 1))
#summary(arboost, cBars = 10,las = 2)
#
#lr_prob3 <- predict(arboost, ar1, type="response")
#test_roc = roc(ar1$growthbucket ~ arboost, plot = TRUE, print.auc = TRUE)
#
#par(mfrow=c(1,1))
#plot(arboost,i="rwtfpna")
#plot(arboost,i="delta")
#
#n.trees=seq(from=100,to=10000,by=100)
#predmat=predict(arboost,dt =ar1[-train,],n.trees=n.trees)
#dim(predmat)
#mean(predmat)
#
#test.err=double(13)
#berr=with(ar1[-train,],apply( (predmat - growthbucket)^2,2,mean))
#plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
#abline(h=min(test.err),col="red")
#
#adjboost =gbm(EFI~.,data=dt[train,],distribution="gaussian",n.trees=600,shrinkage=0.01, interaction.depth=4)
#par(mar = c(5, 8, 1, 1))
#summary(adjboost, cBars = 10,las = 2)

ar_train$growthbucket <- ifelse(ar_train$growthbucket == 2,
                      c(1), c(0))

cvcontrol <- trainControl(method="repeatedcv", number = 10,
                          allowParallel=TRUE)

train.gbm <- train(as.factor(growthbucket) ~ ., 
                   data=ar_train,
                   method="gbm",
                   verbose=F,
                   trControl=cvcontrol)
train.gbm

#obtaining class predictions
gbm.classTrain <-  predict(train.gbm, 
                           type="raw")
head(gbm.classTrain)

#### random forest




#Setting the random seed for replication
set.seed(123)









498[poi#stargazer(mtcars, type = 'text', out = 'out.txt')
