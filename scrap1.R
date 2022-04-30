library(readr)
library(Matrix)
library(readr)
library(Hmisc)
library(dplyr)
library(caret)
library(ade4)
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

#many hist

recession_plots <- gather(ar,'feature','n',1:49)

ggplot(recession_plots)+geom_density(aes(n))+ 
  facet_wrap(~feature,scales='free')+labs(title= 'Density plot of predictors')+
  theme_few()+ylab('Distribution of values')+ xlab('')

#BN

bn.hc <- hc(ar)
am <- amat(bn.hc)
am
bn.hc

plot(as(essentialGraph(am),'igraph'), edge.arrow.size = 0.2, vertex.size = 4)

#modello difficile da interpretare

#############

#lasso

ar1 <- ar

ar1$growthbucket<-as.integer(ar1$growthbucket)

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}
ar1 <- as.data.frame(lapply(ar1,normalize))

set.seed(123)
train = sample(1:nrow(ar1), 0.7*nrow(ar1))

x=model.matrix(growthbucket~.-1,data=ar1)
y=ar1$growthbucket

grid = 10^seq(10,-2,length=100)
lasso.mod = glmnet(x,y,alpha = 1,lambda = grid)
summary(lasso.mod)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x,y,alpha = 1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x)
mean((lasso.pred-y)^2)

out = glmnet(x,y,alpha=1,lambda=grid)
lasso.coef = predict(out,type='coefficients',s=bestlam)
lasso.coef

select_s = which(lasso.coef>0 | lasso.coef<0)-1  
select_s[select_s>0]

ar1= ar1[c(4, 10, 16, 19, 26, 29, 30, 31, 32, 36, 40, 45, 47, 49,50)]


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

ar1$growthbucket<-as.factor(ar1$growthbucket)
summary(ar1$growthbucket)

recession_plots <- gather(ar1,'feature','n',1:14)

ggplot(recession_plots)+geom_density(aes(n))+ 
  facet_wrap(~feature,scales='free')+labs(title= 'Density plot of predictors')+
  theme_few()+ylab('Distribution of values')+ xlab('')

#BN2


bn.hc2 <- hc(ar1)
bn.hc2
am2 <- amat(bn.hc2)
am2

plot(as(essentialGraph(am2),'igraph'), vertex.size = 4, edge.arrow.size = 0.25)


####


#HC maxmin

# learn the structure using the hill climbing algorithm and the BIC

#ar.mmhc <- mmhc(ar1)
#am3 <- amat(ar.mmhc)
#ar.mmhc
#plot(as(essentialGraph(am3),'igraph'),vertex.size = 4, edge.arrow.size = 0.2)

####

rF <- minForest(ar1)
rF
plot(rF)
nby <- gRapHD::neighbourhood(rF,orig=15, rad=5)$v[,1]
plot(rF,vert=nby, numIter=1000)
####



#PC - INSERIRE QUESTO

ar2 <- ar1
ar2$growthbucket = as.integer(ar2$growthbucket)
C.ar <- cov2cor(cov.wt(ar2,method="ML")$cov)
suff.stat <- list(C=C.ar,n=nrow(ar2))
skeleton <- pcalg::skeleton(suff.stat, gaussCItest,p=ncol(ar2),alpha=0.05)
plot(skeleton)

pdag.ar<- udag2pdagRelaxed(skeleton, verbose = 1)
plot(pdag.ar)

#Inference

set.seed(123)
train = sample(1:nrow(ar), 0.7*nrow(ar))
ar_train = ar1[train,-1]
ar_test = ar1[-train,-1]
ar_train_labels <- ar1[train, 1]
ar_test_labels <- ar1[-train, 1]

#corr

res<-cor(ar2, use = "complete.obs")
round(res, 3)

symnum(res, abbr.colnames = FALSE)

ar2 <- as.matrix(ar2)

res2 <- pcor(ar2)

corrplot(res2$estimate, method="color",
         type="upper", order="hclust",
         addCoef.col = "black", tl.cex = 0.5, 
         tl.col="black", tl.srt=90,
         p.mat = res2$p.value, sig.level = 0.01,
         number.cex = .5, insig = "blank", diag=FALSE)

corrplot(res2$estimate, type="upper", order = "original", tl.cex = 0.75, tl.srt = 45, tl.col = "black", 
         p.mat = res2$p.value, sig.level = 0.05, insig = "blank")

#logit

logit = glm(formula = growthbucket ~ cwtfp + csh_g + csh_r + pl_i + pl_g + forestry + agriculture + fish_change, data = ar1, family = binomial)
summary(logit)

library(car)
vif(logit)
sqrt(vif(logit)) > 10
sqrt(vif(logit)) > 5


library(robustbase)
ar2 <- ar1
ar2$growthbucket = as.integer(ar2$growthbucket)
res <- lmrob(growthbucket ~., data=ar2)
summary(res)
cbind(coef(res),confint(res, level = 0.95))

library(robust)
rob <- glmRob(growthbucket ~ cwtfp + csh_r, data = ar1, family = binomial(), method = 'mallows')

summary.glmRob(rob, correlation = F, bootstrap.se = F)

#boosting 



#### random forest



