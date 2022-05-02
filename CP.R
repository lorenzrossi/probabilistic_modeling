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


cb <- read.csv('Company_Bankruptcy.csv')
cb$Bankrupt.<-as.factor(cb$Bankrupt.)
cb[,c(2:31)] <- scale(cb[,c(2:31)])
cb <- na.omit(cb)

table(cb$Bankrupt.)

barplot(table(cb$Bankrupt.),col=rainbow(2),
        main="Frequency of Bankruptcy",
        xlab="Recession Tag",
        ylab="Number of Recession Events")

cb_balanced <- ROSE(Bankrupt. ~ ., data=cb, seed = 123)$data

cb$Bankrupt.<-as.integer(cb$Bankrupt.)

S.cb <- cov.wt(cb, method="ML")$cov
C.cb  <- cov2cor(S.cb)
res.lasso <- glasso(C.cb , rho=0.4)
AM <- res.lasso$wi!=0
diag(AM)<-FALSE
g.lasso <- as(AM,"graphNEL")
nodes(g.lasso)<-names(cb)
glasso.cp <- cmod(edgeList(g.lasso),data=cb)
plot(as(glasso.cp ,"igraph"),vertex.color="red",vertex.label.dist=0.5,alpha=TRUE,
     vertex.label.cex=0.6,vertex.size=8, edge.color="black",edge.size=5,
     layout=layout_with_fr(as(glasso.cp,'igraph')), vertex.label.family='Helvetica')


bf<-minForest(cb)
plot(bf,cex.vert.label=0.6,numIter=6000,col.labels=c("red"), edge.size = 0.5, vert.hl=c(1),col.hl=c("blue"),energy=TRUE)
mbG<-stepw(model=bf,data=cb)
plot(mbG,cex.vert.label=0.6,numIter=6000,col.labels=c("red"),vert.hl=c(1),col.hl=c("blue"),energy=TRUE)

levels(cb_balanced)

cb_balanced$Bankrupt.<-as.integer(cb_balanced$Bankrupt.)

str(cb_balanced)

fit_mgm <- mgm(data = cb_balanced,type = "c", c(rep("g",30)), levels = 2,c(rep(1,30)),k=2,lambdaSel = "CV",
               lambdaFolds= 10,ruleReg="AND",overparameterize = T)

qgraph::qgraph(fit_mgm$pairwise$wadj,
               layout = "spring", repulsion = 1.3,
               edge.color = fit_mgm$pairwise$edgecolor,
               nodeNames = colnames(Asteroids_FINAL[,-1] ),
               color = c(rep("lightblue",20),"purple"),
               legend.mode="style2", legend.cex=.4,
               vsize = 3.5, esize = 15)


