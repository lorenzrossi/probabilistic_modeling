library(readxl)
library(PerformanceAnalytics)
library(ggcorrplot)
library(mgm)
library(gRim)
library(bnlearn)
library(qgraph)
library(Rgraphviz)
library(igraph)
library(gRbase)
library(RBGL)
library(caret)
library(pROC)
library(ROCR)
library(FactoMineR)
library(factoextra)
library(ggpubr)
library(gRapHD)
library(SIN)
library(gRain)
library(randomForestExplainer)
library(e1071)
library(MASS)
library(infotheo)
library(varrank)
library(glmnet)
library(car)
library(Matrix)
library(Hmisc)
library(dplyr)
library(ade4)
library(car)
library(readr)
library(dplyr)
library(ggpubr)
library(olsrr)
library(tidyverse)
library(caret)
library(leaps)
library(paran)
library(ROSE)
library(reshape)
library(class)
library(tidyr)
library(kernlab)
library(corrplot)
library(Rgraphviz)
library(igraph)
library(ggplot2)
library(ggthemes)
library(ggm)
library(gRim)
library(gRapHD)
library(glmnet)
library(BiocManager)
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
library(rstatix)


ar <- read.csv('africa_recession.csv')
vd <- read.csv('VariableDefinitions.csv')
ar$growthbucket<-as.integer(ar$growthbucket)
ar <- na.omit(ar)
ar[1:49]<-scale(ar[1:49])

shap <- ar %>% shapiro_test(pop,emp, emp_to_pop_ratio,hc, ccon, cda, cn, ck, ctfp, cwtfp, rconna, rdana, rnna,
                     rkna,rtfpna,rwtfpna,labsh,irr,delta,xr,pl_con,pl_da,pl_gdpo,csh_c,csh_i,csh_g,
                     csh_x,csh_m,csh_r,pl_c,pl_i,pl_g,pl_x,pl_m,pl_n,total,excl_energy,energy,metals_minerals,
                     forestry,agriculture,fish,total_change,excl_energy_change,energy_change,metals_minerals_change,forestry_change,agriculture_change,
                     fish_change)


nzv <- nearZeroVar(ar)
nzv[!nzv %in% 1]
df <- ar[,-nzv[!nzv %in% 1]]
descrCor <- cor(ar)
descrCor <- na.omit(descrCor)
sum(abs(descrCor[upper.tri(descrCor)]) > 0.999)
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.79)
highlyCorDescr[!highlyCorDescr %in% 1]

ar <- ar[-c(23, 22, 21, 37, 41, 34, 39,  8,  7, 12,  6 ,13, 11 ,36, 40,  2 , 9, 43 ,46 ,48)]

recession_plots <- gather(ar,'feature','n',1:31)

ggplot(recession_plots)+geom_density(aes(n))+ 
  facet_wrap(~feature,scales='free')+labs(title= 'Density plot of predictors')+
  theme_few()+ylab('Distribution of values')+ xlab('')

#M_cov <- cov(ar[,-50])
#M_cor <- cor(ar[,-50])
#
#chart.Correlation(ar, histogram=TRUE, pch=19)
#testRes = cor.mtest(M_cor, conf.level = 0.95)
#corrplot(M_cor , p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
#         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, 
#         insig = 'label_sig', pch.col = 'grey20', order = 'AOE')

S.ar <- cov.wt(ar[,-50], method="ML")$cov
C.ar  <- cov2cor(S.ar)
res.lasso <- glasso(C.ar , rho=0.4)
AM <- res.lasso$wi!=0
diag(AM)<-FALSE
g.lasso <- as(AM,"graphNEL")
nodes(g.lasso)<-names(ar[,-50])
glasso.ar <- cmod(edgeList(g.lasso),data=ar[,-50])
plot(as(glasso.ar ,"igraph"),vertex.color="red",vertex.label.dist=2,alpha=TRUE,
     vertex.label.cex=0.8,vertex.size=10, edge.color="black",edge.size=2,
     layout=layout_with_fr(as(glasso.ar,'igraph')), vertex.label.family='Helvetica')

bf<-minForest(ar)
plot(bf,cex.vert.label=0.6,numIter=5000,col.labels=c("red"),vert.hl=c(26),col.hl=c("blue"),energy=TRUE)
mbG<-stepw(model=bf,data=ar)
plot(mbG,cex.vert.label=0.6,numIter=5000,col.labels=c("red"),vert.hl=c(26),col.hl=c("blue"),energy=TRUE)


fit_mgm <- mgm(data = ar,type = c(rep("g",29),"c"),levels = c(rep(1,29),2),k=2,lambdaSel = "CV",
               lambdaFolds= 20,ruleReg="AND",overparameterize = T)

qgraph::qgraph(fit_mgm$pairwise$wadj,
               layout = "spring", repulsion = 1,
               edge.color = fit_mgm$pairwise$edgecolor,
               nodeNames = colnames(ar),
               color = c(rep("lightblue",20),"orange"),
               legend.mode="style2", legend.cex=.4,
               vsize =4, esize = 10)


pred_obj <- predict(fit_mgm, ar)

round(pred_obj[["probabilties"]][[30]],2)

Predicted_vs_real<-as.data.frame(pred_obj[["predicted"]][,30])

Predicted_vs_real<-as.data.frame(as.factor(Predicted_vs_real$`pred_obj[["predicted"]][,26]`))

colnames(Predicted_vs_real)<-c("PRED")

levels(Predicted_vs_real$PRED) <- c("TRUE", "FALSE")
conf<-data.frame(Predicted_vs_real)
conf$growthbucket <- as.integer(ar$growthbucket)

Confusion_matrix_AR<-confusionMatrix(conf$growthbucket,conf$PRED)

fourfoldplot(Confusion_matrix_ASTEROIDS$table,color = c("red","darkgreen"),conf.level = 0)

Confusion_matrix_ASTEROIDS

pred_numeric<-prediction(as.numeric(conf$PRED),as.numeric(conf$Hazardous))

roc_mgm.perf <- performance(pred_numeric,measure = "tpr", x.measure = "fpr")

phi_Asteroids<-performance(pred_numeric, "phi")

phi_Asteroids@y.values[[1]][2]


df <- data.frame (roc_mgm.perf@x.values,roc_mgm.perf@y.values)

colnames(df)<-c("FPR", "TPR")

ggplot(df,aes(FPR,TPR))+geom_point(color="red")+geom_line(color="red")+ggtitle("ROC CURVE")+theme_bw()+theme(axis.text=element_text(size=16),axis.title=element_text(size=14,face="bold"))

mim <- build.mim(Asteroids_FINAL[,-1])
graph_mu<-aracne(mim)
plot(graph_from_graphnel( as( graph_mu ,"graphNEL")),layout=layout_with_fr(as(graph_mu,'igraph')) )







full <- glm(growthbucket~., data = ar, family = "binomial")
summary(full)
vif(full)
sqrt(vif(full)) > 10

ar <- ar[-c(1,2,5:13,21,20,36:41,43:46,48)]

recession_plots <- gather(ar,'feature','n',1:25)

ggplot(recession_plots)+geom_density(aes(n))+ 
  facet_wrap(~feature,scales='free')+labs(title= 'Density plot of predictors')+
  theme_few()+ylab('Distribution of values')+ xlab('')

