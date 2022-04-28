library(BiocManager)
library(RBGL)
library(Rgraphviz)
library(igraph)
library(gRbase)
library(bnlearn)
library(bnstruct)
library(UPG)
library(mvtnorm)
library(matrixcalc)
library(datasets)
library(readr)
library(Matrix)
library(glmnet)
library(boot)
library(glasso)
library(readr)
library(ggplot2)
library(ggthemes)
library(ggcorrplot)
library(dplyr)
library(caret)
library('ade4')
library("paran")
library("ROSE")
library('pROC')
library('reshape')
library('class')
library('tidyr')
library('kernlab')

ar <- read.csv('africa_recession.csv')
vd <- read.csv('VariableDefinitions.csv')
cb <- read.csv('company bankruptcy.csv')

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

#now we check the distribution of the independent variables
#since we have about 50 independet variables we will partition the charts
par(mfrow = c(10,5))
i <- 1
for (i in 1:50) 
{
  hist((ar[,i]), main = paste("Distibution of ", colnames(ar[i])), xlab = colnames(ar[i]))
}

recession_plots <- gather(ar,'feature','n',1:49)

ggplot(recession_plots)+geom_density(aes(n))+ 
  facet_wrap(~feature,scales='free')+labs(title= 'Density plot of predictors')+
  theme_few()+ylab('Distribution of values')+ xlab('')

#BN

bn.hc <- hc(ar)
am <- amat(bn.hc)

library('ggm')
plot(as(essentialGraph(am),'igraph'))




#pm lasso
C.body <- cov2cor(S.body)
res.lasso <- glasso(C.body,rho=0.1)
AM <- res.lasso$wi!=0 # etimate K matrix
diag(AM) <-FALSE
g.lasso <- as(AM,"graphNEL")
nodes(g.lasso) <- names(gRbodyfat)
glasso.body <- cmod(edgeList(g.lasso),data=gRbodyfat)
plot(as(glasso.body,"igraph"))

graph :: degree(as(glasso.body,"graphNEL"))



