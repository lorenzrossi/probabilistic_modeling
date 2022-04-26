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

ar <- read.csv('africa_recession.csv')
vd <- read.csv('VariableDefinitions.csv')
cb <- read.csv('company bankruptcy.csv')

data <- na.omit(file)

# Keep/drop columns (TYPE IN THE COL NO.)

data <- data[-c(50)]
summary(data)

library('Matrix')
library('glmnet')
library('boot')
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

select_s = which(lasso.coef>0 | lasso.coef<0)-1  # 去掉截距项
select_s[select_s>0]
data_x = re[select_s]          # 变量选择
data_y = y

nby <- gRapHD::neighbourhood(bF,orig=1001, rad=4)$v[,1]
plot(bF,vert=nby, numIter=1000)