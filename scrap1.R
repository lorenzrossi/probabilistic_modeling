library(BiocManager)
library(RBGL)
library(Rgraphviz)
library(igraph)
library(gRbase)
library(bnlearn)
library(mvtnorm)
library(matrixcalc) 

setwd('C:\\Users\\loren\\OneDrive\\Desktop\\Lezioni uni\\Prob Model\\progetto')

heart_2020_cleaned <- read.csv("C:/Users/loren/OneDrive/Desktop/Lezioni uni/Prob Model/progetto/heart/heart_2020_cleaned.csv")
hd <- heart_2020_cleaned
rm(heart_2020_cleaned)