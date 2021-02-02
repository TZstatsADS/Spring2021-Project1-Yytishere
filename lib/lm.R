setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Columbia/01_APPLIED DATA SCIENCE/Project1-RNotebook/data")

df_init<-read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Columbia/01_APPLIED DATA SCIENCE/Project1-RNotebook/data/anes_pilot_2020ets_csv.csv")
library(haven)
df_init2<-read_sav("~/Library/Mobile Documents/com~apple~CloudDocs/Columbia/01_APPLIED DATA SCIENCE/Project1-RNotebook/data/anes_pilot_2020ets_sav.sav")
names(df_init2)
# 2.1 missing vals and types
library(DataExplorer)
plot_missing(df_init[,c(210:230)])
plot_missing(df_init[,c(450:470)])
summary(df_init)

names(df_init)
# 2.2 cleaning for lm

# fit a model on the whole dataset to find significant variables  
df_reg<-df_init[,c(11:470)]
# Partitioning; get 20% test set
splitPercent <- round(nrow(df_reg) %*% .8)
set.seed(1234)
idx      <- sample(1:nrow(df_reg), splitPercent)
trainSet <- df_reg[idx, ]
testSet  <- df_reg[-idx, ]
# fit the model
lmfit <- lm(vote20jb ~ ., trainSet)
# Examine results and output them in a csv file
lmfit
# store the results
coe<-lmfit$coefficients
res<-lmfit$residuals
pvalue<-coef(summary(lmfit))[,4]
output<-cbind(coe,res,pvalue)
write.csv(output,'outputmodel.csv')
