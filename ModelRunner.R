library(xgboost)
library(dplyr)
library(DiagrammeR)
library(pcaPP)
source("EquitiesModelSetup.R")
source("EquitiesData.R")
#sp1018 <- get_tq_sp500_data('2010-01-01','2018-12-31')
head(sp1018)

ad <- sp1018 %>% dplyr::filter(symbol == 'AAPL')
head(ad)

start <- 20100104
end <- 20181227
ffdat <- get_KFDL_3Factor(start,end)
xgexog <- xgboost_return_exog(ad$adjusted, c(1,2,3,4,5), c(2,5,10,20,30,40), ffdat[-(1:2),])
head(xgexog,10)


#run xgboost

binXG <- xgFitPredict(ad$adjusted, c(1,2,3,4,5), c(2,5,10,20), ffdat[-1,], 2000, 173,"Mod")
aPRes <- xgFitPredict(ad$adjusted, c(1,2,3,4,5), c(2,5,10,20), ffdat[-1,], 2000, 173,"Acc")
print("In testing on unadjusted predictions we got") 
print(aPRes)
print("percent correct. Test Error = ") 
print(0.01*(100 - aPRes))
trGr <- xgb.plot.tree(model=binXG, feature_names = c("MKT", "SMB", "HML","RF", "Lag1","Lag2","Lag3","Lag4","Lag5" ,"Prev2", "Prev5","Prev10","Prev20","Prev30","Prev40"), trees=NULL, render=FALSE)
export_graph(trGr, 'AppleTreesAll.pdf', width=1500, height=1900)

brm <- big_returns_mat(sp1018, 1)
cor(brm, method="spearman")
system.time(cor(brm[1:400,], method="kendall"))#100:7.721. 200:30.49. 300: 67.758, 120.327
#should take almost exactly an hour for 1:2200...
times <- c(7.721,30.49,67.758,120.327)
n <- c(100,200,300,400)
n2 <- n^2
print(n2)
nln <- n*log(n)
print(nln)
kcormod <- lm(times ~ n + n2)
kcornln <- lm(times ~ n + nln)
summary(kcormod)
summary(kcornln)
coef(kcormod)
plot(kcormod)
AIC(kcormod)
AIC(kcornln)#much higher AIC
#strong evidence found that cor(x, method="kendall") runs in O(n^2) instead of 
#O(n*log(n)), n being the number of rows.

#correct Kendall correlation
cor.fk(brm)

factors <- ffdat[-1,]
allXGPreds <- xgBigDataEval(sp1018,c(1,2,3,4,5), c(2,5,10,20), factors, 2000, 173)
head(allXGPreds)
mean(as.matrix(allXGPreds))
min(as.matrix(allXGPreds))
totb50 <- 0
for(i in 1:456){
  if(as.matrix(allXGPreds)[1,i] < 50){
    totb50 <- totb50 + 1
  }
}
prmat <- as.matrix(allXGPreds)
#mean correct preds
mean(prmat[prmat < 50]) * (totb50 / 456) + mean(prmat[prmat > 50]) * (1- totb50 / 456)
mean(prmat)
