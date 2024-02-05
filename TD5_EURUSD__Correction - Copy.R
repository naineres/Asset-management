library(xts)
library(car)
library(TTR)
library(glmnet)
library(rpart)
library(rpart.plot)

### Import data and preprocessing

mainpath<-paste("Z://Misc//Cours//ESILV//2023//TD5//",sep="")
EUR<-read.csv(paste(mainpath,"EURUSD_2007.csv",sep=""),header=T,sep=";",row.names=1, dec=".")
EUR_Mat<-as.matrix(EUR)
EUR_xts=as.xts(EUR_Mat, order.by=as.POSIXct(rownames(EUR), format="%Y-%m-%d %H:%M:%S"))

nbobs=nrow(EUR)

mid = EUR$Mid

# TODO Question 1 : plot the first 100 values
plot(head(mid, 100))
# TODO Question 2 : compute returns using mid values
mid_returns = mid[2:nbobs]/mid[1:(nbobs-1)] - 1

nbreturns=nbobs-1

rsi_lookbacks = c(5*(1:6),60)
nbrissignals = length(rsi_lookbacks)

# TODO Question 3 : loop on the RSI lookbacks to compute rsi using the RSI function from TTR and store results in signals_rsi
?RSI # should help
signals_rsi = data.frame(matrix(nrow = nbobs, ncol = nbrissignals)) 
counter=0
for(k in rsi_lookbacks){
  counter = counter+1
  signals_rsi[,counter]=RSI(EUR$Mid, n = k)
}

# TODO Question 4 : compute macd using the MACD function from TTR and store results in signals_rsi
?MACD # should help
macd_init=MACD(EUR$Mid, nFast = 12, nSlow = 26, nSig = 9)
# Retain only the second variable
macd = macd_init[,2]
# Set NA to 0
macd[is.na(signals_macd)] = 0


### Set up variables

predictionHistory = 60

# TODO Question 5: compute future returns (hint:  predictionHistory should be taken into account twice)
returns_tobemodelled = mid[(2*predictionHistory+1):nbobs] / mid[1:(nbobs-2*predictionHistory)] - 1
Y = returns_tobemodelled

# Combine MACD and RSI signals
Xinit = cbind(macd, signals_rsi)
# TODO Question 6: build X so that it is the same size as Y and data are well specified
X=Xinit[(predictionHistory+1):(nbobs-predictionHistory),]


#### Full sample models

# TODO Question 7: plot correlations of Y with X
barplot(cor(Y,X))

# Preprocessing: normalization
Ycen = Y - mean(Y)
Xnorm = scale(X, center = TRUE, scale = TRUE)
Xnorm_df = as.data.frame(Xnorm)

# OLS
linModelFull = lm(y~., data = data.frame(Xnorm, y = Y))
summary(linModelFull)
# TODO Question 8: make predictions (in-sample) on the full dataset
prediction_full_ols = predict(linModelFull, Xnorm_df)

# LASSO
lso = cv.glmnet(Xnorm, Ycen, alpha = 1) # lasso
plot(lso$glmnet.fit, "lambda", label = TRUE, lwd = 4)
# TODO Question 9: make predictions (in-sample) on the full dataset
prediction_full_lasso = predict(lso, s = lso$lambda.min , newx = Xnorm_df)

# Regression tree
tree <- rpart(y ~ ., data=data.frame(X, y = Y))
# TODO Question 10: add constrains on the tree with the  cp (complexity parameter, set at .0001) and minsplit (set at 3750) variables
# calling ?rpart should help
tree_cp <- rpart(y ~ ., data=data.frame(X, y = Y), control=rpart.control(cp = .0001))
tree_split <- rpart(y ~ ., data=data.frame(X, y = Y), control=rpart.control(cp = .0001, minsplit = 3750))

# Print trees
printcp(tree)
printcp(tree_cp)
printcp(tree_split)

# Identify best tree according to lowest complexity parameter
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
# Pruned tree
pruned_tree <- prune(tree, cp=best)
# Plot pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

# TODO Question 11: make predictions using the pruned tree on the whole dataset
prediction_full_tree = predict(pruned_tree, newdata=X)

# Naive backtests
prediction_full_ols_df =  as.data.frame(prediction_full_ols)
prediction_full_tree_df = as.data.frame(prediction_full_tree)

positions_ols_df = data.frame(matrix(nrow = nbobs-2*predictionHistory, ncol = 1))
positions_tree_df = positions_ols_df
# TODO Question 13: go long (resp. short) when prediction is positive (resp. negative)
positions_ols_df[prediction_full_ols_df > 0] = 1
positions_ols_df[prediction_full_ols_df < 0] = -1
positions_ols_df[prediction_full_ols_df == 0] = 0

positions_tree_df[prediction_full_tree_df > 0] = 1
positions_tree_df[prediction_full_tree_df < 0] = -1
positions_tree_df[prediction_full_tree_df == 0] = 0

positions_ols = data.matrix(positions_ols_df)
positions_tree = data.matrix(positions_tree_df)
# TODO Question 14: build returns annd cumulative returns of the in-sample strategies
naive_fullsample_ols_returns = positions_ols * mid_returns[(nbreturns+1-2*predictionHistory):nbreturns]
naive_fullsample_ols_cumreturns = cumprod(1+naive_fullsample_ols_returns)

naive_fullsample_tree_returns = positions_tree * mid_returns[(nbreturns+1-2*predictionHistory):nbreturns]
naive_fullsample_tree_cumreturns = cumprod(1+naive_fullsample_tree_returns)

# TODO Question 15: why are these backtests not relevant? (no code required)


### Rolling models: build a model on one hour, apply it during next hour

rolling_history = 60

# See below the interest of using XTS: it is straighforward to select rebalancing times
Rebalancing_Dates = endpoints(EUR_xts, on = 'hours', k = 1)
Rebalancing_Dates = Rebalancing_Dates[Rebalancing_Dates > rolling_history-1]
Rebalancing_Dates = Rebalancing_Dates[Rebalancing_Dates <= length(Y)]

positions_ols_rolling = matrix(nrow = nbobs-rolling_history, ncol = 1)
positions_tree_rolling = positions_ols_rolling
# Loop on the rebalancing dates
for (Index_t in 1:(length(Rebalancing_Dates)-1)) {
  # First hour: build models
  t = Rebalancing_Dates[Index_t]
  rolling_Y = Y[(t-rolling_history+1):t]
  rolling_Ycen = rolling_Y - mean(rolling_Y)
  rolling_X = X[(t-rolling_history+1):t, ]
  rolling_X_norm = scale(rolling_X, center = TRUE, scale = TRUE)
  
  # TODO Question 16: build OLS and tree models
  linModel_rolling = lm(y~., data = data.frame(rolling_X_norm, y = rolling_Y))
  #lso = cv.glmnet(rolling_X_norm, rolling_Ycen, alpha = 1)
  tree_cp <- rpart(y ~ ., data=data.frame(rolling_X, y = rolling_Y), control=rpart.control(cp = .0001))
  # TODO Question 17: build an altternative tree model adding a constraint on minbucket (at 4)
  tree_bucket <- rpart(y ~ ., data=data.frame(rolling_X, y = rolling_Y), control=rpart.control(cp = .0001, minbucket = 4))
  
  # Apply on next hour
  next_t = Rebalancing_Dates[Index_t+1]
  # TODO Question 18: select variables on which the predictions wille be made
  rolling_X_next = X[(t+1):next_t, ]
  rolling_X_next_df = as.data.frame(rolling_X_next)
  rolling_X_norm_next_df = as.data.frame(scale(rolling_X_next, center = TRUE, scale = TRUE))
  
  # TODO Question 19: make predictions
  prediction_rolling_ols = predict(linModel_rolling, rolling_X_norm_next_df)
  prediction_rolling_tree = predict(tree_cp, newdata=rolling_X_next_df)
  prediction_rolling_ols_df = as.data.frame(prediction_rolling_ols)
  prediction_rolling_tree_df = as.data.frame(prediction_rolling_tree)
  
  # Building positions
  idx = (t+1) : Rebalancing_Dates[Index_t + 1]
  
  temp_positions_ols_df = data.frame(matrix(nrow = rolling_history, ncol = 1))
  temp_positions_tree_df = temp_positions_ols_df
  
  temp_positions_ols_df[ prediction_rolling_ols_df > 0] = 1
  temp_positions_ols_df[ prediction_rolling_ols_df < 0] = -1
  temp_positions_ols_df[ prediction_rolling_ols_df == 0] = 0
  temp_positions_ols = data.matrix(temp_positions_ols_df)
  positions_ols_rolling[idx]=temp_positions_ols
  
  temp_positions_tree_df[ prediction_rolling_tree_df > 0] = 1
  temp_positions_tree_df[ prediction_rolling_tree_df < 0] = -1
  temp_positions_tree_df[ prediction_rolling_tree_df == 0] = 0
  temp_positions_tree = data.matrix(temp_positions_tree_df)
  positions_tree_rolling[idx]=temp_positions_tree
  
}

# Simple backtests of rolling models

positions_ols_rolling[is.na(positions_ols_rolling)] = 0
naive_rolling_ols_returns = positions_ols_rolling * mid_returns[predictionHistory:nbreturns]
naive_rolling_ols_cumreturns = cumprod(1+naive_rolling_ols_returns)
plot(naive_rolling_ols_cumreturns)
positions_tree_rolling[is.na(positions_tree_rolling)] = 0
naive_rolling_tree_returns = positions_tree_rolling * mid_returns[predictionHistory:nbreturns]
naive_rolling_tree_cumreturns = cumprod(1+naive_rolling_tree_returns)

# Charts using XTS
naive_fullsample_cumreturns_xts = xts(cbind(naive_fullsample_ols_cumreturns,naive_fullsample_tree_cumreturns), order.by = index(EUR_xts[(2*predictionHistory++1):nbobs,]))
colnames(naive_fullsample_cumreturns_xts) = c('OLS', 'Tree')
plot(naive_fullsample_cumreturns_xts,main="Full sample strategies",legend.loc="topleft")

naive_rolling_cumreturns_xts = xts(cbind(naive_rolling_ols_cumreturns,naive_rolling_tree_cumreturns), order.by = index(EUR_xts[(rolling_history+1):nbobs,]))
colnames(naive_rolling_cumreturns_xts) = c('OLS', 'Tree')
plot(naive_rolling_cumreturns_xts,main="Rolling strategies",legend.loc="topleft")

# [EoF]