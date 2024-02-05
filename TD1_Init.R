############################################################
# 1 - Donnees                                              #
############################################################

# Fichier source dispnible sur DVO.
# A copier sur votre ordinateur pour pouvoir l'importer.

library(xts)
mainpath<-paste("C:/Users/paull/Documents/Asset Management/TD1/DataForStatsTD1.csv",sep="")

# ou un seul '/' mainpath<-paste("C:/MyFolder/",sep="")

# Problemes typiques: separateur de texte, separateur de decimale

importData<-read.csv(mainpath , header=TRUE ,sep=";",row.names=1, dec=",")

allAssetPrices<-as.matrix(read.csv(paste(mainpath, sep=""), header=T, sep=";", row.names=1, dec="."))

matplot(allAssetPrices[,c("DMEquitiesEUR","DMEquitiesUSD")], main="ES50 and SP500", type = "l")

allAssetPrices_xts= as.xts(allAssetPrices, order.by = as.Date(rownames(allAssetPrices), "%d/%m/%Y"))


############################################################
# 2 - Exploration des donnees                              #
############################################################

# plot
plot(allAssetPrices_xts[ ,c("DMEquitiesEUR","DMEquitiesUSD")] ,main="ES50 and SP500")

# plot a subset of the data
plot(allAssetPrices_xts['2019/2020',c("DMEquitiesEUR","DMEquitiesUSD")], main="ES50 and SP500 - From 2019 to 2022", rebase=TRUE)

# Changer frequence

allAssetPrices_Daily_xts = allAssetPrices_xts
allAssetPrices_weekly_xts = to.weekly(allAssetPrices_Daily_xts, indexAt = "last", OHLC = FALSE)
allAssetPrices_Monthly_xts = to.monthly(allAssetPrices_Daily_xts, indexAt = "last", OHLC = FALSE)

head(allAssetPrices_Daily_xts)
head(allAssetPrices_weekly_xts)
head(allAssetPrices_Monthly_xts)

# ici, utiliser les fonctions to.weekly et to.monthly

# Fonction pour calculer les rendements a partir des prix
Compute_Return <- function(Price){
  n = length(Price)
  ret = Price/lag.xts(Price) # C'est equivalent a Price[2:n]/Price[1:(n-1)] - 1
  return(ret)
}

Compute_Return(allAssetPrices_Daily_xts$DMEquitiesCAD)

#Exemple

Prices_SP = allAssetPrices_Daily_xts[,'DMEquitiesUSD']
Returns_SP = Compute_Return(Prices_SP)
head(Returns_SP)

#######################################
# 3 - Calcul de statistiques usuelles #
#######################################

# Compute Compound Annual Growth Rate
# rendement géométrique annualisé

Compute_CAGR <- function(Price, Multiplicator = 252){
  n = length(Price)
  ret = (as.numeric(Price[n])/as.numeric(Price[1]))^(Multiplicator/n)-1
  return(ret)
}

# Exemple
CAGR_SP = Compute_CAGR(Prices_SP)
print(paste0('CAGR de SP 500 est: ', round(CAGR_SP*100, 2), "%"))


# Compute Volatility
Compute_Vol <- function(Price, Multiplicator = 252){
  
  ret = as.numeric(Price/lag.xts(Price) - 1)  # Calculer les rendements
  n = length(Price)                           # Obtenir la longueur de time series
  mu = mean(as.numeric(ret), na.rm = TRUE)
  sigma_daily = sqrt(sum((ret-mu)^2, na.rm=TRUE)/(n-1))
  sigma = sqrt(Multiplicator)*sigma_daily
  return (sigma)
}

# Exemple
Vol_SP = Compute_Vol(Prices_SP)
print(paste0('Vol de SP 500 est: ', round(Vol_SP*100, 2), "%"))

# Compute Drawdown and MaxDrawdown
Compute_DD <- function(Price){
  Price = as.numeric(Price)
  DrawDown = Price/cummax(Price)-1
  return (DrawDown)
}

# Exemple
DD = Compute_DD(Prices_SP) # calculer les drawdowns
plot(index(Prices_SP), DD, type="l", lwd = 2, xlab = "Date", ylab = "SPX 500 Index DrawDown")
MDD = min(DD)
print

# Compute Sharpe Ratio
Compute_SR <- function(Price, RetWithoutRisk = 0.00, Multiplicator = 252){
  CAGR = Compute_CAGR(Price, Multiplicator) # CAGR
  Vol = Compute_Vol(Price, Multiplicator) # Vol
  SR = (CAGR - RetWithoutRisk)/Vol
  
  return (SR)
}

# Exemple:
r = 0.02 # Taux sans risque
SR = Compute_SR(Prices_SP, r, 252) # Sharpe Ratio
print(paste0("Sharpe Ratio de SP 500 = ", round(SR, 2)))


# Examples to get sub xts

Prix_SP_subset1 = Price_SP["2000-01/2007-12"]
Prix_SP_subset2 = Price_SP["2008-01/2009-12"]
Prix_SP_subset3 = Price_SP["2010-01/2022-12"]
Prix_SP_subset4 = Price_SP[1001:2000]

SR_1 = Compute_SR(Prix_SP_subset1, r , 252) # Sharp Ratio 2000-2007
print(paste0("Sharpe Ratio of SPX 500 Index (2000-2009) = ", round(SR_1,2)))

SR_2 = Compute_SR(Prix_SP_subset2, r , 252) # Sharp Ratio 2008-2009
print(paste0("Sharpe Ratio of SPX 500 Index (2008-2009) = ", round(SR_2,2)))

SR_3 = Compute_SR(Prix_SP_subset3, r , 252) # Sharp Ratio 2010-2022
print(paste0("Sharpe Ratio of SPX 500 Index (2010-2022) = ", round(SR_3,2)))

# Rolling & expanding Sharpe Ratio
# Hint: utiliser rollaply

Rolling_SR = rollapply(zoo(Prices_SP), width = 252*3, Compute_SR, by = 22*3, align="right")
plot(as.xts(Rolling_SR), main = "Rolling SR")

Expanding_SR = rollapply(zoo(Prices_SP), width=1:length(Prices_SP), Compute_SR, align = "right")
Expanding_SR[!is.finite(Expanding_SR)]<-0
plot(as.xts(Expanding_SR), main="Expanding SR")


#############################################
# 4 - Calcul de covariances et correlations #
#############################################


library(corrplot)



##########################
# 5 - Creation d'indices #
##########################

# Bravo d'etre arrives la !


#########################
# 6 - Un premier modele #
#########################

# Import du nouveau dataset
# Utiliser read.csv

# Transformer les variables:
# Core PCE into Core PCE - 2%
# Unemployment into 5% - Unemployment
# Combine ED2 and ED6 into a spread


#### Ordinary least squares (OLS) regression #### 
# Full history

# Utiliser les fonctions lm, summary et predict


# Last 10y

# Last 5y

