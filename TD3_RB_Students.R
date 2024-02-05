# Getting ready

library('nloptr')

#-------------------------------------------------------------------------------
# Data

# Example annualised data
# Expected return
mu = c(0.01, -0.01, 0.03, -0.03, 0.05, -0.05)
# Covariance matrix
sigma = c (0.06101, 0.02252, 0.03315, 0.03971, 0.04186, 0.04520,
           0.02252, 0.08762, 0.04137, 0.04728, 0.05241, 0.05310,
           0.03315, 0.04137, 0.10562, 0.06210, 0.06885, 0.06574,
           0.03971, 0.04728, 0.06210, 0.11357, 0.07801, 0.07790,
           0.04186, 0.05241, 0.06885, 0.07801, 0.19892, 0.09424,
           0.04520, 0.05310, 0.06574, 0.07790, 0.09424, 0.36240)
sigma = matrix (sigma, nrow = 6)
sigma

#-------------------------------------------------------------------------------
# Functions

# Objective function and gradient, written separately
eval_logwLongOnly = function(w, riskTarget, covMatrix) {
  return ( -sum (log (w))) 
}
eval_grad_logwLongOnly = function(w, riskTarget, covMatrix) {
  return ( -1 / w )
}

#-------------------------------------------------------------------------------
# Les questions arrivent !

# Objective function and gradient
# Questions 1 et 2: écrire la fonction d'optimisation et le gradient en une seule ?tape
eval_logwLongOnly = function(w, riskTarget, covMatrix) {
  return( list("objective"= eval_logwLongOnly(w, riskTarget, covMatrix) , #Question 1 - Quelle est la fonction objectif ?
               "gradient"= eval_grad_logwLongOnly(w, riskTarget, covMatrix)  ) ) #Question 2 - Quel est le gradient associ? ?
}
# Remarque: l'?tape pr?c?dente (?criture de la fonction objectif et du gradient s?par?ment)
# est similaire ? l'?criture simultan? de la fonction objectif et du gradient
  
# Constraint function
eval_Volatility= function( w, riskTarget, covMatrix) {
    w = as.matrix( w ,ncol = 1)
    risk = sqrt(  )[1] #Question 3 - Comment ?crire la volatilit? de mani?re matricielle ?
    return( list( "constraints"= risk-riskTarget,
                  "jacobian"=  ) ) #Question 4 - Quel est le jacobien ? Indice: c'est la contribution marginale, cf CM5-6
  }

# Optimisation code
numAssets = ncol( sigma)
initial = rep(1, numAssets )
initial = initial / sum( initial)
riskTarget = sqrt (t(initial) %*% sigma %*% initial)

res0 <- nloptr( x0=initial,
                eval_f=,#Fonction d'?valuation
                lb = rep(1e-6, numAssets),
                ub = rep(1, numAssets),
                eval_g_ineq = eval_Volatility,
                opts = list("algorithm" = "NLOPT_LD_SLSQP",
                            "print_level" = 0,
                            "maxeval" = 500,
                            "check_derivatives" = FALSE,
                            "check_derivatives_print" = "all"),
                riskTarget = riskTarget, covMatrix =sigma )
#Question 5 -  Quel est le portefeuille issu de l'optimisation ? V?rifiez que  les contributions au risque des actifs sont identiques
#Question 6 -  Comment cr?er un portefeuille dont les poids somment ? un ? Quelles sont les contributions au risque des actifs ?
# Vous retrouverez la d?finition de la contribution au risque sur le m?me slide du CM5-6


# Long / short by pair optimisation code
eval_logwLongShort = function(w, mu, riskTarget, covMatrix) {
  return( list("objective"=-sum( abs(mu) * log( abs(w) )) ,
               "gradient"= -abs(mu) / w ) )
}
eval_Volatility2 = function( w, mu, riskTarget, covMatrix) {
  w = as.matrix( w ,ncol = 1)
  risk = sqrt( t(w)%*% covMatrix %*% w )[1]
  return( list( "constraints"= risk - riskTarget,
                "jacobian"= t(w)%*% covMatrix / risk ) )
}
numAssets = ncol( sigma)
initial = rep(0, numAssets )
initial [mu > 0] = 1
initial [mu < 0] = -1
initial = initial / sum( mu > 0)
riskTarget = sqrt (t (initial) %*% sigma %*% initial)
#Question 7 - D?terminer lb
#Quetsion 8 - D?terminer ub
res1 <- nloptr( x0=initial,
                eval_f=eval_logwLongShort,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_Volatility2,
                opts = list("algorithm" = "NLOPT_LD_SLSQP",
                            "print_level" = 0,
                            "maxeval" = 500,
                            "check_derivatives" = FALSE,
                            "check_derivatives_print" = "all"),
                mu = mu, riskTarget = riskTarget, covMatrix =sigma )
#Question 9 -  Quel est le portefeuille r?sultant de cette optimisation ?

# Long / short optimisation with market neutrality constraint.
eval_MarketNeutral = function(w, mu, riskTarget, covMatrix) {
  return( list( "constraints"= sum( w),
                "jacobian"= rep(1, length(w) ) ) )
}
res2 <- nloptr( x0=initial,
                eval_f=eval_logwLongShort,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_Volatility2,
                eval_g_eq = eval_MarketNeutral, ## Equality constraint
                opts = list("algorithm" = "NLOPT_LD_SLSQP",
                            "maxeval" = 500),
                mu = mu, riskTarget = riskTarget, covMatrix =sigma )
#Question 10 - Comparer le r?sultat aux autres optimisations
# Tous ces portefeuilles sont des portefeuilles optimaux, m?me s'ils ne se ressemblent pas

#Question 11 - Construction de portefeuilles ERC rolling
#Pour les questions 11 et 12, vous vous inspirerez de la question II.3 du TD2
#Vous commencerez par charger les donn?ees fournies:
# Stocks = read.csv(file="...
#Il faudra ensuite g?n?rer les rendements (la fonction compute_return vous rappelle-t-elle quelque chose ?)
#Puis boucler tous les 22 jours et optimiser un portefeuille

#Question 12 - Faire le graphe de ces portefeuilles en utilisant la librairie ggplot2
