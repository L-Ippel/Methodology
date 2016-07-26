#######################
## Stochastic Gradient  Decent - logistic regression 
#######################

library(MASS)
################
##generate data
################
n_pred	    <- 1									# predictors and an intercept
averages  	<- rep(0,n_pred)							# set a mean for each of the variables 
var_matrix 	<- matrix(0.2, nrow=n_pred, ncol=n_pred) + diag(n_pred)*.8 	# specify the variance covariance matrix  
N		        <- 30000 								# sample size
Xvar 		    <- cbind(1,mvrnorm(n=N, mu=averages, Sigma=var_matrix))#  independent variables
Beta		    <- runif(n_pred+1, -5,5)					# regression coefficients 
e           <- rnorm(N)
Yvar	      <- rbinom(N,1,exp(rowSums(Beta*Xvar+e))/(1+exp(rowSums(Beta*Xvar+e))))			# the dependent variable is predicted from independent variables plus some random noise
LR          <- 0.1 

###############
##starting values
###############

beta        <- rep(0,n_pred+1)
###############
##run through the data
###############


for(i in 1:nrow(data))
{
  p <- exp(sum(beta*Xvar[i,]))/(1+exp(sum(beta*Xvar[i,])) )
  beta <-beta + LR*(Yvar[i]- p) %*%Xvar[i,]
}


