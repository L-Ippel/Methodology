#######################
## Stochastic Gradient  Decent - logistic regression 
#######################

library(MASS)
################
##generate data
################
n_pred	<- 1									# predictors and an intercept
averages	<- rep(0,n_pred)							# set a mean for each of the variables 
var_matrix	<- matrix(0.2, nrow=n_pred, ncol=n_pred) + diag(n_pred)*.8 	# specify the variance covariance matrix  
N		<- 30000 								# sample size
Xvar		<- cbind(1,mvrnorm(n=N, mu=averages, Sigma=var_matrix))#  independent variables
Beta		<- c(2,3)#runif(n_pred+1, -5,5)					# regression coefficients 
e		<- rnorm(N)
Yvar		<- rbinom(N,1,exp(rowSums(Beta*Xvar+e))/(1+exp(rowSums(Beta*Xvar+e))))			# the dependent variable is predicted from independent variables plus some random noise


###############
##starting values
###############

beta		<- rep(1,n_pred+1)
n		<- 0
###############
##run through the data
###############


for(i in 1:nrow(Xvar))
{
	n	<- n + 1
	p	<- exp(sum(beta*Xvar[i,]))/(1+exp(sum(beta*Xvar[i,])) )
	beta	<- beta + 1/sqrt(n)*(Yvar[i]- p) %*%Xvar[i,]
	print(beta)
}
