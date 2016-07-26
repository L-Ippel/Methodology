#################
##linear regression 
#################

## We require variables, that might be correlated. 
## We use mvrnorm (multivariate normal distribution) function from the MASS package

library(MASS)

################
##generate data
################
n_pred	<- 4									# number of predictors (without intercept)
averages 	<- rep(0,n_pred)							# set a mean for each of the variables 
var_matrix 	<- matrix(0.2, nrow=n_pred, ncol=n_pred) + diag(n_pred)*.3 	# specify the variance covariance matrix  
N		<- 3000 								# sample size
Xvar 		<- cbind(1,mvrnorm(n=N, mu=averages, Sigma=var_matrix))# data consisting of independent variables including 1's for intercept
Beta		<- runif(n_pred+1, -5,5)					# regression coefficients 
Yvar		<- rowSums(Beta*Xvar+rnorm(N,0,2))			# the dependent variable is predicted from independent variables plus some random noise

data		<- cbind(Xvar, Yvar)
 
################
##set starting values
################
online_beta	<- rep(NA,n_pred+1)
Xmat		<- matrix(0,n_pred+1,n_pred+1)
n		<- 0
XYproduct	<- rep(0,n_pred+1)

for(i in 1:nrow(data))
{
	n			<- n+1
	XYproduct		<- XYproduct +data[i,1:(n_pred+1)]*data[i,(n_pred+2)]
	if(n <= (1+length(online_beta)))	
	{ #before the number of observations exceeds the number of predictors, the X'X matrix is not positive definite and can therefore not be inverted
		Xmat		<- Xmat + data[i,1:(1+n_pred)]%*%t(data[i,1:(1+n_pred)])
	}
	if(n==1+length(online_beta))
	{
		inv_Xvar	<- solve(Xmat)	#X'X matrix is inverted only once after which we update the inverted matrix directly
	}	
	if(n>1+length(online_beta))
	{
		inv_Xvar	<- inv_Xvar-((inv_Xvar%*%data[i,1:(1+n_pred)]%*%t(data[i,1:(1+n_pred)])%*%inv_Xvar)/as.numeric(1+t(data[i,1:(1+n_pred)])%*%inv_Xvar%*%data[i,1:(1+n_pred)]))			
		online_beta <- inv_Xvar%*%XYproduct
	}
	print(online_beta)
}
