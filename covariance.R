#################
## covariance 
#################

## We require 2 correlated variables, 
## and an easy way of generating 2 variables that are correlated is using the 
## mvrnorm (multivariate normal distribution) function from the MASS package

library(MASS)
 
################
##generate data
################

averages 	<- rep(0,2)							# set a mean for each of the variables 
var_matrix 	<- matrix(.5, nrow=2, ncol=2) + diag(2)*.3 	# specify the variance covariance matrix  
N		<- 1000 							# sample size
data 		<- mvrnorm(n=N, mu=averages, Sigma=var_matrix)	# data
 
################
##set starting values
################

online_meanx1	<- 0
online_meanx2	<- 0
online_SCP		<- 0							
n			<- 0

################
##run through the data 
################

for(i in 1:nrow(data))
{
	n			<- n+1
	online_meanx1	<- online_meanx1 + (data[i,1]-online_meanx1)/n
	online_SCP		<- online_SCP + (data[i,1]-online_meanx1)*(data[i,2]-online_meanx2)
	online_meanx2	<- online_meanx2 + (data[i,2]-online_meanx2)/n
	online_cov		<- online_SCP/(n-1) 
	print(online_cov)
}

