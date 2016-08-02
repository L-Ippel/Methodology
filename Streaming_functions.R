######################
## functions for analysis of data streams
######################
## in this file I define function for the sequential analysis of data, for instance data streams
## these functions can be used in a data stream or when the data are big and therefore difficult to 
## read in at once. Although not recommended, these functions work well in a for-loop
## you can feed back the results from a function directly back into function, 
## such that the function can incrementally update the result
## all functions work in the format: function(theta, input), where theta is 
## the current state of the sufficient statistics and input is a new data point. 
## Note that some functions require 2 data points (like covariance etc.)
## Furthermore the functions assume to update to n <- n + 1, 
## so dont feed the functions batches of data
######################

######################
## mean_online() is a function to compute the mean based on a new data point (input) and theta containing the current state
## in this case theta is a list containing 2 cells: sample size "n" and the current mean "Mean"
## both cells should have equal length, but don't have to be scalars, vectors work too
######################


mean_online	<- function(input, theta=NULL)
{
	if(is.null(theta)) 
	{
		theta <- list("n"=0,"Mean"=0)
	}
	theta$n	<- theta$n + 1					
	theta$Mean	<- theta$Mean + (input-theta$Mean)/theta$n 
	return(theta)
}


######################
## SS_online() is a function to compute the sum of squares based on a new data point (input) 
## and theta containing the current state. 
## In this case theta is a list containing 3 cells: sample size "n", the mean "Mean", the sum of squares "SS"
## this function makes use of mean_online function defined previously
## meaning that you should not update the mean yourself, it's done within this function!
## the length of Mean, SS, and input should be the same, scalars or vectors of equal length
## the result can be obtained using "name_object"$SS
#####################


SS_online	<- function(input, theta=NULL)
{
	if(is.null(theta)) 
	{
		theta <- list("n"=0,"Mean"=input,"SS"=0)
	}
	d		<- input-theta$Mean				
	theta		<- mean_online(theta, input)
	theta$SS	<- theta$SS + d*(input-theta$Mean)
	return(theta)
}

######################
## var_online() is a function to compute the variance based on a new data point (input) and theta containing the current state.
## In this case theta is a list containing 3 cells: sample size "n", the mean "Mean", the sum of squares "SS".
## This function makes use of SS_online function defined previously, 
## meaning that you should not update the mean or the sum of squares yourself, it's done within this function!
## the length of Mean, SS, and input should be the same, scalars or vectors of equal length
## the estimated variance is obtained using "name_object"$var
#####################

var_online	<- function(input, theta=NULL)
{
	if(is.null(theta)) 
	{
		theta		<- list("n"=1,"Mean"=input,"SS"=0, "var"=NA)
		print("number of data points too small to compute the variance")
	}
	theta			<- SS_online(theta, input)
	theta$var		<- theta$SS/(theta$n-1)
	return(theta)
}

######################
## sd_online() is a function to compute the standard deviation based on a new data point (input) and theta containing the current state.
## In this case theta is a list containing 3 cells: sample size "n", the mean "Mean", the sum of squares "SS".
## This function makes use of SS_online function defined previously, 
## meaning that you should not update the mean or the sum of squares yourself, it's done within this function!
## the length of Mean, SS, and input should be the same, scalars or vectors of equal length
## Mind that you need at least n=1 to start the function 
## the estimated standard deviation is obtained using "name_object"$sd
#####################

sd_online	<- function(input,theta=NULL)
{
	if(is.null(theta)) 
	{
		theta	<- list("n"=1,"Mean"=input,"SS"=0, "sd"=NA)
		print("number of data points too small to compute the standard deviation")
	}	
	theta		<- SS_online(theta, input)
	theta$sd	<- sqrt(theta$SS/(theta$n-1))
	return(theta)
}


######################
## SSxy_online() is a function to compute the sum of cross products based on a new data point (input) and theta containing the current state.
## In this case theta is a list containing 3 cells: sample size "n", the mean "Mean", the sum of cross products "SSxy".
## This function makes use of mean_online function defined previously, 
## meaning that you should not update the mean yourself, it's done within this function!
## 2 variables should be included in this function
## although in the paper we say "d" isn't necessary (and it really isn't)
## we chose to update both means at the same time, such that we only have to call the mean_online function 
## once and it prevents messiness with counting the sample size
## the result can be obtained using "name_object"$SSxy
#####################

SSxy_online	<- function(input_x, input_y,theta=NULL)
{	
	if(is.null(theta)) 
	{
		theta <- list("n"=0, "Mean"=c("x"=input_x,"y"=input_y),"SSxy"=0)
	}
	d		<- input_x-theta$Mean[1]
	theta		<- mean_online(theta, c(input_x,input_y))
	theta$SSxy	<- theta$SSxy + d*(input_y-theta$Mean[2])
	return(theta)
}

######################
## cov_online() is a function to compute the covariance based on a new data point (input) and theta containing the current state.
## In this case theta is a list containing 5 cells: sample size "n", the mean "Mean", the sum of cross products "SSxy".
## Sum of squares of x and the Sum of squares of y
## This function makes use of mean_online function defined previously, 
## meaning that you should not update the mean yourself, it's done within this function!
## 2 variables should be included in this function
## this function should start using the first data point to initiate the function
## although in the paper we say "d" isn't necessary (and it really isn't)
## we chose to update both means at the same time, such that we only have to call the mean_online function once and it prevents messiness with updates of theta
## The covariance is obtained using "name_object"$cov
#####################

cov_online	<- function(input_x, input_y, theta=NULL)
{
	# some of the code below is also used in other functions, we do not call these functions here
	# that is due to the fact that we would update the same parameter multiple times   
	if(is.null(theta)) 
	{
		theta		<- list("n"=1,"Mean"=c("x"=input_x, "y"=input_y),"SSxy"=0,"SSx"=0,"SSy"=0, "cov"=NA) 
		print("number of data points too small to compute the covariance")
	}
	dx			<- input_x-theta$Mean[1]
	dy			<- input_y-theta$Mean[2]	
	temp			<- mean_online(theta,c(input_x,input_y))
	theta$Mean		<- temp$Mean		
	theta$n		<- temp$n
	theta$SSxy		<- theta$SSxy + dx*(input_y-theta$Mean[2])
	theta$SSx		<- theta$SSx + dx*(input_x-theta$Mean[1])
	theta$SSy		<- theta$SSy + dy*(input_y-theta$Mean[2])
	theta$cov		<- theta$SSxy/(theta$n-1)
	return(theta)
}

######################
## cor_online() is a function to compute the correlation based on a new data point (input) and theta containing the current state.
## In this case theta is a list containing 5 cells: sample size "n", the mean "Mean", the sum of cross products "SSxy".
## Sum of squares of x "SSx" and the Sum of squares of y "SSy"
## This function makes use of cov_online function defined previously, 
## meaning that you should not update any of the sufficient statistics yourself, it's done within this function!
## 2 variables should be included in this function
## this function should start using the first data point to initiate the function
## The correlation is obtained using "name_object"$cor
#####################

cor_online	<- function(input_x, input_y, theta=NULL)
{
	theta		<- cov_online(theta, input_x, input_y)
	theta$cor	<- theta$cov/(sqrt(theta$SSx/(theta$n-1))*sqrt(theta$SSy/(theta$n-1)))
	return(theta)

}

#####################
## inv_matrix_online is a function which computes the inverse of the X'X matrix required for linear regression 
## theta is the inverted matrix, input is the new vector of observations
#####################

inv_matrix_online	<- function(input, theta=NULL)
{
	theta	<- theta - ((theta%*%input%*%t(input)%*%theta)/as.numeric((1+t(input)%*%theta%*%input)))
	return(theta)
}

#####################
## lm_online() fits a linear regression online, it can start from scratch. first input in input is the dependent variable 
## only n  and input is what the function requires, it computes and solves everything else on its own
## the estimated betas are obtained using "name object"$beta
#####################

lm_online	<- function(input_y, input_x,theta=NULL)
{
	if(is.null(theta)) 
	{
		theta <- list("n"=0, "xy"=rep(0,length(input_x)), 
                  "x_sq"=matrix(0, nrow=length(input_x), ncol=length(input_y)), "x_inv"=NULL, "beta"=rep(NA,input_x+1))
	}
	x		<- c(1,input_x) # the 1 is for the intercept
	theta$n	<- theta$n +1

	theta$xy	<- theta$xy+x*input_y
	if(theta$n <= (length(x)+1))
	{
		theta$x_sq 	<- theta$x_sq + x%*%t(x)
		print("n<p+1 : not enough data available")
	}
	if(theta$n==(length(x)+1))
	{
		theta$x_inv	<- solve(theta$x_sq)
		theta$beta	<- theta$x_inv%*%theta$xy
	}
	if(theta$n>(length(x)+1))
	{
		theta$x_inv	<- inv_matrix_online(theta=theta$x_inv, input=x)
		theta$beta	<- theta$x_inv%*%theta$xy	
	}
	return(theta)
}

######################
## etasq_online() this function returns the effect size eta squared. 
## This functions needs 2 types of input, a data point and to which group this data point belongs
## The function is written such it automatically includes new groups. 
## the estimated eta squared is obtained using "name object"$etasq
######################
etasq_online <- function(input, input_group,theta=NULL)
{
	if(is.null(theta)) 
	{
      	theta <- list("group_id"=c(input_group),"n_groups"=1, "SS_w"=0.00001, "SS_t"=0.00001, "mean"=input, "n"=0,
                                    "n_k"=c(0), "mean_k"=c(input))
	}
	if(sum(theta$group_id==input_group)==0)
	{
		theta$group_id  <- c(theta$group_id, input_group)
		theta$n_groups  <- theta$n_groups + 1
		theta$mean_k    <- c(theta$mean_k, input) 
	}
	group		<- which(theta$group_id==input_group)
	d		<- input-theta$mean
	dk		<- input-theta$mean_k[group]

	temp		<- mean_online(theta=list("n"=theta$n, "mean"=theta$mean), input=input)
	theta$mean	<- temp$mean
	theta$n	<- theta$n

	temp		<- mean_online(theta=list("n"=theta$n_k[group], "mean"=theta$mean_k[group]), input=input)
	theta$mean_k[group]	<- temp$mean
	theta$n_k[group]		<- theta$n

	theta$SS_t	<- theta$SS_t+ d*(input-theta$mean)
	theta$SS_w	<- theta$SS_w+ dk*(input-theta$mean_k[group])
  
	if(theta$n_group<2) 
	{
		print("not enough data available to compute eta squared")
	}  
	if(theta$n_group >=2)  
	{ 
		theta$"etasq" <- 1- theta$SS_w/theta$SS_t
	}
	return(theta)
}


######################
## sgd_log() : this function fits a logistic regression using sgd. The first argument
## input_x can be a vector or a scalar, in automatically includes a 1 for the intercept
## input_y should be a scalar
## learnrate can be changed if desired, default is 1/sqrt(n)
## estimated betas can be obtained using "name object"$beta, 
## default starting values for beta are 0 
######################

sgd_log   <- function(input_x, input_y, theta=NULL, LR=function(s){1/sqrt(s)})
{
	if(is.null(theta)) 
	{
		theta <- list("n"=0, "beta"=rep(0,(1+length(input_x))))
	}

	theta$n	<- theta$n + 1

	x		<-c(1,input_x)  #1 is for the intercept
	p		<- exp(sum(theta$beta*x))/(1+exp(sum(theta$beta*x)) )
	theta$beta	<-theta$beta + LR(theta$n)*(input_y-p) %*% x
	return(theta)
}






