#############################
## code for Wells example 
#############################
##data
#############################

#set your working directory to the directory where wells.dat is stored, 
#then read in the data:
mydat			<- read.table("wells.dat")

#distance measures is n 100 meters is more convenient 
mydat$dist100	<- mydat$dist/100

#############################
##starting values
#############################
#feel free to change these numbers:
beta			<- c(0,0,0,0)	#or even add other variables by including more numbers here
n			<- 0 #don't change this one though
#############################
##store results
#############################
results		<- matrix(ncol=4, nrow=nrow(mydat))	#this is only meant to store the beta's, you dont need this other than see the progress

#############################
##run through the data
#############################

# estimate beta's with sgd: learnrate is 1/sqrt(n) (feel free to change the "(1/sqrt(i))" into something else)
# y = switch of wells
# x=intercept, dist, arsenic, dist*arsenic 	#these variables are used in the paper but there are more variables in the data you could try
# sgd logistic regression: beta = beta + learn rate * (y-(exp(beta*x)/(1+exp(beta*x))))*x

y	<- mydat$switch
x	<- cbind(1, mydat$dist100, mydat$arsenic, mydat$dist100*mydat$arsenic) # the 1 is for the intercept
	
for(i in 1:nrow(mydat))
{
	n		<- n+1
	results[i,] <- beta <- beta + (1/sqrt(n))*(y[i]-(exp(sum(beta*x[i,]))/(1+exp(sum(beta*x[i,])))))%*%x[i,]
}

