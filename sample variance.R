####################
## Sample variance
####################

####################
##simulate data
####################
average	<- 10
std		<- 5
N		<- 1000
data		<- rnorm(N, average, std )

###################
##set starting values
###################
online_mean <- data[1]
online_SS	<- 0
n		<- 1

###################
##run through the data:
###################

for(i in 2:length(data))
{
	n		<- n+1
	d		<- data[i]-online_mean
	online_mean	<- online_mean + (data[i]-online_mean)/n
	online_SS	<- online_SS + d*(data[i]-online_mean)
	online_var	<- online_SS/(n-1) 
	print(online_var)
}



