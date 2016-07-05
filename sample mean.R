####################
## Sample mean
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
online_mean <- 0
n		<- 0

###################
##run through the data:
###################

for(i in 1:length(data))
{
	n		<- n+1
	online_mean	<- online_mean + (data[i]-online_mean)/n
	print(online_mean)
	
}

