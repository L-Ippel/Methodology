#########################
## eta squared - ANOVA
#########################

#########################
##generate data
#########################

n_groups 	<- 3						# number of groups
size_group	<- 50						# sample size per group
averages	<- runif(n_groups, 0,10)		# average per group
std		<- 2						# error standard deviation
data		<- matrix(0, ncol=2, nrow=0)		# first we initialize the object data, which we will append the data to later

for(k in 1:n_groups)
{
	data	<- rbind(data,cbind(k,y=rnorm(size_group, averages[k],std))) # for each group we generate data given the group mean 
}

new.order	<- sample(n_groups*size_group)	# we do not want the data of a single group to arrive in a block so we randomly order the data
data		<- data[new.order,]

#########################
##set starting values
#########################

average 	<- 0
SSt	  	<- 0.00001 # to prevent that you'll divide by zero in the beginning of the stream
SSw		<- 0.00001
n		<- 0
group_parameters	<- data.frame(k=numeric(), count=numeric(),average=numeric())

#########################
##run through the data
#########################

for(i in 1:nrow(data))
{
	select_group	<- group_parameters[as.numeric(data[i,1]),]
	if(is.na(select_group[1]))
	{
		select_group<- c(data[i,1],0,0)	# add a new group when this is the first time that we observe this group
	}

	n		<- n+1
	select_group[2]<- select_group[2]+1

	d		<- data[i,2]-average
	dk		<- data[i,2]-select_group[3]

	average	<- average+(data[i,2]-average)/n
	select_group[3]	<- select_group[3]+(data[i,2]-select_group[3])/select_group[2]

	group_parameters[as.numeric(data[i,1]),]<-select_group
	SSt		<- SSt+ d*(data[i,2]-average)
	SSw		<- SSw+ dk*(data[i,2]-select_group[3])
 
	print(1-(SSw/SSt))
}