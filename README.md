# Methodology
In this repository we uploaded the R code belonging to the examples discussed in:  

"Dealing with Data streams: an Online, Row-by-Row, Estimation Tutorial" (Methodology). 

The files that include a statistic as file name are small working examples of the statistic the file is named after. The code allows alterations of for example the sample sizes, means, standard deviations, number of variables, or number of groups depending on the statistic.

The file named "Streaming analysis" includes the functions which can be used to estimate the statistics online. The first argument of the functions is always the new data point (and in case of a statistic that requires more variables, then the first two arguments are the new input). The next argument is theta. Theta is a list of the current state of the sufficient statistics. We implemented the functions in such a way that if theta is missing, or when theta is an empty object (NULL), the function creates a list including the required sufficient statistics.  The output of the functions is theta, which is a list of updated sufficient statistics including the parameter of interest. So in a for-loop, one could use the functions as follows:

res   <- NULL 

data  <-rnorm(1000,mean=5,sd=2)

for(i in 1:nrow(data))

{

      res <-mean_online(input=data[i], theta=res)

}

Below we list all the functions in Streaming analysis.R file including the required arguments, 
note that argument theta always has default values, such that you dont have to specify theta yourself. 
The parameter of interest is included in theta in the function,   

- mean_online(input, theta=list("n"=0,"Mean"=0))

- SS_online(input, theta=list("n"=0,"Mean"=input,"SS"=0))

- var_online(input, theta=list("n"=1,"Mean"=input,"SS"=0, "var"=NA))

- sd_online(input, theta=list("n"=1,"Mean"=input,"SS"=0, "sd"=NA))

- SSxy_online(input_x, input_y, theta=list("n"=0, "Mean"=c("x"=input_x,"y"=input_y),"SSxy"=0))

- cov_online(input_x, input_y, theta=list("n"=1,"Mean"=c("x"=input_x, "y"=input_y),"SSxy"=0,"SSx"=0,"SSy"=0, "cov"=NA))

- cor_online(input_x, input_y, theta=list("n"=1,"Mean"=c("x"=input_x, "y"=input_y),"SSxy"=0,"SSx"=0,"SSy"=0, "cov"=NA, "cor"=NA))

- lm_online(input_x, input_y, theta=list("n"=0, "xy"=rep(0,length(input_x)), "x_sq"=matrix(0, nrow=length(input_x),                                                         ncol=length(input_y)), "x_inv"=NULL, "beta"=rep(NA,input_x+1)))
      - this function is supported by: inv_matrix_online(input, theta), where theta is an inverted matrix   

- etasq_online(input, input_group theta=list("group_id"=c(input_group),"n_groups"=1, "SS_w"=0.00001, "SS_t"=0.00001, "mean"=input,                                            "n"=0,"n_k"=c(0), "mean_k"=c(input)))

- sgd_log(input_x, input_y, theta=list("n"=0, "beta"=rep(0,(1+length(input_x)))))


