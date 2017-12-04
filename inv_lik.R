i <- NULL
correct <- NULL
holding <- NULL

inv_lik <- function(data = data, goal){
  correct <- NULL
  pp <- NULL
  qq <- NULL
holding <- NULL
correct_2 <- NULL
helding<- NULL
below_zero <- NULL
below_zero_2 <- NULL
intervals <- NULL
intervals_2 <- NULL
  
n <- dim(data)[2]
for(i in c(1:dim(data)[1])){
  dit <- data[i,]
#finding MLE optims
k <- optim(c(5,12), loglikinvgaus, data = dit, method = 'L-BFGS-B', lower = c(0.1, 0.01), upper = c(Inf, Inf))

lambda <- k$par[2]
mu <- k$par[1]



#confidence intervals
upper <- n*lambda*mu/(max(n*lambda - sqrt(qchisq(.95, 1))*sqrt(n*lambda*mu), 0))
lower <- n*lambda*mu/(n*lambda + sqrt(qchisq(.95, 1))*sqrt(n*lambda*mu))



correct[i] <- 5 < upper & 5 > lower
intervals[i] <- upper - lower
below_zero[i] <- lower


upper_2 <- (lambda/n)*qchisq(.975,n-1)
lower_2 <- (lambda/n)*qchisq(.025,n-1)
correct_2[i] <- goal < upper_2 & goal > lower_2
below_zero_2[i] <- lower_2
intervals_2[i] <- upper_2 - lower_2


}

median_interval_lambda <- median(intervals_2)
median_interval_mu <- median(intervals)
  out_of_bounds_mu <- sum(below_zero <= 0)/(dim(data)[1])
  out_of_bounds_lambda <- sum(below_zero_2 <= 0)/(dim(data)[1])
  
holding <- sum(correct)/dim(data)[1]
helding <- sum(correct_2)/dim(data)[1]
return(list(holding, median_interval_mu, out_of_bounds_mu, helding, median_interval_lambda, out_of_bounds_lambda))
}

