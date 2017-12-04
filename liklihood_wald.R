library(statmod)
library(boot)





j <- rep(1,500)
object<-basicglm(rep(1,500), set_500_2, link = 1, random = 6)


((rinvgauss(100, mean = 5, shape = 2))) -> j
k <- matrix(data = j, ncol = 10)
apply(k, MARGIN  = 1, basicglm, link = 1, xmat = rep(1,10), random = 6) ->esters





#The log liklihood of the inverse gaussian
loglikinvgaus <- function( data, par){
   mu <- par[1]
   lambda <- par[2]
  j <- -(sum(1/2*log(lambda) - 1/2*(log(2*pi))  -3/2*log(data)) - lambda/(2*mu^2)*sum((data-mu)^2/data))
  }



wald_lik <- function(data, goal){
  
  correct <- NULL
correct_2 <- NULL
intervals <- NULL
intervals_2 <- NULL
below_zero <- NULL
below_zero_2 <- NULL
se_2 <- NULL
lambda <- NULL
  
for(i in c(1:dim(data)[1])){
  dit <- data[i,]
#finding MLE optims
k <- optim(c(5,log(2)), loglikinvgaus, data = dit, hessian = T, method = 'L-BFGS-B', lower = c(-Inf, 0.01), upper = c(Inf, Inf))
vcov <- solve(k$hessian)


#confidence intervals
upper <- k$par[1] + 1.96*sqrt(vcov[1,1])
lower <- k$par[1] - 1.96*sqrt(vcov[1,1])
intervals[i] <- upper - lower
median_interval_mu <- median(intervals)
below_zero[i] <- lower
correct[i] <- 5 < upper & 5 > lower


upper_2 <- k$par[2] + 1.96*sqrt(vcov[2,2])
lower_2 <- k$par[2] - 1.96*sqrt(vcov[2,2])
intervals_2[i] <- upper_2 - lower_2
median_interval_lambda <- median(intervals_2)
below_zero_2[i] <- lower_2
se_2[i] <- sqrt(vcov[2,2])

lambda[i] <- k$par[2]

correct_2[i] <- goal < upper_2 & goal > lower_2

}
  holding <- NULL
helding <- NULL
  holding <- sum(correct)/(dim(data)[1])
  helding <- sum(correct_2)/(dim(data)[1])
  
  out_of_bounds_mu <- sum(below_zero <= 0)/(dim(data)[1])
  out_of_bounds_lambda <- sum(below_zero_2 <= 0)/(dim(data)[1])
  
  
  return(list(holding, median_interval_mu, out_of_bounds_mu, helding, median_interval_lambda, out_of_bounds_lambda))
}
















