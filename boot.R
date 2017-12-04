
#The log liklihood of the inverse gaussian
un_lambda <- function( data, par){
   lambda <- par[2]
   mu <- par[1]
  j <- -(sum(1/2*log(lambda) - 1/2*(log(2*pi))  -3/2*log(data)) - lambda/(2*mu^2)*sum((data-mu)^2/data))
  }


moments <- function(data){
  mu <- mean(data)
  lambda <- mean(data)^3/var(data)
  return(c(mu, lambda))
}




k <-function(data, indices) {return(optim(c(5,12), un_lambda, data=data[indices], hessian = T, method = 'L-BFGS-B', lower = c(0.01, 0.01), upper = c(Inf, Inf))$par) }
kk <- function(data, indices) {return(moments(data = data[indices]))}


goal <- 2
data <- set_10_2[1:2000,]
{
ranged_u <- NULL
ranged_lambda_u <- NULL
ranged_lambda_l <- NULL
stud_lam_correct <- NULL
ranged_lambda_correct <- NULL
momental_lambda_correct <- NULL
ranged_correct <- NULL
basic_momental_correct <- NULL
basic_lambda_correct <- NULL
basic_correct <- NULL
stud_correct <- NULL
momental <- NULL
ci_momental_lambda <- NULL
ci_momental <- NULL
basic_momental <- NULL
ci_lambda <- NULL
basic_lambda <- NULL
momental_lambda <- NULL
ranged_l <- NULL
ranged <- NULL
holding <- NULL
on_ice <- NULL
stud <- NULL
stud_lam_u <- NULL
stud_lam_l <- NULL
basic <- NULL
}
for(i in 1:(dim(data)[1])){
  dit <- data[i,]
  
  #All for mu
  #studentized and basic
on_ice <- boot(data = dit, statistic = k, 1000)
ci <- boot.ci(on_ice, type = c('basic', 'stud'))
stud <- rbind(stud, ci[[5]][c(4,5)])
stud_correct[i] <- 5 < stud[i,2] & 5 > stud[i,1]

basic <- rbind(basic, ci[[4]][c(4,5)])  
basic_correct[i] <- 5 < basic[i,2] & 5 > basic[i,1]

    

#ranged boot (4)           
  jack <- order(on_ice$t[,1])
mlesk <- optim(c(5, 12), un_lambda, data=dit, hessian = T, method = 'L-BFGS-B', lower = c(0.01, 0.01), upper = c(Inf, Inf))
mles <- mlesk$par
mle <- mles[1]
ordering <- (on_ice$t[,1])[jack] 
ranged_u[i] <-   mle^2/(ordering[floor(length(ordering)*.025)])
ranged_l[i] <- mle^2/(ordering[ceiling(length(ordering)*.975)])

ranged_correct[i] <- 5 < ranged_u[i] & 5 > ranged_l[i]




#moment boot
momental <- boot(data = dit, statistic = kk, 1000)
ci_momental <- boot.ci(momental, type = c('basic'))
basic_momental <- rbind(basic_momental, ci_momental[[4]][c(4,5)]) 
basic_momental_correct[i] <- 5 < basic_momental[i,2] & 5 > basic_momental[i,1]



#All for lambda
#basic
ci_lambda <- boot.ci(on_ice, type = c('basic'), index = 2)
basic_lambda <- rbind(basic_lambda, ci_lambda[[4]][c(4,5)]) 
basic_lambda_correct[i] <- goal < basic_lambda[i,2] & goal > basic_lambda[i,1]


#moment
ci_momental_lambda <- boot.ci(momental, type = c('basic'), index = 2)
momental_lambda <- rbind(momental_lambda, ci_momental_lambda[[4]][c(4,5)]) 
momental_lambda_correct[i] <- goal < momental_lambda[i,2] & goal > momental_lambda[i,1]

#ranged (4)
  jack <- order(on_ice$t[,2])
  lmlm <- mles[2]
ordering <- (on_ice$t[,2])[jack] 
ranged_lambda_u[i] <-   lmlm^2/(ordering[floor(length(ordering)*.025)])
ranged_lambda_l[i] <- lmlm^2/(ordering[ceiling(length(ordering)*.975)])
ranged_lambda_correct[i] <- goal < ranged_lambda_u[i] & goal > ranged_lambda_l[i]



#studentized
stud_lam_l[i] <- lmlm - sqrt(1/(2*lmlm^2)) * ((ordering[ceiling(length(ordering)*.975)]) - lmlm)/sqrt(1/(2*(ordering[ceiling(length(ordering)*.975)])^2))
stud_lam_u[i] <- lmlm - sqrt(1/(2*lmlm^2)) * ((ordering[ceiling(length(ordering)*.025)]) - lmlm)/sqrt(1/(2*(ordering[ceiling(length(ordering)*.025)])^2))
stud_lam_correct[i] <- goal < stud_lam_u[i] & goal > stud_lam_l[i]


}



ranged <- cbind(ranged_l, ranged_u)
stud_lam <- cbind(stud_lam_l, stud_lam_u)
ranged_lambda <- cbind(ranged_lambda_l, ranged_lambda_u)
boot_10_2_mu <- list(stud[1:2000,], stud_correct[1:2000], basic[1:2000,], basic_correct[1:2000], basic_momental[1:2000,], basic_momental_correct[1:2000], ranged[1:2000,], ranged_correct[1:2000])
names(boot_10_2_mu) <- c('stud', 'stud_c', 'basic', 'basic_c', 'b_momental', 'b_momental_c', 'range', 'range_c')



write.csv(boot_10_2_mu, 'boot_10_2_mu.csv')

boot_10_2_lambda <- list(stud_lam[1:2000,], stud_lam_correct[1:2000], basic_lambda[1:2000,], basic_lambda_correct[1:2000], momental_lambda[1:2000,], momental_lambda_correct[1:2000], ranged_lambda[1:2000,], ranged_lambda_correct[1:2000])
names(boot_10_2_lambda) <- c('stud', 'stud_c', 'basic', 'basic_c', 'b_momental', 'b_momental_c', 'range', 'range_c')
write.csv(boot_10_2_lambda, 'boot_10_2_lambda.csv')


sum(basic_momental_correct[1:2000])/2000
sum(ranged_lambda_correct[1:2000])/2000
sum(momental_lambda_correct[1:2000])/2000
sum(basic_lambda_correct[1:2000])/2000

sum(stud_correct[1:2000])/2000
sum(basic_correct[1:2000])/2000
sum(ranged_correct[1:2000])/2000
sum(basic_momental_correct[1:2000])/2000

f(boot_500_4_mu[[1]])

