sum(boot_500_2_lambda[[2]])/2000
sum(boot_50_2_lambda[[1]]<0)

param_failure <- function(data, goal, mu = 0, n, x){
  param_stud <- sum(x[[1]] < 0) /3000
  param_basic <- sum(x[[3]] < 0) /3000
  param_momental <- sum(x[[5]] < 0) /3000
  param_range <- sum(x[[7]] < 0) /3000
  
  wald <- wald_lik(data, goal)
param_wald <- wald[[3 + mu]]

lik <- inv_lik(data, goal)
param_lik <- lik[[3 + mu]]

k <- data.frame(n, goal, param_stud, param_basic, param_momental, param_range, param_wald, param_lik)
names(k) <- c('n', 'lambda', 'stud', 'basic', 'momental', 'range', 'wald', 'lik')
return(k)
}




par_10_2 <- param_failure(data = set_10_2[1:2000,], 2, mu = 0, 10, boot_10_2_mu)
par_25_2 <- param_failure(set_25_2, 2, mu = 0, 25, boot_25_2_mu)
par_50_2 <- param_failure(set_50_2, 2, mu = 0, 50, boot_50_2_mu)
par_100_2 <- param_failure(set_100_2, 2, mu = 0, 100, boot_100_2_mu)
par_500_2 <- param_failure(set_500_2, 2, mu = 0, 500, boot_500_2_mu)


par_10_4 <- param_failure(set_10_4, 4, mu = 0, 10, boot_10_4_mu)
par_25_4 <- param_failure(set_25_4, 4, mu = 0, 25, boot_25_4_mu)
par_50_4 <- param_failure(set_50_4, 4, mu = 0, 50, boot_50_4_mu)
par_100_4 <- param_failure(set_100_4, 4, mu = 0, 100, boot_100_4_mu)
par_500_4 <- param_failure(set_500_4, 4, mu = 0, 500, boot_500_4_mu)



par_10_8 <- param_failure(set_10_8, 8, mu = 0, 10, boot_10_8_mu)
par_25_8 <- param_failure(set_25_8, 8, mu = 0, 25, boot_25_8_mu)
par_50_8 <- param_failure(set_50_8, 8, mu = 0, 50, boot_50_8_mu)
par_100_8 <- param_failure(set_100_8, 8, mu = 0, 100, boot_100_8_mu)
par_500_8 <- param_failure(set_500_8, 8, mu = 0, 500, boot_500_8_mu)


par_10_12 <- param_failure(set_10_12, 12, mu = 0, 10, boot_10_12_mu)
par_25_12 <- param_failure(set_25_12, 12, mu = 0, 25, boot_25_12_mu)
par_50_12 <- param_failure(set_50_12, 12, mu = 0, 50, boot_50_12_mu)
par_100_12 <- param_failure(set_100_12, 12, mu = 0, 100, boot_100_12_mu)
par_500_12 <- param_failure(set_500_12, 12, mu = 0, 500, boot_500_12_mu)


par_dataframe <- round(rbind(par_10_2, par_25_2, par_50_2, par_100_2, par_500_2,
                       par_10_4, par_25_4, par_50_4, par_100_4, par_500_4,
                       par_10_8, par_25_8, par_50_8, par_100_8, par_500_8,
                       par_10_12, par_25_12, par_50_12, par_100_12, par_500_12), digits = 4)

j <- melt(par_dataframe[,3:8])
row.names(j) <- NULL
n <- c(kronecker(rep(1,24), c(10,25,50,100,500)))
lambda <- c(kronecker(rep(1,6), kronecker(c(2,4,8,12), rep(1,5))))
par_mu <- cbind(j, as.factor(n),as.factor(lambda))
names(par_mu) <- c('method', 'bounds', 'n', 'lambda')

write.csv(par_mu, 'par_mu.csv')


ggplot(par_mu, aes(n, bounds)) + geom_point(aes(col = lambda)) + facet_wrap(~method)  
ggplot(subset(par_mu, bounds >0), aes(n, bounds)) + geom_point(aes(col = lambda)) + facet_wrap(~method) +
  ylab("Proportion of Mu's Out of Bound")


par_mu_reduced <- subset(par_mu, bounds>0)
par_mu_reduced





par_10_2 <- param_failure(data = set_10_2[1:2000,], 2, mu = 3, 10, boot_10_2_mu)
par_25_2 <- param_failure(set_25_2, 2, mu = 3, 25, boot_25_2_mu)
par_50_2 <- param_failure(set_50_2, 2, mu = 3, 50, boot_50_2_mu)
par_100_2 <- param_failure(set_100_2, 2, mu = 3, 100, boot_100_2_mu)
par_500_2 <- param_failure(set_500_2, 2, mu = 3, 500, boot_500_2_mu)


par_10_4 <- param_failure(set_10_4, 4, mu = 3, 10, boot_10_4_mu)
par_25_4 <- param_failure(set_25_4, 4, mu = 3, 25, boot_25_4_mu)
par_50_4 <- param_failure(set_50_4, 4, mu = 3, 50, boot_50_4_mu)
par_100_4 <- param_failure(set_100_4, 4, mu = 3, 100, boot_100_4_mu)
par_500_4 <- param_failure(set_500_4, 4, mu = 3, 500, boot_500_4_mu)



par_10_8 <- param_failure(set_10_8, 8, mu = 3, 10, boot_10_8_mu)
par_25_8 <- param_failure(set_25_8, 8, mu = 3, 25, boot_25_8_mu)
par_50_8 <- param_failure(set_50_8, 8, mu = 3, 50, boot_50_8_mu)
par_100_8 <- param_failure(set_100_8, 8, mu = 3, 100, boot_100_8_mu)
par_500_8 <- param_failure(set_500_8, 8, mu = 3, 500, boot_500_8_mu)


par_10_12 <- param_failure(set_10_12, 12, mu = 3, 10, boot_10_12_mu)
par_25_12 <- param_failure(set_25_12, 12, mu = 3, 25, boot_25_12_mu)
par_50_12 <- param_failure(set_50_12, 12, mu = 3, 50, boot_50_12_mu)
par_100_12 <- param_failure(set_100_12, 12, mu = 3, 100, boot_100_12_mu)
par_500_12 <- param_failure(set_500_12, 12, mu = 3, 500, boot_500_12_mu)


par_dataframe <- round(rbind(par_10_2, par_25_2, par_50_2, par_100_2, par_500_2,
                       par_10_4, par_25_4, par_50_4, par_100_4, par_500_4,
                       par_10_8, par_25_8, par_50_8, par_100_8, par_500_8,
                       par_10_12, par_25_12, par_50_12, par_100_12, par_500_12), digits = 4)

j <- melt(par_dataframe[,3:8])
row.names(j) <- NULL
n <- c(kronecker(rep(1,24), c(10,25,50,100,500)))
lambda <- c(kronecker(rep(1,6), kronecker(c(2,4,8,12), rep(1,5))))
par_lambda <- cbind(j, as.factor(n),as.factor(lambda))
names(par_lambda) <- c('method', 'bounds', 'n', 'lambda')

write.csv(par_lambda, 'par_lambda.csv')
par_lambda
ggplot(par_lambda, aes(n, bounds)) + geom_point(aes(col = lambda)) + facet_wrap(~method)  + ylab("Proportion of Lambda's Out of Bounds")
ggplot(subset(par_lambda, bounds >0), aes(n, bounds)) + geom_point(aes(col = lambda)) + facet_wrap(~method) +
  ylab("Proportion of Lambda's Out of Bound")

