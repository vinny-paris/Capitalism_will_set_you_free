sparam_failure <- function(data, goal, mu = 0, n, x){
  param_stud <- sum(x[[2]] ) /2000
  param_basic <- sum(x[[4]] ) /2000
  param_momental <- sum(x[[6]] ) /2000
  param_range <- sum(x[[8]] ) /2000
  
  wald <- wald_lik(data, goal)
param_wald <- wald[[1 + mu]]

lik <- inv_lik(data, goal)
param_lik <- lik[[1 + mu]]

k <- data.frame(n, goal, param_stud, param_basic, param_momental, param_range, param_wald, param_lik)
names(k) <- c('n', 'lambda', 'stud', 'basic', 'momental', 'range', 'wald', 'lik')
return(k)
}



par_10_2 <- sparam_failure(data = set_10_2[1:2000,], 2, mu = 0, 10, boot_10_2_mu)
par_25_2 <- sparam_failure(set_25_2, 2, mu = 0, 25, boot_25_2_mu)
par_50_2 <- sparam_failure(set_50_2, 2, mu = 0, 50, boot_50_2_mu)
par_100_2 <- sparam_failure(set_100_2, 2, mu = 0, 100, boot_100_2_mu)
par_500_2 <- sparam_failure(set_500_2, 2, mu = 0, 500, boot_500_2_mu)


par_10_4 <- sparam_failure(set_10_4, 4, mu = 0, 10, boot_10_4_mu)
par_25_4 <- sparam_failure(set_25_4, 4, mu = 0, 25, boot_25_4_mu)
par_50_4 <- sparam_failure(set_50_4, 4, mu = 0, 50, boot_50_4_mu)
par_100_4 <- sparam_failure(set_100_4, 4, mu = 0, 100, boot_100_4_mu)
par_500_4 <- sparam_failure(set_500_4, 4, mu = 0, 500, boot_500_4_mu)



par_10_8 <- sparam_failure(set_10_8, 8, mu = 0, 10, boot_10_8_mu)
par_25_8 <- sparam_failure(set_25_8, 8, mu = 0, 25, boot_25_8_mu)
par_50_8 <- sparam_failure(set_50_8, 8, mu = 0, 50, boot_50_8_mu)
par_100_8 <- sparam_failure(set_100_8, 8, mu = 0, 100, boot_100_8_mu)
par_500_8 <- sparam_failure(set_500_8, 8, mu = 0, 500, boot_500_8_mu)


par_10_12 <- sparam_failure(set_10_12[1:2000,], 12, mu = 0, 10, boot_10_12_mu)
par_25_12 <- sparam_failure(set_25_12, 12, mu = 0, 25, boot_25_12_mu)
par_50_12 <- sparam_failure(set_50_12, 12, mu = 0, 50, boot_50_12_mu)
par_100_12 <- sparam_failure(set_100_12, 12, mu = 0, 100, boot_100_12_mu)
par_500_12 <- sparam_failure(set_500_12, 12, mu = 0, 500, boot_500_12_mu)


par_dataframe <- round(rbind(par_10_2, par_25_2, par_50_2, par_100_2, par_500_2,
                       par_10_4, par_25_4, par_50_4, par_100_4, par_500_4,
                       par_10_8, par_25_8, par_50_8, par_100_8, par_500_8,
                       par_10_12, par_25_12, par_50_12, par_100_12, par_500_12), digits = 4)

j <- melt(par_dataframe[,3:8])
row.names(j) <- NULL
n <- c(kronecker(rep(1,24), c(10,25,50,100,500)))
lambda <- c(kronecker(rep(1,6), kronecker(c(2,4,8,12), rep(1,5))))
rate_mu <- cbind(j, as.factor(n),as.factor(lambda))
names(rate_mu) <- c('method', 'rate', 'n', 'lambda')
rate_mu
ggplot(rate_mu, aes(n, rate)) + geom_point(aes(color=lambda)) + facet_wrap(~method) + ylab('Observed Coverage Rate for Lambda')

ggplot(subset(rate_mu,rate>.3), aes(n, rate)) + geom_point(aes(color=lambda)) + facet_wrap(~method) + ylab('Observed Coverage Rate for Lambda')


write.csv(rate_mu, 'rate_mu.csv')
















par_10_2 <- sparam_failure(data = set_10_2[1:2000,], 2, mu = 3, 10, boot_10_2_lambda)
par_25_2 <- sparam_failure(set_25_2, 2, mu = 3, 25, boot_25_2_lambda)
par_50_2 <- sparam_failure(set_50_2, 2, mu = 3, 50, boot_50_2_lambda)
par_100_2 <- sparam_failure(set_100_2, 2, mu = 3, 100, boot_100_2_lambda)
par_500_2 <- sparam_failure(set_500_2, 2, mu = 3, 500, boot_500_2_lambda)


par_10_4 <- sparam_failure(set_10_4, 4, mu = 3, 10, boot_10_4_lambda)
par_25_4 <- sparam_failure(set_25_4, 4, mu = 3, 25, boot_25_4_lambda)
par_50_4 <- sparam_failure(set_50_4, 4, mu = 3, 50, boot_50_4_lambda)
par_100_4 <- sparam_failure(set_100_4, 4, mu = 3, 100, boot_100_4_lambda)
par_500_4 <- sparam_failure(set_500_4, 4, mu = 3, 500, boot_500_4_lambda)



par_10_8 <- sparam_failure(set_10_8, 8, mu = 3, 10, boot_10_8_lambda)
par_25_8 <- sparam_failure(set_25_8, 8, mu = 3, 25, boot_25_8_lambda)
par_50_8 <- sparam_failure(set_50_8, 8, mu = 3, 50, boot_50_8_lambda)
par_100_8 <- sparam_failure(set_100_8, 8, mu = 3, 100, boot_100_8_lambda)
par_500_8 <- sparam_failure(set_500_8, 8, mu = 3, 500, boot_500_8_lambda)


par_10_12 <- sparam_failure(set_10_12, 12, mu = 3, 10, boot_10_12_lambda)
par_25_12 <- sparam_failure(set_25_12, 12, mu = 3, 25, boot_25_12_lambda)
par_50_12 <- sparam_failure(set_50_12, 12, mu = 3, 50, boot_50_12_lambda)
par_100_12 <- sparam_failure(set_100_12, 12, mu = 3, 100, boot_100_12_lambda)
par_500_12 <- sparam_failure(set_500_12, 12, mu = 3, 500, boot_500_12_lambda)


par_dataframe <- round(rbind(par_10_2, par_25_2, par_50_2, par_100_2, par_500_2,
                       par_10_4, par_25_4, par_50_4, par_100_4, par_500_4,
                       par_10_8, par_25_8, par_50_8, par_100_8, par_500_8,
                       par_10_12, par_25_12, par_50_12, par_100_12, par_500_12), digits = 4)

j <- melt(par_dataframe[,3:8])
row.names(j) <- NULL
n <- c(kronecker(rep(1,24), c(10,25,50,100,500)))
lambda <- c(kronecker(rep(1,6), kronecker(c(2,4,8,12), rep(1,5))))
rate_lambda <- cbind(j, as.factor(n),as.factor(lambda))
names(rate_lambda) <- c('method', 'rate', 'n', 'lambda')
ggplot(rate_lambda, aes(n, rate)) + geom_point(aes(color=lambda)) + facet_wrap(~method) + ylab('Observed Coverage Rate for Lambda')

ggplot(subset(rate_lambda, rate>.3), aes(n, rate)) + geom_point(aes(color=lambda)) + facet_wrap(~method) + ylab('Observed Coverage Rate for Lambda')


