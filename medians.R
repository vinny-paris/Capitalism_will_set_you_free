#mu
c('stud_c', 'basic_c','b_momental_c', 'range_c')
x <- boot_10_12_mu

medians <- function(data, goal, mu = 0, n, x){
med_stud <- median(x[['stud']][,2] - x[['stud']][,1])
med_stud

med_basic <- median(x[['basic']][,2] - x[['basic']][,1])
med_basic

med_momental <- median(x[['b_momental']][,2] - x[['b_momental']][,1])
med_momental

med_range <- median(x[['range']][,2] - x[['range']][,1])
med_range

wald <- wald_lik(data, goal)
med_wald <- wald[[2 + mu]]

lik <- inv_lik(data, goal)
med_lik <- lik[[2 + mu]]



holding <- c(n, goal, med_stud, med_basic, med_momental, med_range, med_wald, med_lik)
names(holding) <- c('n', 'lambda', 'stud', 'basic', 'momental', 'range', 'wald', 'lik')
return(holding)
}
med_10_2 <- medians(set_10_2[1:2000,], 2, mu = 0, 10, boot_10_2_mu)
med_25_2 <- medians(set_25_2, 2, mu = 0, 25, boot_25_2_mu)
med_50_2 <- medians(set_50_2, 2, mu = 0, 50, boot_50_2_mu)
med_100_2 <- medians(set_100_2, 2, mu = 0, 100, boot_100_2_mu)
med_500_2 <- medians(set_500_2, 2, mu = 0, 500, boot_500_2_mu)

med_10_4 <- medians(set_10_4, 4, mu = 0, 10, boot_10_4_mu)
med_25_4 <- medians(set_25_4, 4, mu = 0, 25, boot_25_4_mu)
med_50_4 <- medians(set_50_4, 4, mu = 0, 50, boot_50_4_mu)
med_100_4 <- medians(set_100_4, 4, mu = 0, 100, boot_100_4_mu)
med_500_4 <- medians(set_500_4, 4, mu = 0, 500, boot_500_4_mu)


med_10_8 <- medians(set_10_8, 8, mu = 0, 10, boot_10_8_mu)
med_25_8 <- medians(set_25_8, 8, mu = 0, 25, boot_25_8_mu)
med_50_8 <- medians(set_50_8, 8, mu = 0, 50, boot_50_8_mu)
med_100_8 <- medians(set_100_8, 8, mu = 0, 100, boot_100_8_mu)
med_500_8 <- medians(set_500_8, 8, mu = 0, 500, boot_500_8_mu)


med_10_12 <- medians(set_10_12, 12, mu = 0, 10, boot_10_12_mu)
med_25_12 <- medians(set_25_12, 12, mu = 0, 25, boot_25_12_mu)
med_50_12 <- medians(set_50_12, 12, mu = 0, 50, boot_50_12_mu)
med_100_12 <- medians(set_100_12, 12, mu = 0, 100, boot_100_12_mu)
med_500_12 <- medians(set_500_12, 12, mu = 0, 500, boot_500_12_mu)


med_dataframe <- round(rbind(med_10_2, med_25_2, med_50_2, med_100_2, med_500_2,
                       med_10_4, med_25_4, med_50_4, med_100_4, med_500_4,
med_10_8, med_25_8, med_50_8, med_100_8, med_500_8,
                       med_10_12, med_25_12, med_50_12, med_100_12, med_500_12), digits = 4)

write.csv(med_dataframe, 'median_dataframe_mu.csv')

library(reshape2)
j <- melt(med_dataframe[,3:8])
row.names(j) <- NULL
n <- c(kronecker(rep(1,24), c(10,25,50,100,500)))
lambda <- c(kronecker(rep(1,6), kronecker(c(2,4,8,12), rep(1,5))))
med <- cbind(j,as.factor(n),as.factor(lambda))
names(med) <- c('hold','method', 'width', 'n', 'lambda')

library(ggplot2)

ggplot(med, aes(n, width)) + geom_point(aes(col = lambda)) + facet_wrap(~method) + ylab('C.I. Median Width for Mu')
ggplot(med[-101,], aes(n, width)) + geom_point(aes(col = lambda)) + facet_wrap(~method)+ ylab('C.I. Median Width for Mu')












med_10_2 <- medians(set_10_2[1:2000,], 2, mu = 3, 10, boot_10_2_lambda)
med_25_2 <- medians(set_25_2, 2, mu = 3, 25, boot_25_2_lambda)
med_50_2 <- medians(set_50_2, 2, mu = 3, 50, boot_50_2_lambda)
med_100_2 <- medians(set_100_2, 2, mu = 3, 100, boot_100_2_lambda)
med_500_2 <- medians(set_500_2, 2, mu = 03, 500, boot_500_2_lambda)

med_10_4 <- medians(set_10_4, 4, mu = 03, 10, boot_10_4_lambda)
med_25_4 <- medians(set_25_4, 4, mu = 03, 25, boot_25_4_lambda)
med_50_4 <- medians(set_50_4, 4, mu = 03, 50, boot_50_4_lambda)
med_100_4 <- medians(set_100_4, 4, mu = 03, 100, boot_100_4_lambda)
med_500_4 <- medians(set_500_4, 4, mu = 03, 500, boot_500_4_lambda)


med_10_8 <- medians(set_10_8, 8, mu = 03, 10, boot_10_8_lambda)
med_25_8 <- medians(set_25_8, 8, mu = 03, 25, boot_25_8_lambda)
med_50_8 <- medians(set_50_8, 8, mu = 03, 50, boot_50_8_lambda)
med_100_8 <- medians(set_100_8, 8, mu = 03, 100, boot_100_8_lambda)
med_500_8 <- medians(set_500_8, 8, mu = 03, 500, boot_500_8_lambda)


med_10_12 <- medians(set_10_12, 12, mu = 03, 10, boot_10_12_lambda)
med_25_12 <- medians(set_25_12, 12, mu = 03, 25, boot_25_12_lambda)
med_50_12 <- medians(set_50_12, 12, mu = 03, 50, boot_50_12_lambda)
med_100_12 <- medians(set_100_12, 12, mu = 03, 100, boot_100_12_lambda)
med_500_12 <- medians(set_500_12, 12, mu = 03, 500, boot_500_12_lambda)


med_dataframe <- round(rbind(med_10_2, med_25_2, med_50_2, med_100_2, med_500_2,
                       med_10_4, med_25_4, med_50_4, med_100_4, med_500_4,
med_10_8, med_25_8, med_50_8, med_100_8, med_500_8,
                       med_10_12, med_25_12, med_50_12, med_100_12, med_500_12), digits = 4)




write.csv(med_dataframe, 'median_dataframe_lambda.csv')
j <- melt(med_dataframe[,3:8])
row.names(j) <- NULL
n <- c(kronecker(rep(1,24), c(10,25,50,100,500)))
lambda <- c(kronecker(rep(1,6), kronecker(c(2,4,8,12), rep(1,5))))
med <- cbind(j,as.factor(n),as.factor(lambda))
names(med) <- c('holding', 'method', 'width', 'n', 'lambda')


ggplot(med, aes(n, width)) + geom_point(aes(col = lambda)) + facet_wrap(~method) + ylab('C.I. Median Width for Lambda')




