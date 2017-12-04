


set_10_2 <- matrix(replicate(2000, rinvgauss(10, mean = 5, shape = 2)), nrow = 2000)
set_25_2 <- matrix(replicate(2000, expr = rinvgauss(25, mean = 5, shape = 2)), nrow = 2000)
set_50_2 <-matrix(replicate(2000, expr = rinvgauss(50, mean = 5, shape = 2)), nrow = 2000)
set_100_2 <- matrix(replicate(2000, expr = rinvgauss(100, mean = 5, shape = 2)), nrow = 2000)
set_500_2 <- matrix(replicate(2000, expr = rinvgauss(500, mean = 5, shape = 2)), nrow = 2000)


set_10_4 <- matrix(replicate(2000, rinvgauss(10, mean = 5, shape = 4)), nrow = 2000)
set_25_4 <- matrix(replicate(2000, expr = rinvgauss(25, mean = 5, shape = 4)), nrow = 2000)
set_50_4 <-matrix(replicate(2000, expr = rinvgauss(50, mean = 5, shape = 4)), nrow = 2000)
set_100_4 <- matrix(replicate(2000, expr = rinvgauss(100, mean = 5, shape = 4)), nrow = 2000)
set_500_4 <- matrix(replicate(2000, expr = rinvgauss(500, mean = 5, shape = 4)), nrow = 2000)


set_10_8 <- matrix(replicate(2000, rinvgauss(10, mean = 5, shape = 8)), nrow = 2000)
set_25_8 <- matrix(replicate(2000, expr = rinvgauss(25, mean = 5, shape = 8)), nrow = 2000)
set_50_8 <-matrix(replicate(2000, expr = rinvgauss(50, mean = 5, shape = 8)), nrow = 2000)
set_100_8 <- matrix(replicate(2000, expr = rinvgauss(100, mean = 5, shape = 8)), nrow = 2000)
set_500_8 <- matrix(replicate(2000, expr = rinvgauss(500, mean = 5, shape = 8)), nrow = 2000)




set_10_12 <- matrix(replicate(3000, rinvgauss(10, mean = 5, shape = 12)), nrow = 3000)
set_25_12 <- matrix(replicate(2000, expr = rinvgauss(25, mean = 5, shape = 12)), nrow = 2000)
set_50_12 <-matrix(replicate(2000, expr = rinvgauss(50, mean = 5, shape = 12)), nrow = 2000)
set_100_12 <- matrix(replicate(1000, expr = rinvgauss(100, mean = 5, shape = 12)), nrow = 1000)
set_500_12 <- matrix(replicate(2000, expr = rinvgauss(500, mean = 5, shape = 12)), nrow = 2000)


j <- c(set_10_2, set_25_2, set_50_2, set_100_2, set_500_2, set_10_4, set_25_4, set_50_4, set_100_4, set_500_4, set_10_8, set_25_8, set_50_8, set_100_8, set_500_8, set_10_12, set_25_12, set_50_12, set_100_12, set_500_12)


write.table(data_2, '/Users/vincentparis/520/Free_Market_Free_People/data/data_2.rda')
write.table(data_4, '/Users/vincentparis/520/Free_Market_Free_People/data/data_4.rda')
write.table(data_8, '/Users/vincentparis/520/Free_Market_Free_People/data/data_8.rda')
write.table(data_12, '/Users/vincentparis/520/Free_Market_Free_People/data/data_12.rda')
