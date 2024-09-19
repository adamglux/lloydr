##### datasets

## error datasets

notDF <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))

## generated data

set.seed(31415926)
x <- c(rnorm(33,0,1), rnorm(33,2,3), rnorm(34, 10, 4))
y <- c(rnorm(33,0,1), rnorm(33,2,3), rnorm(34, 10, 4))
df1 <- as.data.frame(cbind(x,y))

## data3 -- big(ish) dataset
x <- c(rnorm(100,0,1), rnorm(100,2,3), rnorm(100, 10, 4))
y <- c(rnorm(100,0,1), rnorm(100,2,3), rnorm(100, 10, 4))
df3 <- as.data.frame(cbind(x,y))



#calculate clusters
#clusty_k <- kmeans(df1, 3, algorithm = "Lloyd") #kmeans
#clusty <- calculate_clusters(df1, 3, "euclidean", scale = FALSE) #lloydr

#plot kmeans
#plot(df1, col = clusty_k$cluster)
#points(clusty_k$centers, col = 1:2, pch = 8, cex = 2)

#plot lloydr
#plot(df1, col = clusty$cluster)
#points(clusty$centers, col = 1:2, pch = 8, cex = 2)

## invalid inputs
test_that("the calculate clusters function has the right input validation",
          {
            expect_error(calculate_clusters(notDF, 3, "euclidean"),
                         "Input must be a data frame.")

            expect_error(calculate_clusters(df1, "3", "euclidean"),
                         "k must be a positive integer.")

            expect_error(calculate_clusters(df1, -4, "euclidean"),
                         "k must be a positive integer.")

            expect_error(calculate_clusters(df1, 0.5, "euclidean"),
                         "k must be a positive integer.")

            expect_error(calculate_clusters(df1, 2, "batman"),
                         "Distance must be one of 'euclidean', 'manhattan', 'cosine', 'gower'.")

            expect_error(calculate_clusters(df1, 2, "gower", max.iter = -10),
                         "max.iter must be a positive integer.")

            expect_message(calculate_clusters(df3, 4, "euclidean", max.iter = 3))

            expect_error(calculate_clusters(df1, 2, "gower", max.iter = 100, tol = -1),
                         "Tolerance must be a positive number.")

            expect_error(calculate_clusters(df1, 2, "gower", scale = 4),
                         "Scale paramter must be TRUE to scale data or FALSE to skip scalling")

          })

test_that("the compare clusters function has the right input validation",
          {
            expect_error(compare_clusters(notDF, 3),
                         "Input must be a data frame.")

            expect_error(compare_clusters(df1, "3"),
                         "k must be a positive integer.")

            expect_error(compare_clusters(df1, -4),
                         "k must be a positive integer.")

            expect_error(compare_clusters(df1, 0.5),
                         "k must be a positive integer.")

            expect_error(compare_clusters(df1, 2, max.iter = -10),
                         "max.iter must be a positive integer.")

            expect_message(compare_clusters(df3, 4, max.iter = 3))

            expect_error(compare_clusters(df1, 2, max.iter = 100, tol = -1),
                         "Tolerance must be a positive number.")

            expect_error(compare_clusters(df1, 2, scale = 4),
                         "Scale paramter must be TRUE to scale data or FALSE to skip scalling")

          })
