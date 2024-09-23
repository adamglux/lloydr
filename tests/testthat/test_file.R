##### datasets

## testing datasets

notDF <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
notK <- c(2,3)
## generated data

set.seed(314)
x <- c(rnorm(33,0,1), rnorm(33,2,3), rnorm(34, 10, 4))
y <- c(rnorm(33,0,1), rnorm(33,2,3), rnorm(34, 10, 4))
df1 <- as.data.frame(cbind(x,y))

## data3 --
x <- c(rnorm(100,0,1), rnorm(100,2,3), rnorm(100, 10, 4))
y <- c(rnorm(100,0,1), rnorm(100,2,3), rnorm(100, 10, 4))
df3 <- as.data.frame(cbind(x,y))

## data4
df4 <- data.frame(x = c(1,2), y = c(1,2))   #2 points


##############################
########## Tests For Main Functions
##############################

########## input validation
## calculate clusters
test_that("the calculate clusters function has the right input validation",
          {
            expect_error(calculate_clusters(notDF, 3, "euclidean"),
                         "Input must be a data frame.")

            expect_error(calculate_clusters(df1, notK, "euclidean"))

            expect_error(calculate_clusters(df1, "3", "euclidean"),
                         "k must be a positive integer and > 1.")

            expect_error(calculate_clusters(df1, -4, "euclidean"),
                         "k must be a positive integer and > 1.")

            expect_error(calculate_clusters(df1, 0.5, "euclidean"),
                         "k must be a positive integer and > 1.")

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

## compare clusters
test_that("the compare clusters function has the right input validation",
          {
            expect_error(compare_clusters(notDF, 3),
                         "Input must be a data frame.")

            expect_error(compare_clusters(df1, "3"),
                         "k must be a positive integer, greater than 1.")

            expect_error(compare_clusters(df1, -4),
                         "k must be a positive integer, greater than 1.")

            expect_error(compare_clusters(df1, 0.5),
                         "k must be a positive integer, greater than 1.")

            expect_error(compare_clusters(df1, c(1,3,5)),
                         "k must be a positive integer, greater than 1.")

            expect_error(compare_clusters(df1, 2, max.iter = -10),
                         "max.iter must be a positive integer.")

            #expect_message(compare_clusters(df3, 4, max.iter = 3))

            expect_error(compare_clusters(df1, 2, max.iter = 100, tol = -1),
                         "Tolerance must be a positive number.")

            expect_error(compare_clusters(df1, 2, scale = 4),
                         "Scale paramter must be TRUE to scale data or FALSE to skip scalling")

          })

# test_that("compare_clusters handles empty dataframes", {
#   data <- data.frame()  # Empty dataframe
#   expect_error(compare_clusters(data, k = 2:3), "Input must be a data frame.")
# })

########## test edge cases (for few datapoints or identical points)
test_that("calculate clusters handles fewer points than clusters", {
  expect_error(calculate_clusters(df4, 3, "euclidean"),
               "Number of clusters cannot exceed number of data points")
})

# test_that("compare clusters handles fewer points than clusters", {
#   expect_error(compare_clusters(df4, 3, "euclidean"),
#                "Number of clusters cannot exceed number of data points")
# })


########## consistency tests
test_that("compare_clusters handles multiple k values", {
  result <- compare_clusters(df1, k = 2:3, max.iter = 10)
  # Check that the output contains the correct number of rows
  expect_equal(nrow(result), 8)  # 4 distances * 2 k values
  # Check that result includes the correct distance metrics
  expect_true(all(result$distance %in% c('euclidean', 'manhattan', 'cosine', 'gower')))
  # Check that all Dunn indices are numeric
  expect_true(is.numeric(result$DUNN))
})

########## compare to kmeans()
test_that("calculate clusters returns the same centroids as kmeans", {
  set.seed(122)
  clusty_k <- kmeans(df1, 3, algorithm = "Lloyd") #kmeans
  set.seed(122)
  clusty <- calculate_clusters(df1, 3, "euclidean", scale = FALSE) #lloydr
  result1 <- ceiling(sort(clusty$centers))
  result2 <- ceiling(sort(clusty_k$centers))
  expect_equal(result1, result2)
})


##############################
########## Tests For Helper Functions
##############################

########## algorithm integrity tests
test_that("Lloyd's algorithm initializes centroids and updates correctly", {
  # Create a simple dataset
  data <- matrix(c(1, 1, 5, 5, 9, 9), ncol = 2, byrow = TRUE)
  k <- 2
  result <- .lloyds_algorithm(data, k, "euclidean")

  # Check if correct number of clusters are created
  expect_equal(length(unique(result$cluster_assignments)), k)

  # Check centroids are correctly updated during iterations
  expect_true(all(result$centroids != 0))  # Centroids shouldn't remain zero
})

########## convergence tests
test_that("Lloyd's algorithm converges correctly", {
  data <- matrix(rnorm(100), ncol = 5)
  k <- 3
  result <- .lloyds_algorithm(data, k, "euclidean", max_iters = 10)

  # Check that iterations do not exceed max_iters
  expect_lte(result$iterations, 10)

  # Check if the convergence is met (if iteration < max_iters)
  expect_true(result$iterations < 10 || result$iterations == 10)
})

########## edge case tests
test_that("Lloyd's algorithm handles identical data points", {
  data <- matrix(rep(1, 20), ncol = 5)  # Identical points
  k <- 2
  result <- .lloyds_algorithm(data, k, "euclidean")
  # Expect that centroids are all the same since data points are identical
  expect_true(all(result$centroids[1, ] == result$centroids[2, ]))
})

########## scalability tests
test_that("Lloyd's algorithm handles small datasets", {
  data <- matrix(c(1, 2, 3, 4), ncol = 2)  # Small dataset
  k <- 2

  result <- .lloyds_algorithm(data, k, "euclidean")

  expect_equal(length(result$cluster_assignments), nrow(data))
})

test_that("Lloyd's algorithm handles large datasets", {
  data <- matrix(rnorm(10000), ncol = 10)  # Large dataset
  k <- 5

  result <- .lloyds_algorithm(data, k, "euclidean", max_iters = 50)

  expect_equal(length(result$cluster_assignments), nrow(data))
})


########## correctness tests
test_that("Lloyd's algorithm produces correct output for example dataset", {
  data <- matrix(c(1, 1, 10, 10, 20, 20), ncol = 2, byrow = T)  # Well-separated data points
  k <- 3
  result <- .lloyds_algorithm(data, k, "euclidean")
  # Expect that each point is its own cluster in this simple case
  #expect_equal(result$cluster_assignments, c(1, 2, 3) %in% result$cluster_assignments)
  expect_equal(length(unique(result$cluster_assignments)), length(result$cluster_assignments))
})

########## consistency tests
test_that("Lloyd's algorithm is consistent with fixed random seed", {
  set.seed(123)
  data <- matrix(rnorm(50), ncol = 5)
  k <- 3

  set.seed(345)
  result1 <- .lloyds_algorithm(data, k, "euclidean")
  set.seed(345)
  result2 <- .lloyds_algorithm(data, k, "euclidean")

  expect_equal(result1$cluster_assignments, result2$cluster_assignments)
  expect_equal(result1$centroids, result2$centroids)
})


# ########## boundary performance tests
test_that("Lloyd's algorithm handles boundary cases with k near n", {
  data <- matrix(rnorm(20), ncol = 2)
  k <- 9  # Almost one cluster per point

  result <- .lloyds_algorithm(data, k, "euclidean")

  # Ensure that the number of unique cluster assignments is close to k
  expect_equal(length(unique(result$cluster_assignments)), k)
})

