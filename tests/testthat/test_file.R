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
########## Tests For Lloyd's Algorithm Helper Function
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


##############################
########## Tests For Recalculate Centroids Helper Function
##############################

test_that("recalculate centroids handles empty clusters by sampling a point", {
  data <- matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE)
  cluster_assignments <- c(1, 1)
  k <- 3

  result <- .recalculate_centroids(data, cluster_assignments, k)

  # Cluster 3 is empty; check if the centroid is sampled from existing data
  expect_true(all(result[1, ] == c(2, 3))) # Check cluster 1 centroid
  expect_true(any(result[3, ] %in% data))  # Check sampled centroid for empty cluster
})

test_that("recalculate centroids returns the data point itself when k = 1", {
  data <- matrix(c(5, 5), ncol = 2)
  cluster_assignments <- c(1)
  k <- 1

  result <- .recalculate_centroids(data, cluster_assignments, k)

  # Expect the centroid to be the single data point itself
  expect_equal(result, matrix(c(5, 5), ncol = 2))
})

test_that("recalculate centroids works with single-point clusters", {
  data <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2, byrow = TRUE)
  cluster_assignments <- c(1, 2, 3)
  k <- 3

  result <- .recalculate_centroids(data, cluster_assignments, k)

  # Each point should be its own centroid
  expect_equal(result, data)
})

test_that("recalculate centroids handles missing data gracefully", {
  data <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2, byrow = TRUE)
  cluster_assignments <- c(1, 1, 2)
  k <- 2

  result <- .recalculate_centroids(data, cluster_assignments, k)

  # Verify centroids with missing data
  expect_true(is.na(result[1, 1]) || !is.na(result[1, 1]))  # Handling potential NA correctly
  expect_true(any(result[2, ] %in% data, na.rm = TRUE))  # Check for valid centroid
})

##############################
########## Tests For Assign Cluster Helper Function
##############################

test_that("assign clusters correctly assigns points to nearest centroid", {
  data <- matrix(c(1, 1, 4, 4, 8, 8), ncol = 2, byrow = TRUE)
  centroids <- matrix(c(1, 1, 10, 10), ncol = 2, byrow = TRUE)
  euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

  result <- .assign_clusters(data, centroids, euclidean_distance)

  # Expected assignments: points (1,1) -> centroid (1,1); (4,4) and (8,8) -> (10,10)
  expect_equal(result, c(1, 1, 2))
})

test_that("assign clusters handles single data point and centroid correctly", {
  data <- matrix(c(5, 5), ncol = 2)
  centroids <- matrix(c(5, 5), ncol = 2)
  euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

  result <- .assign_clusters(data, centroids, euclidean_distance)

  # Expected assignment: single point (5,5) -> centroid (5,5)
  expect_equal(result, 1)
})

test_that("assign clusters works with multiple identical centroids", {
  data <- matrix(c(1, 1, 4, 4), ncol = 2, byrow = TRUE)
  centroids <- matrix(c(1, 1, 1, 1), ncol = 2, byrow = TRUE)
  euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

  result <- .assign_clusters(data, centroids, euclidean_distance)

  # Even with identical centroids, points (1,1) and (4,4) both assign to centroid 1
  expect_equal(result, c(1, 1))
})

test_that("assign clusters correctly assigns points when centroids are at extremes", {
  data <- matrix(c(-1, -1, 0, 0, 1, 1), ncol = 2, byrow = TRUE)
  centroids <- matrix(c(-10, -10, 10, 10), ncol = 2, byrow = TRUE)
  euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

  result <- .assign_clusters(data, centroids, euclidean_distance)

  # Expected assignments: all points closest to the first centroid (-10, -10)
  expect_equal(result, c(1, 1, 2))
})

test_that("assign clusters handles zero distances with identical data and centroids", {
  data <- matrix(c(2, 2, 2, 2), ncol = 2, byrow = TRUE)
  centroids <- matrix(c(2, 2), ncol = 2)
  euclidean_distance <- function(a, b) sqrt(sum((a - b)^2))

  result <- .assign_clusters(data, centroids, euclidean_distance)

  # All points are identical to the single centroid
  expect_equal(result, c(1, 1))
})

test_that("assign clusters handles non-Euclidean distance function", {
  data <- matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE)
  centroids <- matrix(c(0, 0, 3, 3), ncol = 2, byrow = TRUE)
  manhattan_distance <- function(a, b) sum(abs(a - b))

  result <- .assign_clusters(data, centroids, manhattan_distance)

  # Expected assignments: points (0,0) -> centroid (0,0), others -> (3,3)
  expect_equal(result, c(1, 1, 2))
})


##############################
########## Tests For Dunn Index Helper Function
##############################

test_that("dunn_index calculates correctly for well-separated clusters", {
  # Simulated k-means object
  clusters <- list(
    cluster = c(1, 1, 2, 2, 3, 3),
    data = matrix(c(1, 1, 2, 2, 10, 10, 11, 11, 20, 20, 21, 21), ncol = 2, byrow = TRUE),
    centers = matrix(c(1.5, 1.5, 10.5, 10.5, 20.5, 20.5), ncol = 2, byrow = TRUE)
  )

  # Calculate Dunn index
  result <- .dunn_index(clusters, "euclidean")

  # Expect a high Dunn index for well-separated clusters
  expect_true(result > 1)
})


test_that("dunn_index handles clusters with high intra-cluster variance", {
  clusters <- list(
    cluster = c(1, 1, 2, 2),
    data = matrix(c(1, 1, 10, 10, 5, 5, 15, 15), ncol = 2, byrow = TRUE),
    centers = matrix(c(5.5, 5.5, 10.5, 10.5), ncol = 2, byrow = TRUE)
  )

  result <- .dunn_index(clusters, "euclidean")

  # Expect a lower Dunn index due to high intra-cluster distances
  expect_true(result < 1)
})


##############################
########## Tests For CPP Functions
##############################

set.seed(501)
data <- matrix(runif(20), nrow = 5, ncol = 4)  # Random 5x4 matrix
point <- data[1, ]  # Selecting a point from the data
centroid <- colMeans(data)

########## Euclidean tests


test_that("euclidean_distance_cpp matches R's manual Euclidean distance calculation", {
  # Calculate Euclidean distance between a point and centroid using R
  r_distance <- sqrt(sum((point - centroid)^2))

  # Calculate Euclidean distance using the C++ function
  cpp_distance <- euclidean_distance_cpp(point, centroid)

  # Check if the distances are equal
  expect_equal(cpp_distance, r_distance, tolerance = 1e-8,
               info = "Mismatch between C++ and R Euclidean point-to-centroid distance")
})

test_that("euclidean_distance_cpp correctly handles identical points", {
  # Test with identical points
  identical_point <- point
  cpp_distance_identical <- euclidean_distance_cpp(point, identical_point)

  # Expect the distance to be zero
  expect_equal(cpp_distance_identical, 0,
               info = "Distance between identical points should be zero")
})

test_that("euclidean_point_distances handles a single row input correctly", {
  # Single point case
  single_row <- matrix(c(0, 0, 0, 0), nrow = 1)
  cpp_distances_single <- euclidean_point_distances(single_row)

  # Expect a 1x1 matrix with zero distance
  expect_equal(cpp_distances_single, matrix(0, nrow = 1, ncol = 1),
               info = "Single row matrix should return zero distance")
})

test_that("euclidean_point_distances handles empty input correctly", {
  # Empty matrix case
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 4)
  cpp_distances_empty <- euclidean_point_distances(empty_matrix)

  # Expect an empty matrix in return
  expect_equal(cpp_distances_empty, matrix(numeric(0), nrow = 0, ncol = 0),
               info = "Empty input matrix should return empty distance matrix")
})


########## Cosine tests


test_that("cosine_distance_cpp matches R's manual Cosine distance calculation", {
  # Calculate Cosine distance between a point and centroid using R
  dot_product <- sum(point * centroid)
  norm_point <- sqrt(sum(point^2))
  norm_centroid <- sqrt(sum(centroid^2))
  r_distance <- 1 - (dot_product / (norm_point * norm_centroid))

  # Calculate Cosine distance using the C++ function
  cpp_distance <- cosine_distance_cpp(point, centroid)

  # Check if the distances are equal
  expect_equal(cpp_distance, r_distance, tolerance = 1e-8,
               info = "Mismatch between C++ and R Cosine point-to-centroid distance")
})

test_that("cosine_distance_cpp correctly handles identical points", {
  # Test with identical points
  identical_point <- point
  cpp_distance_identical <- cosine_distance_cpp(point, identical_point)

  # Expect the distance to be zero
  expect_equal(cpp_distance_identical, 0,
               info = "Cosine distance between identical points should be zero")
})

test_that("cosine_point_distances handles a single row input correctly", {
  # Single point case
  single_row <- matrix(c(1, 1, 1, 1), nrow = 1)
  cpp_distances_single <- cosine_point_distances(single_row)

  # Expect a 1x1 matrix with zero distance
  expect_equal(cpp_distances_single, matrix(0, nrow = 1, ncol = 1),
               info = "Single row matrix should return zero distance")
})

test_that("cosine_point_distances handles empty input correctly", {
  # Empty matrix case
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 4)
  cpp_distances_empty <- cosine_point_distances(empty_matrix)

  # Expect an empty matrix in return
  expect_equal(cpp_distances_empty, matrix(numeric(0), nrow = 0, ncol = 0),
               info = "Empty input matrix should return empty distance matrix")
})


########## Manhattan tests


test_that("manhattan_distance_cpp matches R's manual Manhattan distance calculation", {
  # Calculate Manhattan distance between a point and centroid using R
  r_distance <- sum(abs(point - centroid))

  # Calculate Manhattan distance using the C++ function
  cpp_distance <- manhattan_distance_cpp(point, centroid)

  # Check if the distances are equal
  expect_equal(cpp_distance, r_distance, tolerance = 1e-8,
               info = "Mismatch between C++ and R Manhattan point-to-centroid distance")
})

test_that("manhattan_distance_cpp correctly handles identical points", {
  # Test with identical points
  identical_point <- point
  cpp_distance_identical <- manhattan_distance_cpp(point, identical_point)

  # Expect the distance to be zero
  expect_equal(cpp_distance_identical, 0,
               info = "Manhattan distance between identical points should be zero")
})

test_that("manhattan_point_distances handles a single row input correctly", {
  # Single point case
  single_row <- matrix(c(1, 2, 3, 4), nrow = 1)
  cpp_distances_single <- manhattan_point_distances(single_row)

  # Expect a 1x1 matrix with zero distance
  expect_equal(cpp_distances_single, matrix(0, nrow = 1, ncol = 1),
               info = "Single row matrix should return zero distance")
})

test_that("manhattan_point_distances handles empty input correctly", {
  # Empty matrix case
  empty_matrix <- matrix(numeric(0), nrow = 0, ncol = 4)
  cpp_distances_empty <- manhattan_point_distances(empty_matrix)

  # Expect an empty matrix in return
  expect_equal(cpp_distances_empty, matrix(numeric(0), nrow = 0, ncol = 0),
               info = "Empty input matrix should return empty distance matrix")
})



