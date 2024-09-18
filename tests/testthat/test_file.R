##### datasets

## generated data

set.seed(31415926)
x <- c(rnorm(100,0,1), rnorm(100,2,3), rnorm(100, 10, 4))
y <- c(rnorm(100,0,1), rnorm(100,2,3), rnorm(100, 10, 4))

df1 <- as.data.frame(cbind(x,y))

#calculate clusters
clusty_k <- kmeans(df1, 3, algorithm = "Lloyd") #kmeans
clusty <- calculate_clusters(df1, 3, "euclidean", scale = FALSE) #lloydr

#plot kmeans
plot(df1, col = clusty_k$cluster)
points(clusty_k$centers, col = 1:2, pch = 8, cex = 2)

#plot lloydr
plot(df1, col = clusty$cluster)
points(clusty$centers, col = 1:2, pch = 8, cex = 2)


cl2 <- calculate_clusters(as.data.frame(x),2,"Euclidean", scale = F)
class(cl2)
plot(x, col = cl2$cluster)
points(cl2$centers, col = 1:2, pch = 8, cex = 2)
compare_clusters(as.data.frame(x),2, scale = F)
