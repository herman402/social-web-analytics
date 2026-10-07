# Load libraries:
library("tm")
library("cluster")

# Load processed data:
load("data/processed/bluesky_processed_data.RData")

# K-Means Clustering ====
# Build matrix to be used:
text_tdm = TermDocumentMatrix(corpus)
text_wtdm = weightTfIdf(text_tdm)
text_matrix = t(as.matrix(text_wtdm))

# Keep non-empty
not_empties = which(rowSums(abs(text_matrix)) > 0)
text_matrix = text_matrix[not_empties, ]

# Normalise to unit length:
normalised_matrix = diag(1 / sqrt(rowSums(text_matrix ^ 2))) %*% text_matrix

# Cosine distance:
D = dist(normalised_matrix, method = "euclidean") ^ 2 / 2

# Available unit vectors:
N = dim(text_matrix)[1]
N

# MDS:
mds.matrix = cmdscale(D, k = 301) # 301/322 eigenvalues > 0

dim(as.matrix(D))
dim(mds.matrix)

# Redo elbow method:
n = 20
SSW = rep(0, n)
silhouette_scores = rep(0, n)
for (a in 1:n) {
  K = kmeans(mds.matrix, a, nstart = 20) # K-Means
  SSW[a] = K$tot.withinss # total within-cluster sum of squares
  
  # Silhouette score: (to find best K)
  if (a > 1) {
    sil = silhouette(K$cluster, dist(mds.matrix))
    silhouette_scores[a] = mean(sil[, 3])
  }
}
# Plot elbow:
best_K = which.max(silhouette_scores) # best amount of clusters
plot(1:n, SSW, type = "b", xlab = "K", main = "K-Means Elbow Method")
abline(v = best_K, col = "red", lty = 2, lwd = 2)

# Plot clustering:
K = kmeans(mds.matrix, best_K, nstart = 20)
mds2.matrix = cmdscale(D, k = 2)
plot(mds2.matrix, col = K$cluster, 
     xlab = "MDS Dimension 1", ylab = "MDS Dimension 2",
     main = "Visualisation of K-Means Clusters in Bluesky Posts")

# Examine clusters:
summary(K)
table(K$cluster)

# Finding top terms for each cluster:
for (i in 1:best_K) {
  clusterStr = paste("Cluster", i)
  
  clusterId = which(K$cluster == i)
  
  # Extract original TF-IDF matrix:
  clusterposts = text_matrix[clusterId, ]
  
  # Average each term's TF-IDF weight across documents:
  clusterTermWeight = colMeans(clusterposts)
  print(clusterStr)
  print(sort(clusterTermWeight, decreasing = TRUE)[1:10])
}
