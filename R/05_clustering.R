# Load libraries:
library("tm")

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

set.seed(69)

# Do elbow method:
n = 15
SSW = rep(0, n)

for (a in 1:n) {
  K = kmeans(text_matrix, a, nstart = 10) # K-Means
  SSW[a] = K$tot.withinss # total within-cluster sum of squares
}
# Plot elbow:
plot(1:n, SSW, type = "b", xlab = "K", main = "K-Means Elbow Method")

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
n = 15
SSW = rep(0, n)
for (a in 1:n) {
  K = kmeans(mds.matrix, a, nstart = 20) # K-Means
  SSW[a] = K$tot.withinss # total within-cluster sum of squares
}
# Plot elbow:
plot(1:n, SSW, type = "b", xlab = "K", main = "K-Means Elbow Method (MDS)")

best_K = 4 # select K from elbow

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


