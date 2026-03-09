# Install if needed
install.packages("MASS")

library(MASS)

set.seed(123)

n <- 1000
rho <- 0.8

# Mean vector
mu <- c(0, 0)

# Covariance matrix
Sigma <- matrix(c(1, rho,
                  rho, 1), nrow = 2)


# Cholesky decomposition
L <- chol(Sigma)

# Generate independent normal data
Z <- matrix(rnorm(n*2), nrow = n)

# Create correlated data
X <- Z %*% L


# Generate data
X <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

# Check correlation
cor(X)

# Scatter plot
plot(X[,1], X[,2],
     xlab = "X1",
     ylab = "X2",
     main = "2D Data with Correlation 0.8",
     pch = 19)

# Principal Component Analysis (PCA) on matrix X — full workflow

# -------------------------------
# 2. Check structure
# -------------------------------
dim(X)
head(X)

# -------------------------------
# 3. Handle missing values (optional)
# -------------------------------
# Remove rows with NA
X <- na.omit(X)

# -------------------------------
# 4. Standardize variables
# -------------------------------
# PCA typically requires scaling
X_scaled <- scale(X)

# -------------------------------
# 5. Compute covariance matrix
# -------------------------------
cov_matrix <- cov(X_scaled)

# -------------------------------
# 6. Eigen decomposition
# -------------------------------
eig <- eigen(cov_matrix)

eigenvalues  <- eig$values
eigenvectors <- eig$vectors

# -------------------------------
# 7. Principal component scores
# -------------------------------
scores <- X_scaled %*% eigenvectors

# -------------------------------
# 8. Variance explained
# -------------------------------
variance_explained <- eigenvalues / sum(eigenvalues)
cumulative_variance <- cumsum(variance_explained)

variance_explained
cumulative_variance

# -------------------------------
# 9. Scree plot
# -------------------------------
plot(eigenvalues,
     type = "b",
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     main = "Scree Plot")

# -------------------------------
# 10. Biplot
# -------------------------------
biplot(prcomp(X_scaled))

library(ggplot2)

scores_df <- as.data.frame(scores)

ggplot(scores_df, aes(x = V1, y = V2)) +
  geom_point() +
  labs(title = "PCA Projection",
       x = "PC1",
       y = "PC2")
