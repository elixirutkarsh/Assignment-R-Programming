# Assignment-R-Programming
Q1. To create a file with 1000 lines of random strings, you can use the following code in R:
# Generate random strings
random_strings <- replicate(1000, paste(sample(letters, 10, replace = TRUE), collapse = ""))

# Write strings to a file
writeLines(random_strings, "random_strings.txt")

Q2. To perform the operations on the random dataset, you can use the following code in R:

# Create a random dataset
set.seed(123)  # Set seed for reproducibility
random_dataset <- matrix(sample(1:200, 100 * 30, replace = TRUE), nrow = 100, ncol = 30)

# (i) Replace values with NA between [10, 60]
random_dataset[10:60, ] <- NA
missing_rows <- sum(rowSums(is.na(random_dataset)) > 0)
print(paste("Number of rows with missing values:", missing_rows))

# (ii) Replace NA values with column average
column_averages <- colMeans(random_dataset, na.rm = TRUE)
random_dataset[is.na(random_dataset)] <- column_averages[col(random_dataset)[is.na(random_dataset)]]

# (iii) Find Pearson correlation and plot heat map
correlation_matrix <- cor(random_dataset)
corrplot::corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black")
selected_columns <- colnames(random_dataset)[colSums(abs(correlation_matrix) <= 0.7) == nrow(random_dataset)]
print(paste("Selected columns:", paste(selected_columns, collapse = ", ")))

# (iv) Normalize values between 0 and 10
normalized_dataset <- scale(random_dataset, center = FALSE, scale = apply(random_dataset, 2, max) / 10)

# (v) Replace values with 1 or 0 based on condition
random_dataset[random_dataset <= 0.5] <- 1
random_dataset[random_dataset > 0.5] <- 0
Q3. To apply clustering algorithms and plot the distance metric graph, you can use the following code in R:

# Create a random dataset
set.seed(123)  # Set seed for reproducibility
random_dataset <- data.frame(matrix(runif(5000, min = -10, max = 20), nrow = 500))

# Apply K-Means clustering
kmeans_clusters <- kmeans(random_dataset, centers = 2)
kmeans_dist <- dist(random_dataset)
plot(kmeans_dist, main = "Distance Metric Graph - K-Means Clustering")

# Apply Hierarchical clustering
hierarchical_clusters <- hclust(dist(random_dataset))
hierarchical_dist <- dist(random_dataset)
plot(hierarchical_dist, main = "Distance Metric Graph - Hierarchical Clustering")
Q4. To perform operations on a random dataset and create scatter plots, histograms, and box plots, you can use the following code in R:

# Create a random dataset
set.seed(123)  # Set seed for reproducibility
random_dataset <- matrix(runif(9000, min = -100, max = 100), nrow = 600, ncol = 15)

# (i) Scatter plot between Column 5 and Column 6
plot(random_dataset[, 5], random_dataset[, 6], main = "Scatter Plot", xlab = "Column 5", ylab = "Column 6")

# (ii) Histograms of each column in a single graph
par(mfrow = c(3, 5))  # Set layout for subplots
for (i in 1:15) {
  hist(random_dataset[, i], main = paste("Column", i), xlab = "")
}

# (iii) Box plots of each column in a single graph
par(mfrow = c(3, 5))  # Set layout for subplots
for (i in 1:15) {
  boxplot(random_dataset[, i], main = paste("Column", i))
}
Q5. To perform t-tests, Wilcoxon signed rank tests, and two-sample t-tests and Wilcoxon rank sum tests on columns of a random dataset, you can use the following code in R:

# Create a random dataset
set.seed(123)  # Set seed for reproducibility
random_dataset <- matrix(runif(2500, min = 5, max = 10), nrow = 500, ncol = 5)

# (i) Perform t-Test on each column
t_test_results <- apply(random_dataset, 2, function(col) t.test(col)$p.value)
print(t_test_results)

# (ii) Perform Wilcoxon Signed Rank Test on each column
wilcoxon_results <- apply(random_dataset, 2, function(col) wilcox.test(col)$p.value)
print(wilcoxon_results)

# (iii) Perform Two Sample t-Test and Wilcoxon Rank Sum Test on Column 3 and Column 4
column3 <- random_dataset[, 3]
column4 <- random_dataset[, 4]
t_test_2sample <- t.test(column3, column4)$p.value
wilcoxon_2sample <- wilcox.test(column3, column4)$p.value
print(paste("Two Sample t-Test p-value:", t_test_2sample))
print(paste("Wilcoxon Rank Sum Test p-value:", wilcoxon_2sample))
