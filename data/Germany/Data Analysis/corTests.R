# This file is an R script which analyses the data from the
# Naming the Pain in RE survey. It calculates the correlation
# coefficient Kendall's tau for all numerical results.

# load the dataset
input = read.table("R-Import.csv", header=TRUE, sep=";")

# Correlations using Kendall. In case of missing data, we ignore
# pairs with missing data.
corrmatrix = cor(input, method = "kendall", use= "pairwise.complete.obs")

# write dataset to file
write.csv(corrmatrix, file = "corrmatrix.csv", sep = ";")
