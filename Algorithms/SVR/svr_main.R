### Defining working directory ----
script_location <- getwd()
root_folder <- dirname(dirname(script_location))
data_folder <- paste0(root_folder, '/Data Outputs')

### Libraries used ----

### Sourcing auxiliary R files ----
setwd(script_location)
source('svr_functions.R')

### Loading data ----
setwd(data_folder)
data <- read.csv("combined_processed_data.csv")

# Extracting Y (includes all states + districts)
# Y_all = data[, 1:52]
# But for now, let's just test one single Y
Y = data[, 2]

# Generally, we would use all predictors:
# X = data[, 53:ncol(data)]
# But for now, let's test just a couple
X = data[, 53:ncol(data)]

# ----------------------------------------------------------------------
#
# Running the model
#
# ----------------------------------------------------------------------

# First, scaling the data
X = scale(X)
Y = scale(Y)

# Then, defining the number of data objects as well as all other hyperparameters
n = dim(X)[1]
p1 = 2
ke = "polynomial"
C = 1
e = 0.05

# Finally, running the model
results <- svr_model(X, Y, n, ke, p1, C, e)

# Extracting the estimated beta, and the two bias implementations
beta <- as.matrix(results[[1]])
bias1 <- results[[2]]
bias2 <- results[[3]]
bias3 <- results[[4]]

# Finding the fitted values
# We have to define H (just like we do in svr_model)
H <- hess_mat(X, n, ke, p1)

# Calculating the fitted values
svr_fit2 <- H %*% beta + bias2
svr_fit3 <- H %*% beta + bias3
svr_fit1 <- H %*% beta
