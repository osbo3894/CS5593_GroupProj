### Defining working directory ----
script_location <- getwd()
root_folder <- dirname(script_location)
data_folder <- paste0(root_folder, '/Data Outputs')
svr_location <- paste0(root_folder, '/Algorithms/SVR/')

### Sourcing auxiliary R files ----
setwd(svr_location)
source('svr_functions.R')

### Sourcing Cross validation file ----
setwd(script_location)
source('rolling_cross_validation.R')

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

# Scaling the data
X = scale(X)
Y = scale(Y)

# ----------------------------------------------------------------------
#
# Hyperparameter tuning
# Cross validation on a rolling basis & hyperparameter tuning
#
# ----------------------------------------------------------------------

## Defining the cross validation parameters
# start_size = 40
# K = 10
start_size = 100
K = 30
model = "SVR"

### Defining hyperparameter grid
## SVR
# degree of polynomials (No more than 2)
p1 = seq(from = 1, to = 2, by = 1)
# type of kernel
ke = c("polynomial", "linear")
# Cost
C = seq(from = 1, to = 5, by = 1)
# e (No more than 0.5)
e = seq(from = 0.1, to = 0.5, by = 0.1)

# Creating the grid
hyperparameters <- expand.grid(poly_deg = p1, ke = ke, 
                               C = C, e = e)

### Running the function
wew <- roll_cross_validation(X, Y, start_size, K, model, hyperparameters)

# Best for SVR: 1, poly, 1, 0.31