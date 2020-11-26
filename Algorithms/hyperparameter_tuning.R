### Defining working directory ----
script_location <- getwd()
root_folder <- dirname(script_location)
data_folder <- paste0(root_folder, '/Data Outputs')
svr_location <- paste0(root_folder, '/Algorithms/SVR/')

### Sourcing auxiliary R files ----
setwd(svr_location)
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

# Scaling the data
X = scale(X)
Y = scale(Y)

## Defining performance metric to be used in Cross Validation
RMSE = function(forecasted, observed){
  
  val <- sqrt(mean((forecasted - observed)^2))
  return(val)
  
}

# ----------------------------------------------------------------------
#
# Hyperparameter tuning
# Cross validation on a rolling basis & hyperparameter tuning
#
# ----------------------------------------------------------------------

# Cross validation function
roll_cross_validation <- function(X, Y, start_size = 40, K = 10, model, hyperparameters){
  
  if(model == "SVR"){
    
    # Creating a list to store average RMSE values from each iteration
    average_RMSE_vector <- c()

    for(i in 1:nrow(hyperparameters)){
      
      # Extracting hyperparameters to be used in each iteration
      poly_deg <- hyperparameters[i, 1]
      ke <- as.character(hyperparameters[i, 2])
      C <- hyperparameters[i, 3]
      e <- hyperparameters[i, 4]

      # Defining a list that will then be used to calculate average RMSE
      RMSE_vector <- c()
      indx = 1
      
      # Resetting parameters
      start_size_svr = start_size
      
      while(start_size_svr < nrow(X)){
        
        ## Splitting data on a rolling basis
        # Defining the train validation
        train_X <- X[1:start_size_svr, ]
        train_Y <- Y[1:start_size_svr, ]
        
        # Increasing the counter
        start_size_svr <- start_size_svr + K
        
        # Defining the test validation
        test_X <- X[(dim(train_X)[1] + 1):start_size_svr, ]
        test_Y <- Y[(length(train_Y) + 1):start_size_svr, ]
        
        ########################################################
        ########################################################
        
        ## Training model
        # Defining number of train observations
        n = dim(train_X)[1]
        
        # Passing parameters to SVR function
        model <- svr_model(train_X, train_Y, n, ke, poly_deg, C, e)
        
        # Extracting outputs from model
        beta <- as.matrix(model[[1]])
        bias <- model[[3]]
        
        ########################################################
        ########################################################
        
        ## Testing model
        # Defining number of test observations
        n = dim(test_X)[1]
        
        # Finding Hessian Matrix of test X
        H_test <- hess_mat_test(test_X, train_X, poly_deg)
        
        # Finding predicted values
        forecasted_vals <- H_test %*% beta + bias
        
        # Calculating RMSE
        RMSE_val <- RMSE(forecasted_vals, test_Y)
        
        # Adding to list, plus increasing counter
        RMSE_vector[indx] <- RMSE_val
        indx = indx + 1
        
      }
      
      print(i)
      # Calculating average RMSE and storing
      average_RMSE_vector[i] <- mean(RMSE_vector)
      
    }
    
  } else {
    
    
  }
  
  output_data <- cbind(hyperparameters, average_RMSE_vector)
  return(output_data)
  
}

## Defining the cross validation parameters
start_size = 40
K = 10
model = "SVR"

### Defining hyperparameter grid
## SVR
# degree of polynomials (No more than 2)
p1 = seq(from = 1, to = 2, by = 1)
# type of kernel
ke = "polynomial"
# Cost
C = seq(from = 1, to = 5, by = 0.5)
# e (No more than 0.5)
e = seq(from = 0.01, to = 0.5, by = 0.05)

# Creating the grid
hyperparameters <- expand.grid(poly_deg = p1, ke = "polynomial", 
                               C = C, e = e)

### Running the function
wew <- roll_cross_validation(X, Y, start_size, K, model, hyperparameters)

# Best for SVR: 1, poly, 1, 0.31