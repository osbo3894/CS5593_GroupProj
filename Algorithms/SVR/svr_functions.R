### Libraries used ----
# For the ipop() function, to solve a quadratic programming problem.
library(kernlab)

#####################################################################
#
# Auxiliary functions
#
#####################################################################

hess_mat <- function(X, n, ke, p1){
  
  ####################################################################
  #
  # This function takes X (data matrix), 
  # n (number of data objects) and defines H.
  #
  ####################################################################
  
  # Defining the Hessian Matrix
  H = matrix(0, ncol = n, nrow = n)
  
  # Defining a two-loop to apply a linear kernel (others are possible)
  # as p'*q, if p and q are matrices, then we apply the dot product.
  
  if(ke == "linear"){
    
    for(i in 1:n){
      for(j in 1:n){
        
        H[i, j] = t(X[i, ]) %*% X[j, ]
        
      }
    }
    
  } else if(ke == "polynomial"){
    
    for(i in 1:n){
      for(j in 1:n){
        
        H[i, j] = (t(X[i, ] %*% X[j, ] + 1)^p1)
        
      }
    }
    
  }
  
  return(H)
  
}

optim_params <- function(H, e, n, Y, C){
  
  ####################################################################
  #
  # H is the hessian matrix (from hess_mat function)
  # e is the tolerance (set at 0.05)
  # n is the number of data objects
  # Y is the variable to predict
  # C is the cost (in this application, it'll be 1)
  #
  ####################################################################
  
  # Defining H_sq (square matrix appearing in the quadratic function)
  H_sq <- rbind(cbind(H, -H), 
                cbind(-H, H))
  
  # Defining f (vector appearing in quadratic function)
  f <- rbind(as.matrix(e * matrix(1, nrow = n) - Y),
             as.matrix(e * matrix(1, nrow = n) + Y))
  
  # Defining the starting point
  x0 = matrix(0, nrow = 2 * n)
  
  # Defining the upper and lower bounds
  lb = matrix(0, nrow = 2 *n)
  ub = C * matrix(1, nrow = 2 * n)
  
  # Defining the matrix  and vector (0 for equality) of constraints
  A = t(rbind(matrix(1, nrow = n), matrix(-1, nrow = n)))
  b = 0
  
  return(list(f = f, H_sq = H_sq,
              lb = lb, ub = ub,
              x0 = x0, A = A,
              b = b))
  
}

svr_vals <- function(qp_outputs, n, Y, epsilon, H){
  
  ####################################################################
  #
  # qp_outputs contains the alpha (from the primal objective)
  # and the lambda (from the dual -- langragian multipliers)
  #
  # n (number of data objects)
  # Y (variable of interest)
  # epsilon (hyper parameter -- set at 0.1)
  #
  ####################################################################
  
  # Calculating Beta
  beta = primal(qp_outputs)[1:n] - primal(qp_outputs)[(n + 1):length(primal(qp_outputs))]
  
  # Checking the support vectors
  sup_vects <- which(abs(beta) > epsilon)
  
  # Calculating bias -- there are two implementations
  sv <- which(abs(beta) > epsilon & abs(beta) < (C - epsilon))
  
  # First implementation
  bias.1 <- (1 / length(sv)) * sum(Y[sv] - (e * sign(beta[sv])) - H[sv, sup_vects]*beta[sup_vects])
  bias.2 <- (max(Y) + min(Y)) / 2
  bias.3 <- mean(Y - H %*% beta)
  
  return(list(beta, bias.1, bias.2, bias.3))
  
}

