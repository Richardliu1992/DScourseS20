install.packages("nloptr")
library(nloptr)

# Question 4. Using R, Python, or Julia, create a data set that has the following properties:
# Set the seed of the random number generator by issuing the (R) command.
set.seed(100)

# X is a matrix of dimension N = 100, 000 by K = 10 containing normally distributed random numbers.
N <- 100000
K <- 10
sigma <- 0.5
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K) 

# The first column which should be a column of 1’s.
X[,1] <- 1

# ε (call it eps in your code) is a vector of length N containing random numbers.
eps <- rnorm(N,mean=0,sd=sigma)

# β (call it beta in your code) is a vector of length 10.
beta <- c( 1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Now generate Y which is a vector equal to Xβ + ε.
Y <- X%*%beta + eps 

# Question 5 compute βˆOLS, which is the OLS estimate of β using the closed-form solution.
beta_OLS <- solve(t(X)%*%X)%*%(t(X)%*%Y)
View(beta_OLS)

# Question 6 Compute βˆOLS using gradient descent.
# set up a stepsize
alpha <- 0.0000003

# set up a number of iterations
maxiter <- 500000

# define the gradient 
gradient <- function(beta_GD,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta_GD)) )
  }

## initial values
beta_GD <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
set.seed(100)

# create a vector to contain all beta's for all steps
beta_GD.All <- matrix("numeric",length(beta_GD),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta_GD0 <- 0*beta_GD
while (norm(as.matrix(beta_GD0)-as.matrix(beta_GD))>1e-8) {
  beta_GD0 <- beta_GD
  beta_GD <- beta_GD0 - alpha*gradient(beta_GD0,Y,X)
  beta_GD.All[,iter] <- beta_GD
  if (iter%%10000==0) {
    print(beta_GD)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta_GD,Y,X) is ", beta_GD, sep = ""))

#7.1 Compute βˆOLS using nloptr’s L-BFGS algorithm. 
library(nloptr)

## Our objective function
objfun <- function(beta_OLS_L,Y,X) {
  return (sum((Y-X%*%beta_OLS_L)^2))
}

## Gradient of our objective function
gradient <- function(beta_OLS_L,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta_OLS_L)) )
}

## initial values
beta_OLS_L <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta_OLS_L,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)
beta_OLS_L<-result$solution

#7.2 Do it again using the Nelder-Mead algorithm.
## initial values
beta_OLS_NM <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options_NM <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

result_NM <- nloptr( x0=beta_OLS_NM,eval_f=objfun,opts=options_NM,Y=Y,X=X)
beta_OLS_NM <-result_NM$solution

#Question 8 Now compute βˆ MLE using nloptr’s L-BFGS algorithm.
library(nloptr)
## Our objective function
objfun_MLE  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

## Gradient of our objective function
gradient_MLE <- function (theta,Y,X) {
  grad <- as.vector (rep (0, length (theta)))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(Y - X%*%beta)/(sig ^2)
  grad[ length (theta )] <- dim(X)[1]/sig - crossprod (Y-X%*%beta)/(sig^3)
  return ( grad )                                                 
}


## initial values
beta_MLE0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options_MLE <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result_MLE <- result <- nloptr( x0=beta_MLE0,eval_f=objfun_MLE,eval_grad_f=gradient_MLE,opts=options_MLE,Y=Y,X=X)
print(result_MLE)
beta_MLE  <- result_MLE$solution[1:(length(result_MLE$solution)-1)]
sigma_MLE <- result_MLE$solution[length(result_MLE$solution)]

# Question 9. Now compute βˆOLS the easy way: using lm() and directly calling the matrices Y and X 
beta_LM_EST <- lm(Y ~ X -1)
beta_LM <- beta_LM_EST$coefficients

library(stargazer)
stargazer(beta_LM_EST)

beta.ALL <- cbind.data.frame(beta,beta_OLS,beta_GD,beta_OLS_L,beta_OLS_NM,beta_MLE,beta_LM)
xtable(beta.ALL, type = "latex", file = "SUMMARY.tex")
