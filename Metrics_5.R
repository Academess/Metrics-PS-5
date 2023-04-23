library(plm)

set.seed(10101)

##########################
# Simulation 1
##########################

# Create a matrix to store the coefficients
beta <- rep(NA, 500)

for (i in 1:500){
  # Set the original unemployment rate to be 5%
  ctrl_ur <- matrix(5 + rnorm(20*31, mean = 0, sd = 1), nrow = 20, ncol = 15)
  
  # Transform the matrix to one vector
  ctrl_ur_vector <- c(t(ctrl_ur))
  
  # Set the treatment status of the control groups to be all 0
  ctrl_status <- rep(0, 620)
  
  # Combine two vectors
  ctrl_obs <- cbind(ctrl_ur_vector, ctrl_status)
  
  # Generate the observations for early adopters
  early_ur <- matrix(NA, nrow=15, ncol=31)
  early_ur[, 1:11] <- 5 + rnorm(15*11, mean=0, sd=1)
  early_ur[, 12:31] <- 10 + rnorm(15*20, mean=0, sd=1)
  early_ur_vector <- c(t(early_ur))
  early_status_each <- c(rep(0, 11), rep(1, 20))
  early_status <- rep(early_status_each, time = 15)
  early_obs <- cbind(early_ur_vector, early_status)
  
  # Generate the observations for late adopters
  late_ur <- matrix(NA, nrow=15, ncol=31)
  late_ur[, 1:25] <- 5 + rnorm(15*25, mean=0, sd=1)
  late_ur[, 26:31] <- 10 + rnorm(15*6, mean=0, sd=1)
  late_ur_vector <- c(t(late_ur))
  late_status_each <- c(rep(0, 25), rep(1, 6))
  late_status <- rep(late_status_each, time = 15)
  late_obs <- cbind(late_ur_vector, late_status)
  
  # Combine the observations of three groups
  obs <- rbind(ctrl_obs, early_obs, late_obs)
  
  # Generate ids
  id <- c(rep(1:50, each = 31))
  
  # Generate years
  year <- c(rep(1980:2010, times = 50))
  
  # Combine the observations with years
  data <- cbind(id, year, obs)
  
  colnames(data) <- c("id", "year", "ur", "D")
  
  # Staggered DID
  did <- plm(ur ~ D, data = data, model = "within", index = c("id", "year"))
  
  # Store the coefficient
  beta[i] <- coef(did)[1]
}

# Plot the distribution of the coefficients
hist(beta, breaks = 20, main = "Distribution of Coefficients", xlab = "Values of Coefficients", ylab = "Density", xlim = c(4.5, 5.5))
abline(v = mean(beta), col = "red", lwd = 2)

##########################
# Simulation 2
##########################

# Create a matrix to store the coefficients
beta <- rep(NA, 500)

for (i in 1:500){
  # Set the original unemployment rate to be 5%
  ctrl_ur <- matrix(5 + rnorm(20*31, mean = 0, sd = 1), nrow = 20, ncol = 15)
  
  # Transform the matrix to one vector
  ctrl_ur_vector <- c(t(ctrl_ur))
  
  # Set the treatment status of the control groups to be all 0
  ctrl_status <- rep(0, 620)
  
  # Combine two vectors
  ctrl_obs <- cbind(ctrl_ur_vector, ctrl_status)
  
  # Generate the observations for early adopters
  early_ur <- matrix(NA, nrow=15, ncol=31)
  early_ur[, 1:11] <- 5 + rnorm(15*11, mean=0, sd=1)
  early_ur[, 12:31] <- 7.5 + rnorm(15*20, mean=0, sd=1)
  early_ur_vector <- c(t(early_ur))
  early_status_each <- c(rep(0, 11), rep(1, 20))
  early_status <- rep(early_status_each, time = 15)
  early_obs <- cbind(early_ur_vector, early_status)
  
  # Generate the observations for late adopters
  late_ur <- matrix(NA, nrow=15, ncol=31)
  late_ur[, 1:25] <- 5 + rnorm(15*25, mean=0, sd=1)
  late_ur[, 26:31] <- 12.5 + rnorm(15*6, mean=0, sd=1)
  late_ur_vector <- c(t(late_ur))
  late_status_each <- c(rep(0, 25), rep(1, 6))
  late_status <- rep(late_status_each, time = 15)
  late_obs <- cbind(late_ur_vector, late_status)
  
  # Combine the observations of three groups
  obs <- rbind(ctrl_obs, early_obs, late_obs)
  
  # Generate ids
  id <- c(rep(1:50, each = 31))
  
  # Generate years
  year <- c(rep(1980:2010, times = 50))
  
  # Combine the observations with years
  data <- cbind(id, year, obs)
  
  colnames(data) <- c("id", "year", "ur", "D")
  
  # Staggered DID
  did <- plm(ur ~ D, data = data, model = "within", index = c("id", "year"))
  
  # Store the coefficient
  beta[i] <- coef(did)[1]
}

# Plot the distribution of the coefficients
hist(beta, breaks = 20, main = "Distribution of Coefficients", xlab = "Values of Coefficients", ylab = "Density", xlim = c(4, 5))
abline(v = mean(beta), col = "red", lwd = 2)