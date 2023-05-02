library(geepack)
library(mice)
library(dplyr)
library(ggplot2)
library("splines")
library("lme4")
library("JointAI")
=============================================================================================================================================================================
#SIMULATE DATA

# Set seed for reproducibility
set.seed(123)

# Define the number of subjects and time points
n_subjects <- 200
n_timepoints <- 5

# Define the true parameter values
beta_0 <- 25
beta_1 <- -1
beta_2 <- 0
beta_3 <- 1

# Generate subject-level random effects
v_0i <- rnorm(n_subjects, mean = 0, sd = 1)
v_1i <- rnorm(n_subjects, mean = 0, sd = 1)

# Generate error terms
epsilon_ij <- rnorm(n_subjects * n_timepoints, mean = 0, sd = 1)

group <- sample(c(0,1), size = n_subjects, replace = TRUE)

# Initialize empty vectors to store the outcome variables and subject and time indices
y_ij <- numeric(n_subjects * n_timepoints)
subject_idx <- numeric(n_subjects * n_timepoints)
time_idx <- numeric(n_subjects * n_timepoints)

# Loop through each subject
for (i in 1:n_subjects) {
  # Loop through each time point for the current subject
  for (j in 1:n_timepoints) {
    # Calculate the subject and time indices
    idx <- (i-1)*n_timepoints + j
    
    # Store the subject and time indices
    subject_idx[idx] <- i
    time_idx[idx] <- j
    
    # Calculate the outcome variable using the given model
    y_ij[idx] <- beta_0 + beta_1*j + beta_2*group[i] + beta_3*(group[i]*j) + v_0i[i] + v_1i[i]*j + epsilon_ij[idx]
  }
}

# Combine data into a data frame
data <- data.frame(subject = subject_idx, 
                   time = time_idx,
                   group = group[subject_idx],
                   y = y_ij)
data

interaction.plot(data$time, data$subject, data$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

df = data %>% filter(subject > 10 & subject < 20)
require(lattice)
xyplot(y ~ time | subject, data=df, group=subject,  xlab=list(label="Time", cex=2), ylab=list(label="y", cex=2), aspect = "xy", type = c("p", "r"), col.line = "darkorange")

----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit the linear mixed effects model
model1 <- lmer(y ~ time*group + (1 + time | subject), data=data)

summary(model1)

-------------------------------------------------------------------------------------------------------------------------------------------------
  
# Define a function to remove 50% of the y data randomly
  remove_data <- function(df) {
    # Loop through each time point
    for (j in 1:n_timepoints) {
      # Subset the data for the current time point
      df_time <- df[df$time == j, ]
      
      # Generate random numbers
      rand <- runif(nrow(df_time))
      
      # Remove y values where rand is less than 0.5
      df_time$y <- ifelse(rand < 0.5, NA, df_time$y)
      
      # Replace the data for the current time point in the data frame
      df[df$time == j, ] <- df_time
    }
    
    # Return the new data frame
    return(df)
  }

# Create a new data frame with 50% of the y data removed randomly at each time point
data_new <- remove_data(data)
data_new

interaction.plot(data_new$time, data_new$subject, data_new$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Impute missing values in data_new, i.e., data where 50% were removed
imp1 <- mice(data_new, maxit = 0)
meth <- imp1$method
meth
pred <- imp1$predictorMatrix
pred
pred[, c('subject', 'time', 'group')] <- 0
imp_mice1 <- mice(data_new, method = meth, predictorMatrix = pred, maxit = 20, seed = 123)
imputed_data1 <- complete(imp_mice1)
imputed_data1

interaction.plot(imputed_data1$time, imputed_data1$subject, imputed_data1$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

# Fit the linear mixed effects model
  model5 <- lmer(y ~ time*group + (1 + time | subject), data=imputed_data1)

summary(model5)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit the linear mixed effects model
  model2 <- lmer(y ~ time*group + (1 + time | subject), data=data_new)

summary(model2)

data_new$y_pred <- predict(model2, newdata = data_new, re.form = NA)
data_new$y[is.na(data_new$y)] <- data_new$y_pred[is.na(data_new$y)]

interaction.plot(data_new$time, data_new$subject, data_new$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

df2 <- data_new[,-5]
df2
==========================================================================================================================================================================
  
# Define a function to randomly generate missing data according to dropout rates of 0%, 25%, 50%, 75%, 87.5% for the five timepoints
  generate_missing_data <- function(df, dropout_rates) {
    # Loop through each time point
    for (j in 1:n_timepoints) {
      # Subset the data for the current time point
      df_time <- df[df$time == j, ]
      
      # Generate missing data for the current time point
      n_missing <- round(nrow(df_time) * dropout_rates[j])
      missing_rows <- sample(nrow(df_time), size = n_missing, replace = FALSE)
      df_time$y[missing_rows] <- NA
      
      # Replace the data for the current time point in the data frame
      df[df$time == j, ] <- df_time
    }
    
    # Return the new data frame with missing data
    return(df)
  }

# Define the dropout rates for the five time points
dropout_rates <- c(0, 0.25, 0.5, 0.75, 0.875)

# Create a new data frame with missing data
data_new2 <- generate_missing_data(data, dropout_rates)
data_new2

interaction.plot(data_new2$time, data_new2$subject, data_new2$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Impute missing values in data_new, i.e., where data was removed according to dropout rates of 0%, 25%, 50%, 75%, 87.5% for the five timepoints
imp2 <- mice(data_new2, maxit = 0)
meth2 <- imp2$method
meth2
pred2 <- imp2$predictorMatrix
pred[, c('subject', 'time', 'group')] <- 0
pred2
imp_mice2 <- mice(data_new2, method = meth2, predictorMatrix = pred2, maxit = 5, seed = 123)
imputed_data2 <- complete(imp_mice2)
imputed_data2

interaction.plot(imputed_data2$time, imputed_data2$subject, imputed_data2$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

# Fit the linear mixed effects model
  model6 <- lmer(y ~ time*group + (1 + time | subject), data=imputed_data2)

summary(model6)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit the linear mixed effects model
  model3 <- lmer(y ~ time*group + (1 + time | subject), data=data_new2)

summary(model3)

data_new2$y_pred2 <- predict(model3, newdata = data_new2, re.form = NA)
data_new2$y[is.na(data_new2$y)] <- data_new2$y_pred[is.na(data_new2$y)]

interaction.plot(data_new2$time, data_new2$subject, data_new2$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

df3 <- data_new2[,-5]
df3

=====================================================================================================================================================================
  
# Define a function to randomly generate missing data according to dropout rates of 0%, 23%, 46%, 70% and 83% for G=0 and 0%, 27%, 55%, 81% and 91% for G=1 for the five time points.
  generate_missing_data <- function(df, dropout_rates) {
    # Loop through each time point
    for (j in 1:n_timepoints) {
      # Subset the data for the current time point
      df_time <- df[df$time == j, ]
      
      # Generate missing data for the current time point
      n_missing <- round(nrow(df_time) * dropout_rates[j])
      missing_rows <- sample(nrow(df_time), size = n_missing, replace = FALSE)
      df_time$y[missing_rows] <- NA
      
      # For subjects that were missing at a time point, set y to NA for all later time points
      for (k in (j+1):n_timepoints) {
        df_time$y[df_time$time == k] <- NA
      }
      
      # Replace the data for the current time point in the data frame
      df[df$time == j, ] <- df_time
    }
    
    # Return the new data frame with missing data
    return(df)
  }

# Define the dropout rates for the five time points for each group
dropout_rates_G0 <- c(0, 0.23, 0.46, 0.7, 0.83)
dropout_rates_G1 <- c(0, 0.27, 0.55, 0.81, 0.91)

# Create a new data frame with missing data for group 0
data_G0 <- data[data$group == 0, ]
n_timepoints <- length(unique(data_G0$time))
data_G0_missing <- generate_missing_data(data_G0, dropout_rates_G0)

# Create a new data frame with missing data for group 1
data_G1 <- data[data$group == 1, ]
n_timepoints <- length(unique(data_G1$time))
data_G1_missing <- generate_missing_data(data_G1, dropout_rates_G1)

# Combine the two data frames
data_new3 <- rbind(data_G0_missing, data_G1_missing)
data_new3

interaction.plot(data_new3$time, data_new3$subject, data_new3$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Impute missing values in data_new, i.e., where data was removed according to dropout rates of 0%, 23%, 46%, 70% and 83% for G=0 and 0%, 27%, 55%, 81% and 91% for G=1 for the five time points

imp3 <- mice(data_new3, maxit = 0)
meth <- imp3$method
meth3
pred3 <- imp3$predictorMatrix
pred3
imp_mice3 <- mice(data_new3, method = meth3, predictorMatrix = pred, maxit = 20, seed = 123)
imputed_data3 <- complete(imp_mice3)
imputed_data3

interaction.plot(imputed_data3$time, imputed_data3$subject, imputed_data3$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

# Fit the linear mixed effects model
  model7 <- lmer(y ~ time*group + (1 + time | subject), data=imputed_data3)

summary(model7)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Fit the linear mixed effects model
  model4 <- lmer(y ~ time*group + (1 + time | subject), data=data_new3)

summary(model4)

data_new3$y_pred3 <- predict(model4, newdata = data_new3, re.form = NA)
data_new3$y[is.na(data_new3$y)] <- data_new3$y_pred[is.na(data_new3$y)]

interaction.plot(data_new2$time, data_new2$subject, data_new2$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

df4 <- data_new3[,-5]
df4

==========================================================================================================================================================================
# Define a function to remove 5% of the y data randomly
  remove_data2 <- function(df) {
    # Loop through each time point
    for (j in 1:n_timepoints) {
      # Subset the data for the current time point
      df_time <- df[df$time == j, ]
      
      # Generate random numbers
      rand <- runif(nrow(df_time))
      
      # Remove y values where rand is less than 0.05
      df_time$y <- ifelse(rand < 0.05, NA, df_time$y)
      
      # Replace the data for the current time point in the data frame
      df[df$time == j, ] <- df_time
    }
    
    # Return the new data frame
    return(df)
  }

# Create a new data frame with 5% of the y data removed randomly at each time point
data_new4 <- remove_data2(data)
data_new4

interaction.plot(data_new4$time, data_new4$subject, data_new4$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Impute missing values in data_new, i.e., data where 5% were removed
imp4 <- mice(data_new4, maxit = 0)
meth4 <- imp4$method
meth4
pred4 <- imp4$predictorMatrix
pred4
pred4[, c('subject', 'time', 'group')] <- 0
imp_mice4 <- mice(data_new4, method = meth4, predictorMatrix = pred4, maxit = 20, seed = 123)
imputed_data4 <- complete(imp_mice4)
imputed_data4

interaction.plot(imputed_data4$time, imputed_data4$subject, imputed_data4$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

# Fit the linear mixed effects model
  model8 <- lmer(y ~ time*group + (1 + time | subject), data=imputed_data4)

summary(model8)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Fit the linear mixed effects model
  model9 <- lmer(y ~ time*group + (1 + time | subject), data=data_new4)

summary(model9)

data_new4$y_pred <- predict(model9, newdata = data_new4, re.form = NA)
data_new4$y[is.na(data_new4$y)] <- data_new4$y_pred[is.na(data_new4$y)]

interaction.plot(data_new4$time, data_new4$subject, data_new4$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

df5<- data_new4[,-5]
df5
------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define a function to remove 10% of the y data randomly
  remove_data3 <- function(df) {
    # Loop through each time point
    for (j in 1:n_timepoints) {
      # Subset the data for the current time point
      df_time <- df[df$time == j, ]
      
      # Generate random numbers
      rand <- runif(nrow(df_time))
      
      # Remove y values where rand is less than 0.1
      df_time$y <- ifelse(rand < 0.1 NA, df_time$y)
      
      # Replace the data for the current time point in the data frame
      df[df$time == j, ] <- df_time
    }
    
    # Return the new data frame
    return(df)
  }

# Create a new data frame with 10of the y data removed randomly at each time point
data_new5 <- remove_data3(data)
data_new5

interaction.plot(data_new5$time, data_new5$subject, data_new5$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Impute missing values in data_new, i.e., data where 10% were removed
imp5 <- mice(data_new5, maxit = 0)
meth5 <- imp5$method
meth5
pred5 <- imp5$predictorMatrix
pred5
pred5[, c('subject', 'time', 'group')] <- 0
imp_mice5 <- mice(data_new5, method = meth5, predictorMatrix = pred5, maxit = 20, seed = 123)
imputed_data5 <- complete(imp_mice5)
imputed_data5

interaction.plot(imputed_data5$time, imputed_data5$subject, imputed_data5$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

# Fit the linear mixed effects model
  model10 <- lmer(y ~ time*group + (1 + time | subject), data=imputed_data5)

summary(model10)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Fit the linear mixed effects model
  model11 <- lmer(y ~ time*group + (1 + time | subject), data=data_new5)

summary(model11)

data_new5$y_pred5 <- predict(model11, newdata = data_new5, re.form = NA)
data_new5$y[is.na(data_new5$y)] <- data_new5$y_pred[is.na(data_new5$y)]

interaction.plot(data_new5$time, data_new5$subject, data_new5$y, xlab="Time", ylab="y", col=c(1:20), legend=F) 

df6<- data_new5[,-5]
df6

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Select 4 random subjects
subjects <- sample(unique(data$subject), 4)

# Extract the data for the selected subjects from the original data frame
data_subset1 <- subset(data, subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
data_subset2 <- subset(imputed_data1, subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
data_subset5<- subset(imputed_data4,subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
data_subset6<- subset(imputed_data5,subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset1,Data = "Original data"), 
  transform(data_subset2, Data = "Imputing 50% missing data"),
  transform(data_subset5, Data = "Imputing 5% missing data"),
  transform(data_subset6, Data = "Imputing 10% missing data")
)

# Plot the value of y over time for the selected subjects
ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Select 4 random subjects
subjects <- sample(unique(data$subject), 4)

# Extract the data for the selected subjects from the original data frame
data_subset1 <- subset(data, subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
data_subset2 <- subset(imputed_data1, subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
data_subset3 <- subset(imputed_data2, subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
data_subset4 <- subset(imputed_data3, subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset, Data = "Original data"), 
  transform(data_subset2, Data = "Imputing 50% missing data"),
  transform(data_subset3, Data = "Imputing timewise dropout data"),
  transform(data_subset4, Data = "Imputing group and timewise dropout data")
)

# Plot the value of y over time for the selected subjects
ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")

===========================================================================================================================================================================

# Select 4 random subjects
subjects <- sample(unique(data$subject), 4)

# Extract the data for the selected subjects from the original data frame
data_subset <- subset(data, subject %in% subjects)

# Extract the data for the selected subjects from the original data frame
df2_subset <- subset(df2, subject %in% subjects)

# Extract the data for the selected subjects from the imputed data frame
data_imputed_subset <- subset(imputed_data1, subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset, Data = "Original"),
  transform(df2_subset, Data = "Fitted"),
  transform(data_imputed_subset, Data = "Imputed")
)

ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")

===========================================================================================================================================================================

# Extract the data for the selected subjects from the original data frame
df3_subset <- subset(df3, subject %in% subjects)

# Extract the data for the selected subjects from the imputed data frame
data_imputed_subset2 <- subset(imputed_data2, subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset, Data = "Original"),
  transform(df3_subset, Data = "Fitted"), 
  transform(data_imputed_subset2, Data = "Imputed")
)

# Plot the value of y over time for the selected subjects
ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")

===========================================================================================================================================================================

# Extract the data for the selected subjects from the original data frame
df4_subset <- subset(df4, subject %in% subjects)

# Extract the data for the selected subjects from the imputed data frame
data_imputed_subset3 <- subset(imputed_data3, subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset, Data = "Original"), 
   transform(df4_subset, Data = "Fitted"),
  transform(data_imputed_subset3, Data = "Imputed")
)

# Plot the value of y over time for the selected subjects
ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")
df5_subset
===========================================================================================================================================================================
# Extract the data for the selected subjects from the original data frame
df5_subset <- subset(df5,subject %in% subjects)

# Extract the data for the selected subjects from the imputed data frame
data_imputed_subset4<- subset(imputed_data4,subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset, Data = "Original"), 
   transform(df5_subset, Data = "Fitted"),
  transform(data_imputed_subset4,Data = "Imputed")
)

# Plot the value of y over time for the selected subjects
ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")

===========================================================================================================================================================================
# Extract the data for the selected subjects from the original data frame
df6_subset <- subset(df6,subject %in% subjects)

# Extract the data for the selected subjects from the imputed data frame
data_imputed_subset5<- subset(imputed_data5,subject %in% subjects)

# Combine the original and imputed data into a single data frame
data_combined <- rbind(
  transform(data_subset, Data = "Original"), 
   transform(df6_subset, Data = "Fitted"),
  transform(data_imputed_subset5,Data = "Imputed")
)

# Plot the value of y over time for the selected subjects
ggplot(data_combined, aes(x = time, y = y, color = Data)) +
  facet_wrap(~ subject, nrow = 2) +
  geom_line(position = position_jitter(width = 0.5)) +  # Add jitter to x-axis values
  xlab("Time") +
  ylab("y")

===========================================================================================================================================================================


# Impute the missing values of data_new using JointAI
JointAIlong <- lme_imp(y ~ time*group + (1 + time | subject),
                       group = 'glmm_logit',
                       no_model = 'time', data = data_new, n.iter = 500, seed = 2020)
traceplot(JointAIlong, use_ggplot = TRUE) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

JointAIlong_extra <- add_samples(JointAIlong, n.iter = 1000)

traceplot(JointAIlong_extra, use_ggplot = TRUE) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

GR_crit(JointAIlong_extra, autoburnin = FALSE)

MC_error(JointAIlong_extra)

===========================================================================================================================================================================
# Impute the missing values of data_new2 using JointAI
JointAIlong2<- lme_imp(y ~ time*group + (1 + time | subject),
                       group = 'glmm_logit',
                       no_model = 'time', data = data_new2, n.iter = 500, seed = 2020)
traceplot(JointAIlong2, use_ggplot = TRUE) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

JointAIlong_extra2 <- add_samples(JointAIlong2, n.iter = 1000)

traceplot(JointAIlong_extra2, use_ggplot = TRUE) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

GR_crit(JointAIlong_extra2, autoburnin = FALSE)

MC_error(JointAIlong_extra2)

===========================================================================================================================================================================
# Impute the missing values of data_new3 using JointAI
JointAIlong3 <- lme_imp(y ~ time*group + (1 + time | subject),
                       group = 'glmm_logit',
                       no_model = 'time', data = data_new3, n.iter = 500, seed = 2020)
traceplot(JointAIlong3, use_ggplot = TRUE) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

JointAIlong_extra3 <- add_samples(JointAIlong3, n.iter = 1000)

traceplot(JointAIlong_extra3, use_ggplot = TRUE) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

GR_crit(JointAIlong_extra3, autoburnin = FALSE)

MC_error(JointAIlong_extra3)

===========================================================================================================================================================================


