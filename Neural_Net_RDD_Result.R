#Install the necessary package
library(ggplot2)
library(MASS)
library(rstudioapi)
library(grf)
library(Metrics)
library(dplyr)

#setting up the path
script_path <- getActiveDocumentContext()$path
script_dir <- dirname(script_path)
setwd(script_dir)
getwd()

#Loading the data and pre-procssing some variables
data <- read.csv("C://Users//ua896//OneDrive//桌面//Thesis//Quantile_RDD//Result//quantile_rdd.csv")
data <- subset(data, select = -c( X))
ncol(data)

#The test data is also based on 25, 50, 75 quantile of the variables
data_test <- read.csv("C://Users//ua896//OneDrive//桌面//Thesis//Quantile_RDD//Result//quantile_rdd_test.csv")
data_test <- subset(data_test, select = -c( X))
ncol(data_test)

data <- data[, c(setdiff(names(data), "Treatment"), "Treatment")]
data_test <- data_test[, c(setdiff(names(data_test), "Treatment"), "Treatment")]
data_test['Treatment'] = 0

desired_order <- colnames(data_test)
data <- data[, desired_order]
data_test['real_treat'] = data_test['mort_cf'] - data_test['mort_age59_related_postHS']

treated_df <- data[data$Treatment==1,]
controled_df <- data[data$Treatment==0,]


#Creating Empty Matrix For Collecting The Result
tau_linear <- matrix(0,500,3)
tau_poly <- matrix(0,500,3)
tau_complex <- matrix(0,500,3)

meansd_linear <- matrix(0,500,3)
meansd_poly <- matrix(0,500,3)
meansd_complex <- matrix(0,500,3)

contains_true_mean_linear <- matrix(0,500,3)
contains_true_mean_poly <- matrix(0,500,3)
contains_true_mean_complex <- matrix(0,500,3)


#set.seed
set.seed(111)

for (i in 1:500){
  for (a in 1:3){
    print(i)
    
    #Sampling the data from Dataset
    treat_df <- treated_df%>%
      slice_sample(n=100)
    control_df <- controled_df%>%
      slice_sample(n=900)
    processed_df <- rbind(treat_df,control_df)
  
    #Training the RDD models on GRF, the min_node_size can be changed 
    #and the fraction of honest tree can also be changed according to the scenario
    Linear_model <- lm_forest(cbind(as.matrix(processed_df[,3:61])),
                         processed_df[,1],
                         cbind(processed_df[,63],processed_df[,2])
                         ,min.node.size = 15)
    Poly_model <- lm_forest(cbind(as.matrix(processed_df[,3:61])),
                            processed_df[,1],
                            cbind(processed_df[,63],processed_df[,2],processed_df[,2]^2)
                            ,min.node.size = 15)
    
    Complex_model <- lm_forest(cbind(as.matrix(processed_df[,3:61])),
                          processed_df[,1],
                          cbind(processed_df[,63],processed_df[,2],processed_df[,2]^2
                          ,processed_df[,2]*processed_df[,63],
                          processed_df[,2]^2*processed_df[,63])
                          ,min.node.size = 15)
    
    #Testing different quantile result
    test_set <- data_test[a,]
    
    #Predict the result at different quantile point
    tau.hat_Linear <- predict(Linear_model,test_set[,3:61],estimate.variance = TRUE)
    sigma.hat_linear<- sqrt(tau.hat_Linear$variance.estimates)
    tau.hat_Linear <- data.frame(tau.hat_Linear)
    upper_linear <- tau.hat_Linear[,1]+1.96*sigma.hat_linear[,1]
    lower_linear <- tau.hat_Linear[,1]-1.96*sigma.hat_linear[,1]
    contains_true_mean_linear[i,a] <- lower_linear <= test_set[,64] && upper_linear >= test_set[,64]
  
    
    tau.hat_poly <- predict(Poly_model,test_set[,3:61],estimate.variance = TRUE)
    sigma.hat_poly <- sqrt(tau.hat_poly$variance.estimates)
    tau.hat_poly <- data.frame(tau.hat_poly)
    upper_poly <- tau.hat_poly[,1]+1.96*sigma.hat_poly[,1]
    lower_poly <- tau.hat_poly[,1]-1.96*sigma.hat_poly[,1]
    contains_true_mean_poly[i,a] <- lower_poly <= test_set[,64] && upper_poly >= test_set[,64]
  
    tau.hat_complex <- predict(Complex_model,test_set[,3:61],estimate.variance = TRUE)
    sigma.hat_complex<- sqrt(tau.hat_complex$variance.estimates)
    tau.hat_complex <- data.frame(tau.hat_complex)
    upper_complex <- tau.hat_complex[,1]+1.96*sigma.hat_complex[,1]
    lower_complex <- tau.hat_complex[,1]-1.96*sigma.hat_complex[,1]
    contains_true_mean_complex[i,a] <- lower_complex <= test_set[,64] && upper_complex >= test_set[,64]
    
    #Collecting the prediction result
    tau_linear[i,a] = tau.hat_Linear[,1]
    tau_poly[i,a] =  tau.hat_poly[,1]
    tau_complex[i,a] = tau.hat_complex[,1]
    
    #Collecting the meansd result
    meansd_linear[i,a] = sigma.hat_linear[,1]
    meansd_poly[i,a] = sigma.hat_poly[,1]
    meansd_complex[i,a] = sigma.hat_complex[,1]

  }
}


#Plot the Result For linear, poly and complex models
bias_median = tau_linear[,1]-data_test[1,64]
df_median = data.frame(bias_median)
df_median['high_eqantile'] = tau_linear[,2]-data_test[2,64]
df_median['low_eqantile'] = tau_linear[,3]-data_test[3,64]

df_median$Index <- seq_along(df_median$bias_median)


plot <- ggplot(df_median, aes(x = Index)) +
  geom_point(aes(y = bias_median, color = "Median"), size = 0.7) +  
  geom_point(aes(y = high_eqantile, color = "75% Quantile"), size = 0.7) +  
  geom_point(aes(y = low_eqantile, color = "25% Quantile"), size = 0.7) + 
  labs(title = "Bias of Linear Model in GRF",
       x = "Iteration Times",
       y = "Bias") +
  theme_minimal()+
  ylim(-6.5,9)
print(plot)
ggsave("bias_linear.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")
#Plot the poly
#Plot the Result

df_median['poly_median'] = tau_poly[,1]-data_test[1,64]
df_median['poly_high_eqantile'] = tau_poly[,2]-data_test[2,64]
df_median['poly_low_eqantile'] = tau_poly[,3]-data_test[3,64]


plot <- ggplot(df_median, aes(x = Index)) +
  geom_point(aes(y = poly_median, color = "Median"), size = 0.7) +  # Apply size to the bias_median points
  geom_point(aes(y = poly_high_eqantile, color = "75% Quantile"), size = 0.7) +  # Apply size to high_quantile points
  geom_point(aes(y = poly_low_eqantile, color = "25% Quantile"), size = 0.7) +  # Apply size to low_quantile points
  labs(title = "Bias of Polynomial Model in GRF",
       x = "Iteration Times",
       y = "Bias") +
  theme_minimal()+
  ylim(-6.5,9)
print(plot)


ggsave("bias_poly.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")


df_median['complex_median'] = tau_complex[,1]-data_test[1,64]
df_median['complex_high_eqantile'] = tau_complex[,2]-data_test[2,64]
df_median['complex_low_eqantile'] = tau_complex[,3]-data_test[3,64]


plot <- ggplot(df_median, aes(x = Index)) +
  geom_point(aes(y = complex_median, color = "Median"), size = 0.7) +  # Apply size to the bias_median points
  geom_point(aes(y = complex_high_eqantile, color = "75% Quantile"), size = 0.7) +  # Apply size to high_quantile points
  geom_point(aes(y = complex_low_eqantile, color = "25% Quantile"), size = 0.7) +  # Apply size to low_quantile points
  labs(title = "Bias of Poly Model in GRF",
       x = "Iteration Times",
       y = "Bias") +
  theme_minimal()
print(plot)
  

#write.csv(tau_linear, file = "tau_linear_neural_pruned.csv", row.names = FALSE)
#write.csv(tau_poly, file = "tau_poly_neural_pruned.csv", row.names = FALSE)
#write.csv(tau_complex, file = "tau_complex_neural_pruned.csv", row.names = FALSE)
#write.csv(contains_true_mean_linear, file = "linear_ci_pruned.csv", row.names = FALSE)
#write.csv(contains_true_mean_poly, file = "poly_ci_pruned.csv", row.names = FALSE)
#write.csv(contains_true_mean_complex, file = "complex_ci_pruned.csv", row.names = FALSE)
#write.csv(meansd_linear, file = "meansd_linear_neural_pruned.csv", row.names = FALSE)
#write.csv(meansd_poly, file = "meansd_poly_neural_pruned.csv", row.names = FALSE)
#write.csv(meansd_complex, file = "meansd_complex_neural_pruned.csv", row.names = FALSE)




