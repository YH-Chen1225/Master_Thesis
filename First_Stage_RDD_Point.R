#Install the necessary package
library(ggplot2)
library(MASS)
library(rstudioapi)
library(grf)
library(Metrics)

#Setup the path
script_path <- getActiveDocumentContext()$path
script_dir <- dirname(script_path)
setwd(script_dir)
getwd()
source("DGP_RDD.R")

#Creating Empty Matrix For Collecting The Result
tau_linear <- matrix(0 , nrow = 500, ncol = 4)
tau_poly <- matrix(0 , nrow = 500, ncol = 4)
tau_complex <- matrix(0 , nrow = 500, ncol = 4)
meansd_linear_nv <- matrix(0 , nrow = 500, ncol = 4)
meansd_poly_nv <- matrix(0 , nrow = 500, ncol = 4)
meansd_complex_nv <- matrix(0 , nrow = 500, ncol = 4)
contains_true_mean_linear <- matrix(0 , nrow = 500, ncol = 4)
contains_true_mean_poly <- matrix(0 , nrow = 500, ncol = 4)
contains_true_mean_complex <- matrix(0 , nrow = 500, ncol = 4)


#The Simple DGP will always be used in the point estimating phase
#However, various point will be tested, -1,-0.5,0,0.5,1
#Noise, Sample Size, Noise Variable will again be tested
#Noise = 1,1.5,2,2.5
#Sample Size = 1000,2000,3000,4000
#NV = 0,5,15,25

#Here is an example of testing Sample size, but it can be changed according to the scenario

N = c(1000,2000,3000,4000)

for (i in 1:500){
  a <- 1
  print(i)
  for (n in N){
    print(n)
    
    #Sample Size Change According to n
    df <- Simple_dgp(n,noise = 1,noise_var = 0)
    
    #The Treatment matrix, can be changed according to the testing scenario, now the example is zero
    treat_cov <- matrix(0, nrow = 1, ncol = 2)
    
    #Train the RDD model on GRF
    Complex_model <- lm_forest(cbind(as.matrix(df[,4:ncol(df)])),
                               df[,1],
                               cbind(as.matrix(df[,2:3]),df[,3]^2,df[,2]*df[,3],df[,2]*df[,3]^2)
                               ,min.node.size = 50
                               ,stabilize.splits = TRUE)
    Poly_model <- lm_forest(cbind(as.matrix(df[,4:ncol(df)])),
                            df[,1],
                            cbind(as.matrix(df[,2:3]),df[,3]^2) 
                            ,min.node.size = 50
                            ,stabilize.splits = TRUE)
    Linear_model <- lm_forest(cbind(as.matrix(df[,4:ncol(df)])),
                              df[,1],
                              cbind(as.matrix(df[,2:3]))
                              ,min.node.size = 50
                              ,stabilize.splits = TRUE)
    real_treat <- 0
    
    #Predict the result at point zero
    tau.hat_Linear <- predict(Linear_model,treat_cov,estimate.variance = TRUE)
    sigma.hat_Linear <- sqrt(tau.hat_Linear$variance.estimates)
    tau.hat_Linear <- data.frame(tau.hat_Linear)
    upper_linear<- tau.hat_Linear[,1]+1.96*sigma.hat_Linear[,1]
    lower_linear <- tau.hat_Linear[,1]-1.96*sigma.hat_Linear[,1]
    contains_true_mean_linear[i,a] <- lower_linear <= real_treat && upper_linear >= real_treat
    
    
    tau.hat_Poly <- predict(Poly_model,treat_cov,estimate.variance = TRUE)
    sigma.hat_Poly <- sqrt(tau.hat_Poly$variance.estimates)
    tau.hat_Poly <- data.frame(tau.hat_Poly)
    upper_Poly<- tau.hat_Poly[,1]+1.96*sigma.hat_Poly[,1]
    lower_Poly <- tau.hat_Poly[,1]-1.96*sigma.hat_Poly[,1]
    contains_true_mean_poly[i,a] <- lower_Poly <= real_treat && upper_Poly >= real_treat
    
    tau.hat_Complex <- predict(Complex_model,treat_cov,estimate.variance = TRUE)
    sigma.hat_Complex <- sqrt(tau.hat_Complex$variance.estimates)
    tau.hat_Complex <- data.frame(tau.hat_Complex)
    upper_complex<- tau.hat_Complex[,1]+1.96*sigma.hat_Complex[,1]
    lower_complex <- tau.hat_Complex[,1]-1.96*sigma.hat_Complex[,1]
    contains_true_mean_complex[i,a] <- lower_complex <= real_treat && upper_complex >= real_treat
    
    #Collecting the meansd data
    meansd_linear_nv[i,a] <- mean(sigma.hat_Linear[,1])
    meansd_poly_nv[i,a] <- mean(sigma.hat_Poly[,1])
    meansd_complex_nv[i,a] <- mean(sigma.hat_Complex[,1])
    
    #Collecting the Prediction Result
    tau_linear[i,a] <- tau.hat_Linear[,1]
    tau_poly[i,a] <- tau.hat_Poly[,1]
    tau_complex[i,a] <- tau.hat_Complex[,1]
    a = a + 1
  }
}

#tau_linear[1,]
#for (i in 1:4){
  #print(rmse(actual = 0, predicted = tau_complex[1:500,i]))
#}

#for (i in 1:4){
  #print(mean(contains_true_mean_complex[1:500,i]))
#}

#write.csv(tau_linear, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//rmse_linear_n_prune-0.5.csv", row.names = FALSE)
#write.csv(tau_poly, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//rmse_poly_n_prune-0.5.csv", row.names = FALSE)
#write.csv(tau_complex, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//rmse_complex_n_prune-0.5.csv", row.names = FALSE)
#write.csv(meansd_linear_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//meansd_linear_n_prune-0.5.csv", row.names = FALSE)
#write.csv(meansd_poly_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//meansd_poly_n_prune-0.5.csv", row.names = FALSE)
#write.csv(meansd_complex_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//meansd_complex_n_prune-0.5.csv", row.names = FALSE)
#write.csv(contains_true_mean_linear, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//ci_linear_n_prune-0.5.csv", row.names = FALSE)
#write.csv(contains_true_mean_poly, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//ci_poly_n_prune-0.5.csv", row.names = FALSE)
#write.csv(contains_true_mean_complex, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Justification//ci_complex_n_prune-0.5.csv", row.names = FALSE)
#write.csv(tau_linear, file = "D://Moses//Quantile_RDD//Justification//complex-1.csv", row.names = FALSE)


