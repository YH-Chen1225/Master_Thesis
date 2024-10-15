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
rmse_linear_nv <- matrix(0 , nrow = 500, ncol = 4)
bias_linear_nv <- matrix(0 , nrow = 500, ncol = 4)
mad_linear_nv <- matrix(0 , nrow = 500, ncol = 4)
meansd_linear_nv <- matrix(0 , nrow = 500, ncol = 4)
ci_linear_nv <- matrix(0 , nrow = 500, ncol = 4)

rmse_poly_nv <- matrix(0 , nrow = 500, ncol = 4)
bias_poly_nv <- matrix(0 , nrow = 500, ncol = 4)
mad_poly_nv <- matrix(0 , nrow = 500, ncol = 4)
meansd_poly_nv <- matrix(0 , nrow = 500, ncol = 4)
ci_poly_nv <- matrix(0 , nrow = 500, ncol = 4)

rmse_complex_nv <- matrix(0 , nrow = 500, ncol = 4)
bias_complex_nv <- matrix(0 , nrow = 500, ncol = 4)
mad_complex_nv <- matrix(0 , nrow = 500, ncol = 4)
meansd_complex_nv <- matrix(0 , nrow = 500, ncol = 4)
ci_complex_nv <- matrix(0 , nrow = 500, ncol = 4)

df_first <- Medium_dgp(1000,1,0)
df_second <- Medium_dgp(1000,1.5,0)
df_third <- Medium_dgp(1000,2,0)
df_fourth <- Medium_dgp(1000,2.5,0)
df_test <- list(df_first,df_second,df_third,df_fourth)

Noise = c(1,1.5,2,2.5)

#################################################################################
######################DGP will change according to the context###################
####################Real Treatment will also change according the context########
#################################################################################

for (i in 1:500){
  a <- 1
  print(i)
  for (noise in Noise){
    print(noise)
    
    #Selecting the testing Data Generating Process
    df <- Complex_dgp(1000,noise,0)
    
    contains_true_mean_poly <- logical(1000)
    contains_true_mean_linear <- logical(1000)
    contains_true_mean_complex <- logical(1000)
    
    #Training different RDD models on GRF
    Complex_model <- lm_forest(cbind(as.matrix(df[,4:ncol(df)])),
                               df[,1],
                               cbind(as.matrix(df[,2:3]),df[,3]^2,df[,2]*df[,3],df[,2]*df[,3]^2))
    Poly_model <- lm_forest(cbind(as.matrix(df[,4:ncol(df)])),
                            df[,1],
                            cbind(as.matrix(df[,2:3]),df[,3]^2))
    Linear_model <- lm_forest(cbind(as.matrix(df[,4:ncol(df)])),
                              df[,1],
                              cbind(as.matrix(df[,2:3])))
    test_df <- df_test[[a]]
    real_treat <- 1 + test_df[,4] + test_df[,5] + test_df[,4]^2 + test_df[,5]^2
    
    #Using Different RDD Models For Prediction
    tau.hat_Linear <- predict(Linear_model,cbind(as.matrix(test_df[,4:ncol(test_df)])),estimate.variance = TRUE)
    sigma.hat_Linear <- sqrt(tau.hat_Linear$variance.estimates)
    tau.hat_Linear <- data.frame(tau.hat_Linear)
    upper_linear<- tau.hat_Linear[,1]+1.96*sigma.hat_Linear[,1]
    lower_linear <- tau.hat_Linear[,1]-1.96*sigma.hat_Linear[,1]
    for (k in 1:1000){
      contains_true_mean_linear[k] <- lower_linear[k] <= real_treat[k] && upper_linear[k] >= real_treat[k]
    }
    
    tau.hat_Poly <- predict(Poly_model,cbind(as.matrix(test_df[,4:ncol(test_df)])),estimate.variance = TRUE)
    sigma.hat_Poly <- sqrt(tau.hat_Poly$variance.estimates)
    tau.hat_Poly <- data.frame(tau.hat_Poly)
    upper_Poly<- tau.hat_Poly[,1]+1.96*sigma.hat_Poly[,1]
    lower_Poly <- tau.hat_Poly[,1]-1.96*sigma.hat_Poly[,1]
    for (k in 1:1000){
      contains_true_mean_poly[k] <- lower_Poly[k] <= real_treat[k] && upper_Poly[k] >= real_treat[k]
    }
    
    tau.hat_Complex <- predict(Complex_model,cbind(as.matrix(test_df[,4:ncol(test_df)])),estimate.variance = TRUE)
    sigma.hat_Complex <- sqrt(tau.hat_Complex$variance.estimates)
    tau.hat_Complex <- data.frame(tau.hat_Complex)
    upper_complex<- tau.hat_Complex[,1]+1.96*sigma.hat_Complex[,1]
    lower_complex <- tau.hat_Complex[,1]-1.96*sigma.hat_Complex[,1]
    for (k in 1:1000){
      contains_true_mean_complex[k] <- lower_complex[k] <= real_treat[k] && upper_complex[k] >= real_treat[k]
    }
    #Collecting the RMSE Result
    rmse_linear_nv[i,a] <- rmse(actual = real_treat, predicted = tau.hat_Linear[,1])
    rmse_poly_nv[i,a] <- rmse(actual = real_treat, predicted = tau.hat_Poly[,1])
    rmse_complex_nv[i,a] <- rmse(actual = real_treat, predicted = tau.hat_Complex[,1])
    
    #Collecting the bias Result
    bias_linear_nv[i,a] <- bias(actual = real_treat, predicted = tau.hat_Linear[,1])
    bias_poly_nv[i,a] <- bias(actual = real_treat, predicted = tau.hat_Poly[,1])
    bias_complex_nv[i,a] <- bias(actual = real_treat, predicted = tau.hat_Complex[,1])
    
    #Collecting the meansd Result
    meansd_linear_nv[i,a] <- mean(sigma.hat_Linear[,1])
    meansd_poly_nv[i,a] <- mean(sigma.hat_Poly[,1])
    meansd_complex_nv[i,a] <- mean(sigma.hat_Complex[,1])
    
    #Collecting the CI Result
    ci_linear_nv[i,a] <- mean(contains_true_mean_linear)
    ci_poly_nv[i,a] <- mean(contains_true_mean_poly)
    ci_complex_nv[i,a] <- mean(contains_true_mean_complex)
    a = a + 1
  }
}

#Store the result
#write.csv(rmse_linear_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//rmse_linear_nv_complex.csv", row.names = FALSE)
#write.csv(rmse_poly_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//rmse_poly_nv_complex.csv", row.names = FALSE)
#write.csv(rmse_complex_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//rmse_complex_nv_complex.csv", row.names = FALSE)
#write.csv(bias_linear_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//bias_linear_nv_complex.csv", row.names = FALSE)
#write.csv(bias_poly_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//bias_poly_nv_complex.csv", row.names = FALSE)
#write.csv(bias_complex_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//bias_complex_nv_complex.csv", row.names = FALSE)
#write.csv(meansd_linear_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//meansd_linear_nv_complex.csv", row.names = FALSE)
#write.csv(meansd_poly_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//meansd_poly_nv_complex.csv", row.names = FALSE)
#write.csv(meansd_complex_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//meansd_complex_nv_complex.csv", row.names = FALSE)
#write.csv(ci_linear_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//ci_linear_nv_complex.csv", row.names = FALSE)
#write.csv(ci_poly_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//ci_poly_nv_complex.csv", row.names = FALSE)
#write.csv(ci_complex_nv, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Final//ci_complex_nv_complex.csv", row.names = FALSE)









