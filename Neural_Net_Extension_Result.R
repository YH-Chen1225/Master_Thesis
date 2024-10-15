#Install The Necessary Package
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
data <- read.csv("C://Users//ua896//OneDrive//桌面//Thesis//Quantile_RDD//Result//quantile_rdd_extension.csv")

#The test data is also based on 25, 50, 75 quantile of the variables
data_test <- read.csv("C://Users//ua896//OneDrive//桌面//Thesis//Quantile_RDD//Result//quantile_rdd_test_extension_second.csv")

desired_order <- colnames(data_test)
data <- data[, desired_order]

data_test['real_treat'] = 0
for (i in 1:3){
  if(data_test[i,"rhc"]==1){
    data_test[i,"real_treat"] = data_test[i,"dth30"]-data_test[i,"dth30_cf"]
  }
  else{
    data_test[i,"real_treat"] = data_test[i,"dth30_cf"] - data_test[i,"dth30"]
  }
}
treated_df <- data[data$rhc==1,]
controled_df <- data[data$rhc==0,]

#Creating Empty Matrix For Collecting The Result
tau_grf <- matrix(0,500,3)
tau_large <- matrix(0,500,3)
tau_honest <- matrix(0,500,3)
meansd_grf <- matrix(0,500,3)
meansd_large <- matrix(0,500,3)
meansd_honest <- matrix(0,500,3)
contains_true_mean_grf <- matrix(0,500,3)
contains_true_mean_large <- matrix(0,500,3)
contains_true_mean_honest <- matrix(0,500,3)

#set.seed
set.seed(111)

for (i in 1:500){
  for (a in 1:3){
    print(i)
    #Sampling the data from Dataset
    treat_df <- treated_df%>%
      slice_sample(n=500)%>%
      mutate(row_number = row_number())%>%
      select(-row_number)
    control_df <- controled_df%>%
      slice_sample(n=500)%>%
      mutate(row_number = row_number())%>%
      select(-row_number)
    processed_df <- rbind(treat_df,control_df)
    
    #Testing different quantile result
    test_df = data_test[a,] 
    
    #Predict the result at different quantile point
    lmf_grf <- lm_forest(cbind(as.matrix(processed_df[,3:74])),
                                processed_df[,1],
                                processed_df[,2])
    tau.hat_grf <- predict(lmf_grf,test_df[,3:74],estimate.variance = TRUE)
    sigma.hat_grf <- sqrt(tau.hat_grf$variance.estimates)
    tau.hat_grf <- data.frame(tau.hat_grf)
    upper_grf <- tau.hat_grf[,1]+1.96*sigma.hat_grf[,1]
    lower_grf <- tau.hat_grf[,1]-1.96*sigma.hat_grf[,1]
    
    real_treat <- data_test[,76]
  
    contains_true_mean_grf[i,a] <- lower_grf <= real_treat && upper_grf >= real_treat
    
    #Change the honest tree fraction
    lmf_grf_honest <- lm_forest(cbind(as.matrix(processed_df[,3:74])),
                                processed_df[,1],
                                processed_df[,2],honesty.fraction = 0.2)
    tau.hat_grf_honest <- predict(lmf_grf_honest,test_df[,3:74],estimate.variance = TRUE)
    sigma.hat_grf_honest <- sqrt(tau.hat_grf_honest$variance.estimates)
    tau.hat_grf_honest <- data.frame(tau.hat_grf_honest)
    upper_grf_honest <- tau.hat_grf_honest[,1]+1.96*sigma.hat_grf_honest[,1]
    lower_grf_honest <- tau.hat_grf_honest[,1]-1.96*sigma.hat_grf_honest[,1]
    
    contains_true_mean_honest[i,a] <- lower_grf_honest <= real_treat && upper_grf_honest >= real_treat
    
    
    #4000 tree
    lmf_grf_tree <- lm_forest(cbind(as.matrix(processed_df[,3:74])),
                              processed_df[,1],
                              processed_df[,2],num.trees = 4000)
    tau.hat_grf_tree <- predict(lmf_grf_tree,test_df[,3:74],estimate.variance = TRUE)
    sigma.hat_grf_tree <- sqrt(tau.hat_grf_tree$variance.estimates)
    tau.hat_grf_tree <- data.frame(tau.hat_grf_tree)
    upper_grf_tree <- tau.hat_grf_tree[,1]+1.96*sigma.hat_grf_tree[,1]
    lower_grf_tree <- tau.hat_grf_tree[,1]-1.96*sigma.hat_grf_tree[,1]
    
    contains_true_mean_large[i,a] <- lower_grf_tree <= real_treat && upper_grf_tree >= real_treat
    
    tau_grf[i,a] = tau.hat_grf[,1]
    tau_honest[i,a] = tau.hat_grf_honest[,1]
    tau_large[i,a] = tau.hat_grf_tree[,1]
  
    
    meansd_grf[i,a] = sigma.hat_grf[,1]
    meansd_honest[i,a] = sigma.hat_grf_honest[,1]
    meansd_large[i,a] = sigma.hat_grf_tree[,1]
  }
}

for (i in 1:3){
  print(sqrt(mean((tau_honest[,i]-data_test[i,76])^2)))
}

#upper = tau_large[,3]+1.96*meansd_large[,3]
#lower = tau_large[,3]-1.96*meansd_large[,3]
#temp = rep(0,500)
#for (i in 1:500){
  #temp[i] <- data_test[3,76]<= upper[i] && data_test[3,76]>=lower[i]
#}
#mean(temp)

#data_test
#for (i in 1:3){
  #print(mean(meansd_large[,i]))
#}


#Plot the Result For defualt GRF, Honest GRF, 4000 Tree GRF
df_bias = tau_grf[,1]-data_test[1,76]
df_bias = data.frame(df_bias)
df_bias['high_eqantile'] = tau_grf[,2]-data_test[2,76]
df_bias['low_eqantile'] = tau_grf[,3]-data_test[3,76]

df_bias$Index <- seq_along(df_bias$df_bias)


plot <- ggplot(df_bias, aes(x = Index)) +
  geom_point(aes(y = df_bias, color = "Median"), size = 0.7) +  # Apply size to the bias_median points
  geom_point(aes(y = high_eqantile, color = "75% Quantile"), size = 0.7) +  # Apply size to high_quantile points
  geom_point(aes(y = low_eqantile, color = "25% Quantile"), size = 0.7) +  # Apply size to low_quantile points
  labs(title = "Bias of Regular Forest",
       x = "Iteration Times",
       y = "Bias") +
  theme_minimal()
print(plot)
ggsave("bias_linear.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")



#write.csv(tau_grf, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Neural//Typical//tau_grf.csv", row.names = FALSE)
#write.csv(tau_honest, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Neural//Typical//tau_honest.csv", row.names = FALSE)
#write.csv(tau_large, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Neural//Typical//tau_large.csv", row.names = FALSE)
#write.csv(meansd_grf, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Neural//Typical//meansd_grf.csv", row.names = FALSE)
#write.csv(meansd_honest, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Neural//Typical//meansd_honest.csv", row.names = FALSE)
#write.csv(meansd_large, file = "C://Users//ua896//OneDrive//桌面//Thesis//RDD_Neural//Typical//meansd_large.csv", row.names = FALSE)


