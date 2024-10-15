#Install the necessary package
library(ggplot2)
library(MASS)
library(rstudioapi)
library(grf)
library(Metrics)
library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)

#Loading the data
data <- read.csv("C://Users//ua896//OneDrive//桌面//Thesis//real_data.csv")
treated_df <- data[data$rhc==1,]
controled_df <- data[data$rhc==0,]
controled_df <- controled_df[1:3550,]

#####################################################################
#############################Regular Case Analysis###################
#####################################################################
set.seed(111)

#2000 tree regular case
lmf_grf <- causal_forest(X = cbind(as.matrix(data[,3:74])),
                         Y = data[,1],
                     cbind(data[,2]))

tau.hat_grf <- predict(lmf_grf,data[,3:74],estimate.variance = TRUE)
tau.hat_grf <- data.frame(tau.hat_grf)

#4000 tree
lmf_grf_4000 <- causal_forest(X = cbind(as.matrix(data[,3:74])),
                         Y = data[,1],
                         cbind(data[,2]),num.trees = 4000)

tau.hat_grf_4000 <- predict(lmf_grf_4000,data[,3:74],estimate.variance = TRUE)
tau.hat_grf_4000 <- data.frame(tau.hat_grf_4000)

#Honest Fraction Tree
lmf_grf_honest <- causal_forest(X = cbind(as.matrix(data[,3:74])),
                              Y = data[,1],
                              cbind(data[,2]),honesty.fraction = 0.2)

tau.hat_grf_honest <- predict(lmf_grf_honest,data[,3:74],estimate.variance = TRUE)
tau.hat_grf_honest <- data.frame(tau.hat_grf_honest)





#Doubly Robust estimates of average treatment effects
result_2000 <- round(100*average_treatment_effect(lmf_grf,target.sample="overlap"),3)
result_4000 <- round(100*average_treatment_effect(lmf_grf_4000,target.sample="overlap"),3)
result_honest <- round(100*average_treatment_effect(lmf_grf_honest,target.sample="overlap"),3)




#Plot the tree for regular forest
tree <- get_tree(lmf_grf, 10)
tree.plot = plot(tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//plot_2000_tree.svg")

#Plot the tree for 4000 tree forest
tree <- get_tree(lmf_grf_4000, 10)
tree.plot = plot(tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//plot_4000_tree.svg")

#Plot different fraction honest tree
tree <- get_tree(lmf_grf_honest, 10)
tree.plot = plot(tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//plot_honest_tree.svg")


#Plot the heterogeneous treatment effects
hist_2000 <- hist(nclass = 30,tau.hat_grf[,1],main = "Heterogenerous Treatment Effects", 
     xlab = "Heterogenerous Treatment Effects", 
     col = "lightgreen")

hist_4000 <- hist(nclass = 30,tau.hat_grf_4000[,1],main = "Heterogenerous Treatment Effects", 
               xlab = "Heterogenerous Treatment Effects", 
               col = "lightblue")

hist_honest <- hist(nclass = 30,tau.hat_grf_honest[,1],main = "Heterogenerous Treatment Effects", 
                  xlab = "Heterogenerous Treatment Effects", 
                  col = "lightblue")

#2000 v.s. 4000 plot
plot(hist_4000,col = rgb(0.5, 0.5, 1, 0.5), main = "4000 Tree V.S. 2000 Tree", xlab = "Treatment Effect", ylab = "Frequency")#Blue
plot(hist_2000,  col = rgb(0.5, 1, 0.5, 0.5), add = TRUE)#green

#2000 v.s. honest plot
plot(hist_honest,col = rgb(0.5, 0.5, 1, 0.5), main = "2000 Tree V.S. 0.2 Fraction Honest", xlab = "Treatment Effect", ylab = "Frequency")#Blue
plot(hist_2000,  col = rgb(0.5, 1, 0.5, 0.5), add = TRUE)#green


#Calculating the RATE,Preparing the balanced data
train_treat_sample <- sample(1:2184,1092)
train_treat_data <- treated_df[train_treat_sample,]
test_treat_data <- treated_df[-train_treat_sample,]
train_control_sample <- sample(1:3551,1775)
train_control_data <- controled_df[train_control_sample,]
test_control_data <- controled_df[-train_control_sample,]

train <- rbind(train_treat_data,train_control_data)
test <- rbind(test_treat_data,test_control_data)

#For 2000 tree result
train.forest <- causal_forest(X = cbind(as.matrix(train[,3:74])), train[,1], train[,2])
eval.forest <- causal_forest(cbind(as.matrix(test[,3:74])), test[,1], test[,2])
rate <- rank_average_treatment_effect(eval.forest,
                                      predict(train.forest, test[,3:74])$predictions)
plot(rate)

#For 4000 tree result
train.forest_4000 <- causal_forest(cbind(as.matrix(train[,3:74])), train[,1], train[,2],num.trees = 4000)
eval.forest_4000 <- causal_forest(cbind(as.matrix(test[,3:74])), test[,1], test[,2],num.trees = 4000)
rate_4000 <- rank_average_treatment_effect(eval.forest_4000,
                                      predict(train.forest_4000, test[,3:74])$predictions)
plot(rate_4000)

#For honest tree result
train.forest_honest <- causal_forest(X = cbind(as.matrix(train[,3:74])), train[,1], train[,2],honesty.fraction = 0.2)
eval.forest_honest <- causal_forest(cbind(as.matrix(test[,3:74])), test[,1], test[,2],honesty.fraction = 0.2)
rate_honest <- rank_average_treatment_effect(eval.forest_honest,
                                           predict(train.forest_honest, test[,3:74])$predictions)
plot(rate_honest)

#Plotting together
par(mfrow = c(3, 1),mar = c(2, 1, 1, 1))
plot(rate, xlab = "Treated fraction", main = "TOC evaluated from 2000 Tree Model")
plot(rate_4000, xlab = "Treated fraction", main = "TOC evaluated from 4000 Tree Mdoel")
plot(rate_honest, xlab = "Treated fraction", main = "TOC evaluated from 0.2 Honest Tree Model")

#Calculate the P-Value
test_statistic <- rate$estimate/rate$std.err
p_value <- 2 * (1 - pnorm(abs(test_statistic)))

#Calculate the P-Value
test_statistic <- rate_4000$estimate/rate_4000$std.err
p_value <- 2 * (1 - pnorm(abs(test_statistic)))

#Calculate the P-Value
test_statistic <- rate_honest$estimate/rate_honest$std.err
p_value <- 2 * (1 - pnorm(abs(test_statistic)))
####################################################################################
#########################################RATE for Second Stage Simulation##################
####################################################################################

data <- read.csv("C://Users//ua896//Downloads//generated_data.csv")
treated_df <- data[data$rhc==1,]
controled_df <- data[data$rhc==0,]

ncol(treated_df)
nrow(controled_df)

data_col <- rep(0,10)
for(i in 1:10){
  #train <- sample(1:5735, 2687)
  #train.forest <- causal_forest(X = cbind(as.matrix(data[train,3:74])), data[train,1], data[train,2])
  #eval.forest <- causal_forest(cbind(as.matrix(data[-train,3:74])), data[-train,1], data[-train,2])
  
  #rate <- rank_average_treatment_effect(eval.forest,predict(train.forest, data[-train,3:74])$predictions)
  treat_df <- treated_df%>%
    slice_sample(n=4000)%>%
    mutate(row_number = row_number())%>%
    select(-row_number)
  control_df <- controled_df%>%
    slice_sample(n=4000)%>%
    mutate(row_number = row_number())%>%
    select(-row_number)
  processed_df <- rbind(treat_df,control_df)
  data <- processed_df %>% 
    select(-source)
  
  train_sample <- sample(1:8000,4000)
  
  train.forest <- causal_forest(cbind(as.matrix(data[train_sample,2:72],data[train_sample,74])),
                                data[train_sample,1],
                                data[train_sample,73])
  eval.forest <- causal_forest(cbind(as.matrix(data[-train_sample,2:72],data[-train_sample,74])),
                               data[-train_sample,1],
                               cbind(data[-train_sample,73]))
  rate <- rank_average_treatment_effect(eval.forest,
                                        predict(train.forest, cbind(as.matrix(data[-train_sample,2:72],data[-train_sample,74])))[,1])
  plot(rate)
  #Calculate the P-Value
  
  test_statistic <- rate$estimate/rate$std.err
  p_value <- 2 * (1 - pnorm(abs(test_statistic)))
  print(p_value)
  data_col[i] <- p_value < 0.1
}






