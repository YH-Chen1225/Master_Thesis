#Install the necessary package
library(ggplot2)
library(MASS)
library(rstudioapi)
library(grf)
library(Metrics)
library(DiagrammeR)
library(DiagrammeRsvg)
library(dplyr)
library(fastDummies)
library(rdd)


#Loading the data and pre-processing the data
data <- read.csv("C://Users//ua896//OneDrive//桌面//Thesis//CTV_2017_JPAM-master//heartstart_several_var.csv")#C://Users//ua896//OneDrive//桌面//Thesis//CTV_2017_JPAM-master//heartstart_clean.csv

data <- dummy_cols(data, select_columns = "state")
data <- subset(data, select = -c(X,oldcode,state))

########################################################################################
###############Min node size or Unbalance Restriction Both are tested Here##############
########################################################################################

#Training the RDD models on GRF with min_node_size = 25 pruning
linear_model <- lm_forest(X = cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),
                     Y = data[,2],
                     cbind(data[,27],data[,1]),min.node.size = 25)

tau.hat_linear <- predict(linear_model,cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),estimate.variance = TRUE)
sigma.hat_linear <- sqrt(tau.hat_linear$variance.estimates)
tau.hat_linear <- data.frame(tau.hat_linear)
upper_linear<- tau.hat_linear[,1]+1.96*sigma.hat_linear[,1]
lower_linear <- tau.hat_linear[,1]-1.96*sigma.hat_linear[,1]


poly_model <- lm_forest(X = cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),
                          Y = data[,2],
                          cbind(data[,27],data[,1],data[,1]^2),min.node.size = 25)

tau.hat_poly <- predict(poly_model,cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),estimate.variance = TRUE)
sigma.hat_poly <- sqrt(tau.hat_poly$variance.estimates)
tau.hat_poly <- data.frame(tau.hat_poly)
upper_poly<- tau.hat_poly[,1]+1.96*sigma.hat_poly[,1]
lower_poly <- tau.hat_poly[,1]-1.96*sigma.hat_poly[,1]


#Training the RDD models on GRF with unbalance restriction pruning
linear_model_Tune <- lm_forest(X = cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),
                          Y = data[,2],
                          cbind(data[,27],data[,1]),stabilize.splits = TRUE)#stabilize.splits = TRUE

tau.hat_linear_Tune <- predict(linear_model_Tune,cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),estimate.variance = TRUE)
sigma.hat_linear_Tune <- sqrt(tau.hat_linear_Tune$variance.estimates)
tau.hat_linear_Tune <- data.frame(tau.hat_linear_Tune)
upper_linear_Tune<- tau.hat_linear_Tune[,1]+1.96*sigma.hat_linear_Tune[,1]
lower_linear_Tune <- tau.hat_linear_Tune[,1]-1.96*sigma.hat_linear_Tune[,1]


poly_model_Tune <- lm_forest(X = cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),
                               Y = data[,2],
                               cbind(data[,27],data[,1],data[,1]^2),stabilize.splits = TRUE)

tau.hat_poly_Tune <- predict(poly_model_Tune,cbind(as.matrix(data[,10:18]),as.matrix(data[,28:77])),estimate.variance = TRUE)
sigma.hat_poly_Tune <- sqrt(tau.hat_poly_Tune$variance.estimates)
tau.hat_poly_Tune <- data.frame(tau.hat_poly_Tune)
upper_poly_Tune<- tau.hat_poly_Tune[,1]+1.96*sigma.hat_poly_Tune[,1]
lower_poly_Tune <- tau.hat_poly_Tune[,1]-1.96*sigma.hat_poly_Tune[,1]




#Bandwidth Restriction With Linear Model
bandwidth <- IKbandwidth(data[,1],data[,2], 59.1984)
sample.weights <- kernelwts(data[,1], 59.1984, bandwidth, "triangular")
subset <- sample.weights > 0

linear_model_band <- lm_forest(X = cbind(as.matrix(data[subset,10:18]),as.matrix(data[subset,28:77])),
                               Y = data[subset,2],
                               cbind(data[subset,27],data[subset,1]))

tau.hat_linear_band <- predict(linear_model_band,cbind(as.matrix(data[subset,10:18]),as.matrix(data[subset,28:77])),estimate.variance = TRUE)
sigma.hat_linear_band <- sqrt(tau.hat_linear_band$variance.estimates)
tau.hat_linear_band <- data.frame(tau.hat_linear_band)
upper_linear_band<- tau.hat_linear_band[,1]+1.96*sigma.hat_linear_band[,1]
lower_linear_band <- tau.hat_linear_band[,1]-1.96*sigma.hat_linear_band[,1]

#Bandwidth Restriction With Polynomial Model
bandwidth <- IKbandwidth(data[,1],data[,2], 59.1984)
sample.weights <- kernelwts(data[,1], 59.1984, bandwidth, "triangular")
subset <- sample.weights > 0

poly_model_band <- lm_forest(X = cbind(as.matrix(data[subset,10:18]),as.matrix(data[subset,28:77])),
                               Y = data[subset,2],
                               cbind(data[subset,27],data[subset,1],data[subset,1]^2))

tau.hat_poly_band <- predict(poly_model_band,cbind(as.matrix(data[subset,10:18]),as.matrix(data[subset,28:77])),estimate.variance = TRUE)
sigma.hat_poly_band <- sqrt(tau.hat_poly_band$variance.estimates)
tau.hat_poly_band <- data.frame(tau.hat_poly_band)
upper_poly_band<- tau.hat_poly_band[,1]+1.96*sigma.hat_poly_band[,1]
lower_poly_band <- tau.hat_poly_band[,1]-1.96*sigma.hat_poly_band[,1]

#Using Histogram to observe the Heterogeneous Treatment Effects with different models and pruning methods
range_all <- range(c(tau.hat_poly[,1],tau.hat_linear[,1],tau.hat_poly_Tune[,1],tau.hat_linear_Tune[,1]
                     ))#tau.hat_poly[,1], tau.hat_linear_Tune[,1],tau.hat_poly_Tune[,1],tau.hat_linear_band[,1],tau.hat_poly_band[,1]
breaks <- seq(from = range_all[1], to = range_all[2], length.out = 71) 


hist_linear <- hist(tau.hat_linear[,1],breaks = breaks,
                    main = "Heterogenerous Treatment Effects of Linear Model", 
                  xlab = "Heterogenerous Treatment Effects", 
                  col = "lightblue")
hist_poly <- hist(tau.hat_poly[,1],breaks = breaks,main = "HTE of polynomial Model", 
                    xlab = "Heterogenerous Treatment Effects", 
                    col = "lightgreen")


hist_poly_tune_linear <- hist(tau.hat_linear_Tune[,1],breaks = breaks,
                              ,main = "Heterogenerous Treatment Effects of Tune Linear Model", 
                  xlab = "Heterogenerous Treatment Effects", 
                  col = "lightgreen")
hist_poly_tune_poly <- hist(tau.hat_poly_Tune[,1],breaks = breaks,main = "Heterogenerous Treatment Effects of Tune Poly Model", 
                       xlab = "Heterogenerous Treatment Effects", 
                       col = "lightblue")


hist_poly_band_linear <- hist(tau.hat_linear_band[,1],breaks = breaks,main = "Heterogenerous Treatment Effects of Tune Linear Model", 
                       xlab = "Heterogenerous Treatment Effects", 
                       col = "lightblue")

hist_poly_band_poly <- hist(breaks = breaks,tau.hat_poly_band[,1],main = "Heterogenerous Treatment Effects of Tune poly Model", 
                       xlab = "Heterogenerous Treatment Effects", 
                       col = "lightgreen")


#Overlap the plot for comparison(The other comparison plot are generated accordingly)
png("Linear.png", width = 650, height = 400)
par(mgp = c(2.2, 0.5, 0))
plot(hist_linear, col = rgb(0.5, 0.5, 1, 0.5), 
     main = "Linear_25 V.S. U_R_Linear",
     xlab = "Treatment Effect", ylab = "Frequency", 
     xlim = c(-15, 5), ylim = c(0, 600),
     cex.axis = 2,  # Increase size of axis tick labels
     cex.lab = 2.3,  # Increase size of axis labels
     cex.main = 2.5, # Increase size of the main title        
     lwd = 2,        # Thicker axis lines
     tck = 0.02)     # Longer tick marks
plot(hist_poly_tune_linear, col = rgb(0.5, 1, 0.5, 0.5), add = TRUE)
legend("topleft", 
       legend = c("Linear_25", "U_R_Linear"), 
       fill = c(rgb(0.5, 0.5, 1, 0.5), rgb(0.5, 1, 0.5, 0.5)), 
       cex = 2.3)  # Increase legend text size
dev.off()



#Tree Plotting
linear_tree <- get_tree(linear_model, 3)
tree.plot = plot(linear_tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//linear_original.svg")

linear_tree <- get_tree(linear_model_Tune, 1)
tree.plot = plot(linear_tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//poly_25.svg")

poly_tree <- get_tree(poly_model_Tune, 1)
tree.plot = plot(poly_tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//Prune_poly_tree.svg")

poly_tree <- get_tree(poly_model_Tune, 1)
tree.plot = plot(poly_tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//Prune_poly_tree.svg")

poly_tree <- get_tree(poly_model_band, 1)
tree.plot = plot(poly_tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//band_poly_tree.svg")

poly_tree <- get_tree(linear_model_band, 1)
tree.plot = plot(poly_tree)
cat(DiagrammeRsvg::export_svg(tree.plot), file = "C://Users//ua896//OneDrive//桌面//Thesis//band_linear_tree.svg")