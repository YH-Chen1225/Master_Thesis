
<br> Applying Generalized Random Forest to Analyzing the Heterogeneous Effects in Regression Discontinuity Design
=========
![image](https://img.shields.io/badge/Language-Python3.11.7-lightgreen?style=flat)
![image](https://img.shields.io/badge/Language-R4.1.3-blue?style=flat)

## Brief file Description
- DGP_RDD: It is the main data-generating process for the Monte-Carlo Simulation section
- First_Stage_RDD_General: It is the simulation drawing 1000 points for testing and 1000 points for training from DGP. The test points are fixed among 500 times iterations, and training points are changing.
- First_Stage_RDD_Point: It is the simulation for specifically experiment the points -1, -0.5, 0,0.5, 1 from the data-generating process.
- Neural_Net_RDD: It creates the synthetic data for second-stage simulation. The method is mainly from Susan Athey, Guido W. Imbens, Jonas Metzger, Evan Munro, Using Wasserstein Generative Adversarial Networks for the design of Monte Carlo simulations,Journal of Econometrics,Volume 240, Issue 2, 2024,  105076,ISSN 0304-4076, https://doi.org/10.1016/j.jeconom.2020.09.013.

