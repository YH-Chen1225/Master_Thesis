
<br> Applying Generalized Random Forest to Analyzing the Heterogeneous Effects in Regression Discontinuity Design
=========
![image](https://img.shields.io/badge/Language-Python3.11.7-lightgreen?style=flat)
![image](https://img.shields.io/badge/Language-R4.1.3-blue?style=flat)

## Auther
Name: Yu-Hsin Chen
<br> Email: ua89622u@gmail.com

## Brief File Description
- **DGP_RDD**: It is the main data-generating process for the Monte-Carlo Simulation section
- **First_Stage_RDD_General**: It is the simulation drawing 1000 points for testing and 1000 points for training from DGP. The test points are fixed among 500 times iterations, and training points are changing.
- **First_Stage_RDD_Point**: It is the simulation for specifically experiment the points -1, -0.5, 0,0.5, 1 from the data-generating process.
- **Neural_Net_RDD**: It creates the synthetic data for second-stage simulation. The method is mainly from Susan Athey, Guido W. Imbens, Jonas Metzger, Evan Munro, Using Wasserstein Generative Adversarial Networks for the design of Monte Carlo simulations,Journal of Econometrics,Volume 240, Issue 2, 2024,  105076,ISSN 0304-4076, https://doi.org/10.1016/j.jeconom.2020.09.013. Additionally, the data I generated was from paper Jens Ludwig, Douglas L. Miller, Does Head Start Improve Children's Life Chances? Evidence from a Regression Discontinuity Design, The Quarterly Journal of Economics, Volume 122, Issue 1, February 2007, Pages 159–208, https://doi.org/10.1162/qjec.122.1.159. In total, there are 500,000 data be generated for the simulation.
- **Neural_Nets_Extension_Result**:It is the same synthetic data generating method as **Neural_Net_RDD**, but the generated data was from Connors AF Jr, Speroff T, Dawson NV, Thomas C, Harrell FE Jr, Wagner D, Desbiens N, Goldman L, Wu AW, Califf RM, Fulkerson WJ Jr, Vidaillet H, Broste S, Bellamy P, Lynn J, Knaus WA. The effectiveness of right heart catheterization in the initial care of critically ill patients. SUPPORT Investigators. JAMA. 1996 Sep 18;276(11):889-97. doi: 10.1001/jama.276.11.889. PMID: 8782638. Similarly, 500,000 data were generated and both dataset can be found in following link[![image](https://img.shields.io/badge/Link-Generated%20Data-light%20%20yellow)](https://drive.google.com/drive/folders/1zcn1lUuuLDl4U_tCqlTtk2j2sEE4hMxW?usp=sharing)
- **Neural_Net_RDD_Result**: It is the Monte-Carlo simulation process from data generated in **Neural_Net_RDD**
- **Neural_Net_Extension_Result**: It is the Monte-Carlo simulation process from data generated in **Neural_Nets_Extension_Result**
- **Real_Case_RDD**: In this file, RDD method on GRF platform was applied to analyze the real data from the paper "Does Head Start Improve Children's Life Chances? Evidence from a Regression Discontinuity Design" and comparison with the result from the paper"Cattaneo, Titiunik and Vazquez-Bare (2017): Comparing Inference Approaches for RD Designs: A Reexamination of the Effect of Head Start on Child Mortality"
- **Real_Case_Extension**: In this file, RDD method on GRF platform was applied to analyze the real data from the paper "The effectiveness of right heart catheterization in the initial care of critically ill patients." and the result is compared with the paper  McConnell, K. J. and Lindner, S. (2019). Estimating treatment effects with machine learning Health services research, 54(6):1273–1282"

## Main Contribution
- Develop rigorous mathematics assumptions for RDD applying on the GRF platform and prove the normality property for RDD on GRF
- Bridging the gap between infinite and finite sample size by applying the typical Monte-Carlo Simulation on random 1000 points and on different certain quantile point
- Further applying the state-of-the-art method, WGAN, for Monte-Carlo Simulation. The simulation results reveal the potential challenges of RDD on GRF may face in the real-world analysis
- Applying RDD on GRF to analyze the real-world dataset with different parameters and compare them with the result from the published research to further discussed the advantage ,disadvantage and potential way to futher develop RDD on GRF in the future.



