# mice package: Multivariate imputation by chained equation.
install.packages('mice')
library(mice)

install.packages('rms')
library(rms)

install.packages('finalfit')
library(finalfit)

# Transforamtion - normaliztaion, skewness계산
install.packages('rms')
library(rms)
install.packages('bestNormalize')
library(bestNormalize)

####### selecting variables : best subset selection ########
install.packages('leaps')
library(leaps)
library(datasets)

# Variable importance : regression
# Maximal information coefficient (MIC) -------------
install.packages('minerva')
library(minerva)
# Variable Importance: Classification problem
# Relief algorithm
install.packages('CORElearn')
library(CORElearn)
# ISIS
install.packages('SIS')
library(SIS)
# Simulated Annealing : caret
install.packages('caret')
library(caret)

# PCA
# 시각화 
install.packages("corrplot")

install.packages(lattice)
library(lattice)
install.packages('car')
library(car)
install.packages('boot')
library(boot)

# Prinicipal Curve
install.packages('princurve')
library(princurve)
# Kernel PCA
install.packages('kernlab')
library(kernlab)

# Non-negative matrix factorization ------------------------------------

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")
install.packages('NMF')
library(NMF)

# Independent component analysis -----------------------------------

install.packages('fastICA')
library(fastICA)

# Imbalanced dataset -----------------------------------
# ROC curve
install.packages('pROC')
library(pROC)
library(MASS)
install.packages('caret')
library(caret)
# SMOTE ----------------------------------------------
install.packages("smotefamily")
library(smotefamily)

################# Ensemble-based methods ###################
install.packages("ebmc")
library(ebmc)

# modeling관련
install.packages("gam")
library(gam)
install.packages('matrixcalc')
library(matrixcalc)
install.packages('lmvar')
library(lmvar)
install.packages('lmtest')
library(lmtest)
install.packages('astsa')
library(astsa)
install.packages("orcutt")
library(orcutt)
install.packages('spatialreg')
library(spatialreg)
install.packages('ordinal')
library(ordinal)
install.packages('lme4')
library(lme4)

