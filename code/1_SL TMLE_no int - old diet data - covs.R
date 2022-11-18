##############################################################################################################################
##################################### Interaction ML Analysis       ##########################################################
##################################### AUTHOR: JMP                   ##########################################################
##################################### DATE: 10.03.2022              ##########################################################
##############################################################################################################################
#--------------------------------------------------------------------------------------------------------------------
#PREPARATION --------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
#INSTALL TLVERSE
#install.packages("devtools")
#devtools::install_github("tlverse/tlverse")

#INSTALL AND LOAD PACKAGES
packages <- c("foreach","doParallel","boot","rmutil","mvtnorm","gam","sandwich","ggplot2", "SuperLearner","xgboost",
              "devtools","glmnet","tmle","data.table","rpart","ranger","nnet","arm","earth","e1071","tidyverse",
              "npcausal", "beepr", "fastDummies", "here", "remotes", "rlang", "riskCommunicator")
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package, repos='http://lib.stat.cmu.edu/R/CRAN')
  }
}
for (package in packages) {
  library(package, character.only=T)
}

remotes::install_github("tlverse/sl3")
library(sl3)
remotes::install_github("tlverse/tmle3")
library(tmle3)

sessionInfo()

if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"){
  analysis <- read_csv(here("data","MLInteraction_Imputed_ffqavail_oldnutr_2022-09-27.csv"))
}

summary(analysis)
names(analysis)


# CREATE SUPERLEARNER LIBRARY
# Have to update this for sl3 or else we get an error in tmle3 
ranger_1 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=500, mtry=2, replace=T)
ranger_2 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=500, mtry=3, replace=T)
ranger_3 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=500, mtry=4, replace=T)
ranger_4 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=500, mtry=2, replace=F)
ranger_5 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=500, mtry=3, replace=F)
ranger_6 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=500, mtry=4, replace=F)
ranger_7 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=2500, mtry=2, replace=T)
ranger_8 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=2500, mtry=3, replace=T)
ranger_9 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=2500, mtry=4, replace=T)
ranger_10 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=2500, mtry=2, replace=F)
ranger_11 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=2500, mtry=3, replace=F)
ranger_12 <- make_learner(Lrnr_ranger, min.node.size=10, num.trees=2500, mtry=4, replace=F)

#replaced shrinkage with eta and nrounds with nrounds
xgboost_1 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=4, eta=0.01)
xgboost_2 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=4, eta=0.001)
xgboost_3 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=4, eta=0.0001)
xgboost_4 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=5, eta=0.01)
xgboost_5 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=5, eta=0.001)
xgboost_6 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=5, eta=0.0001)
xgboost_7 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=6, eta=0.01)
xgboost_8 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=6, eta=0.001)
xgboost_9 <- make_learner(Lrnr_xgboost, nrounds=200, max_depth=6, eta=0.0001)
xgboost_10 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=4, eta=0.01)
xgboost_11 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=4, eta=0.001)
xgboost_12 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=4, eta=0.0001)
xgboost_13 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=5, eta=0.01)
xgboost_14 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=5, eta=0.001)
xgboost_15 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=5, eta=0.0001)
xgboost_16 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=6, eta=0.01)
xgboost_17 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=6, eta=0.001)
xgboost_18 <- make_learner(Lrnr_xgboost, nrounds=500, max_depth=6, eta=0.0001)
xgboost_19 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=4, eta=0.01)
xgboost_20 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=4, eta=0.001)
xgboost_21 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=4, eta=0.0001)
xgboost_22 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=5, eta=0.01)
xgboost_23 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=5, eta=0.001)
xgboost_24 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=5, eta=0.0001)
xgboost_25 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=6, eta=0.01)
xgboost_26 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=6, eta=0.001)
xgboost_27 <- make_learner(Lrnr_xgboost, nrounds=1000, max_depth=6, eta=0.0001)
glmnet_1 <- make_learner(Lrnr_glmnet, alpha = 0)
glmnet_2 <- make_learner(Lrnr_glmnet, alpha = 0.2)
glmnet_3 <- make_learner(Lrnr_glmnet, alpha = 0.4)
glmnet_4 <- make_learner(Lrnr_glmnet, alpha = 0.6)
glmnet_5 <- make_learner(Lrnr_glmnet, alpha = 0.8)
glmnet_6 <- make_learner(Lrnr_glmnet, alpha = 1)
mean_learner <- make_learner(Lrnr_mean)
glm_learner <- make_learner(Lrnr_glm)

# CREATE A LIST WITH ALL LEARNERS
lrn_list <- list(ranger_1, ranger_2, ranger_3, ranger_4, ranger_5, ranger_6, ranger_7, ranger_8, ranger_9,
                 ranger_10, ranger_11, ranger_12, xgboost_1, xgboost_2, xgboost_3, xgboost_4, xgboost_5, 
                 xgboost_6, xgboost_7, xgboost_8, xgboost_9, xgboost_10, xgboost_11, xgboost_12, xgboost_13, 
                 xgboost_14, xgboost_15,xgboost_16, xgboost_17, xgboost_18, xgboost_19, xgboost_20, xgboost_21, 
                 xgboost_22, xgboost_23, xgboost_24, xgboost_25, xgboost_26, xgboost_27, 
                 glmnet_1, glmnet_2, glmnet_3, glmnet_4, glmnet_5, glmnet_6, mean_learner, glm_learner)
lrn_list
#--------------------------------------------------------------------------------------------------------------------
# TMLE3 (WITH SAMPLE SPLITTING) -------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------
sl_lib <- Lrnr_sl$new(learners = lrn_list, metalearner = make_learner(Lrnr_nnls))

# PREPARE THE THINGS WE WANT TO FEED IN TO TMLE3
ate_spec <- tmle_ATE(treatment_level = 1, control_level = 0)
learner_list <- list(A = sl_lib, Y = sl_lib)
nodes_fruit <- list(W = c("g_whldens_old", "d_totdens_old", 
                          "p_totdens_old", "p_seaplantdens_old", "g_nwhldens_old", 
                          "fatratio_old", "pct_emptyc_old", "sodium_dens_old", 
                          "v_totdens_old", "bmiprepreg", "momaccult1", "black", 
                          "college", "momage", "married", "insurpub", "smokerpre", 
                          "pregplanned", "prehtn", "prediab", "epds_tot_v1", 
                          "stress_tot_v1", "anx_tot", "METminwk", "sleepsat31"), 
                    A = "f_totdens80_old", 
                    Y="pree_acog")

nodes_veg <- list(W = c("g_whldens_old", "d_totdens_old", 
                        "p_totdens_old", "p_seaplantdens_old", "g_nwhldens_old", 
                        "fatratio_old", "pct_emptyc_old", "sodium_dens_old", 
                        "f_totdens_old", "bmiprepreg", "momaccult1", "black", 
                        "college", "momage", "married", "insurpub", "smokerpre", 
                        "pregplanned", "prehtn", "prediab", "epds_tot_v1", 
                        "stress_tot_v1", "anx_tot", "METminwk", "sleepsat31"), 
                  A = "v_totdens80_old", 
                  Y="pree_acog")


#-------------------------------------------------------------------------------
# FRUIT
#-------------------------------------------------------------------------------
set.seed(123)
tmle_fruit_pree <- tmle3(ate_spec, analysis, nodes_fruit, learner_list)
tmle_fruit_pree 
saveRDS(tmle_fruit_pree, file = here("data",paste0("tmle_fruit_pree_diet2010cov_xgboostglmnetfixed.Rds")))
#Updated code runs with no errors and gives an RD of -2.4, 95% CI -4.0, -0.8

# super learner coefficients for PS model
g_fit <- tmle_fruit_pree$likelihood$factor_list[["A"]]$learner
g_fit
#g_fit$fit_object$full_fit$learner_fits$Lrnr_nnls_TRUE

# super learner coefficients for outcome model
Q_fit <- tmle_fruit_pree$likelihood$factor_list[["Y"]]$learner
Q_fit
#Q_fit$fit_object$full_fit$learner_fits$Lrnr_nnls_TRUE

#-------------------------------------------------------------------------------
# VEGETABLES
#-------------------------------------------------------------------------------
set.seed(123)
tmle_veg_pree <- tmle3(ate_spec, analysis, nodes_veg, learner_list)
tmle_veg_pree 
saveRDS(tmle_veg_pree, file = here("data",paste0("tmle_veg_pree_diet2010cov_xgboostglmnetfixed.Rds")))
#Updated code runs with no errors and gives an RD of -2.9, 95% CI -4.9, -0.8

# super learner coefficients for PS model
g_fit <- tmle_veg_pree$likelihood$factor_list[["A"]]$learner
g_fit
#g_fit$fit_object$full_fit$learner_fits$Lrnr_nnls_TRUE

# super learner coefficients for outcome model
Q_fit <- tmle_veg_pree$likelihood$factor_list[["Y"]]$learner
Q_fit
#Q_fit$fit_object$full_fit$learner_fits$Lrnr_nnls_TRUE