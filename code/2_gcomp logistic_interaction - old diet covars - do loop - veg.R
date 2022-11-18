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

#Total Vegetable Consumption - Logistic with gComp

#Interaction with Whole Grains
if(Sys.info()["nodename"]=="EPIC02G44CTQ05P"|
   Sys.info()["nodename"]=="EPI9TPH7M3"|
   Sys.info()["nodename"]=="LAPTOP-KHT5IA2Q"|
   Sys.info()["nodename"]=="BLACKBEAN"){
  analysis <- read_csv(here("data","MLInteraction_Imputed_ffqavail_oldnutr_2022-09-27.csv"))
}

#Add in all interaction terms
analysis$Iwhlg  <- analysis$v_totdens80_old * analysis$g_whldens_old
analysis$Ifatratio  <- analysis$v_totdens80_old * analysis$fatratio_old
analysis$Ip_tot  <- analysis$v_totdens80_old * analysis$p_totdens_old
analysis$Ip_seaplant  <- analysis$v_totdens80_old * analysis$p_seaplantdens_old
analysis$Id_tot  <- analysis$v_totdens80_old * analysis$d_totdens_old
analysis$If_tot  <- analysis$v_totdens80_old * analysis$f_totdens_old
analysis$Inwh_tot  <- analysis$v_totdens80_old * analysis$g_nwhldens_old
analysis$Isod  <- analysis$v_totdens80_old * analysis$sodium_dens_old
analysis$Iemptyc  <- analysis$v_totdens80_old * analysis$pct_emptyc_old
analysis$Ibmi  <- analysis$v_totdens80_old * analysis$bmiprepreg
analysis$Iacc  <- analysis$v_totdens80_old * analysis$momaccult1
analysis$Iblack  <- analysis$v_totdens80_old * analysis$black
analysis$Icollege  <- analysis$v_totdens80_old * analysis$college
analysis$Iage  <- analysis$v_totdens80_old * analysis$momage
analysis$Imarried  <- analysis$v_totdens80_old * analysis$married
analysis$Iinsurpub  <- analysis$v_totdens80_old * analysis$insurpub
analysis$Ismokerpre  <- analysis$v_totdens80_old * analysis$smokerpre
analysis$Iplan  <- analysis$v_totdens80_old * analysis$pregplanned
analysis$Ihtn  <- analysis$v_totdens80_old * analysis$prehtn
analysis$Idiab  <- analysis$v_totdens80_old * analysis$prediab
analysis$Idepress  <- analysis$v_totdens80_old * analysis$epds_tot_v1
analysis$Istress  <- analysis$v_totdens80_old * analysis$stress_tot_v1
analysis$Ianx  <- analysis$v_totdens80_old * analysis$anx_tot
analysis$IMET  <- analysis$v_totdens80_old * analysis$METminwk
analysis$Isleep  <- analysis$v_totdens80_old * analysis$sleepsat31

#------------NO INTERACTION------------#
#Using riskCommunicator package
 analysis$v_totdens80_old<-as.factor(analysis$v_totdens80_old)
 set.seed(123)
 V_notInt_logistic <- gComp(data = analysis, Y = "pree_acog", X = "v_totdens80_old",
                                  Z = c("v_totdens80_old", "g_whldens_old", "d_totdens_old", 
"p_totdens_old", "p_seaplantdens_old", "g_nwhldens_old", 
"fatratio_old", "pct_emptyc_old", "sodium_dens_old", 
"f_totdens_old", "bmiprepreg", "momaccult1", "black", 
"college", "momage", "married", "insurpub", "smokerpre", 
"pregplanned", "prehtn", "prediab", "epds_tot_v1", 
"stress_tot_v1", "anx_tot", "METminwk", "sleepsat31"), 
                                  outcome.type = "binary", R=100)
 V_notInt_logistic
#RD =  -0.012 (-0.029, 0.006)
 
#Using Ashley's code (modified)
covars = c("v_totdens80_old", "g_whldens_old", "d_totdens_old", 
           "p_totdens_old", "p_seaplantdens_old", "g_nwhldens_old", 
           "fatratio_old", "pct_emptyc_old", "sodium_dens_old", 
           "f_totdens_old", "bmiprepreg", "momaccult1", "black", 
           "college", "momage", "married", "insurpub", "smokerpre", 
           "pregplanned", "prehtn", "prediab", "epds_tot_v1", 
           "stress_tot_v1", "anx_tot", "METminwk", "sleepsat31")
formulaVars = paste(covars,collapse = "+")
modelForm <- as.formula(paste0("pree_acog ~ ", formulaVars)) ## include the interaction!

#' Regress the outcome against the confounders with interaction
ms_model <- glm(modelForm,data=analysis,family=binomial("logit"))
##' Generate predictions for everyone in the sample to obtain
##' unexposed (mu0 predictions) and exposed (mu1 predictions) risks.
mu1 <- predict(ms_model,newdata=transform(analysis,v_totdens80_old=1),type="response")
mu0 <- predict(ms_model,newdata=transform(analysis,v_totdens80_old=0),type="response")

#' Marginally adjusted risk difference
marg_stand_RD <- mean(mu1)-mean(mu0)
marg_stand_RD
#RD -0.01185544

#------------ADDING 2-WAY INTERACTION TERMS------------#
#Using Ashley's modified code
#Applying for loop
for (i in c("Iwhlg", "Ifatratio", "Ip_tot", "Ip_seaplant", "Id_tot", 
            "If_tot", "Inwh_tot", "Isod", "Iemptyc",
            "Ibmi", "Iacc", "Iblack", "Icollege", "Iage", "Imarried", 
            "Iinsurpub", "Ismokerpre", "Iplan", "Ihtn", "Idiab", 
            "Idepress", "Istress", "Ianx", "IMET", "Isleep")) {

  #' Marginal Standardization: version 1
  covars = c("v_totdens80_old", "g_whldens_old", "d_totdens_old", 
                   "p_totdens_old", "p_seaplantdens_old", "g_nwhldens_old", 
                   "fatratio_old", "pct_emptyc_old", "sodium_dens_old", 
                   "f_totdens_old", "bmiprepreg", "momaccult1", "black", 
                   "college", "momage", "married", "insurpub", "smokerpre", 
                    "pregplanned", "prehtn", "prediab", "epds_tot_v1", 
                    "stress_tot_v1", "anx_tot", "METminwk", "sleepsat31")
    formulaVars = paste(covars,collapse = "+")
    modelForm <- as.formula(paste0("pree_acog ~ ", formulaVars, " + ", print(i))) ## include the interaction!
  #' Regress the outcome against the confounders with interaction
  ms_model <- glm(modelForm,data=analysis,family=binomial("logit"))
  ##' Generate predictions for everyone in the sample to obtain
  ##' unexposed (mu0 predictions) and exposed (mu1 predictions) risks.
  mu1 <- predict(ms_model,newdata=transform(analysis,v_totdens80_old=1),type="response")
  mu0 <- predict(ms_model,newdata=transform(analysis,v_totdens80_old=0),type="response")
  
  print(mean(mu1))
  print(mean(mu0))
  #' Marginally adjusted risk difference
  marg_stand_RD <- mean(mu1)-mean(mu0)
  print(marg_stand_RD)
}

# [1] 0.0428254
# [1] 0.106054
# [1] -0.06322859
