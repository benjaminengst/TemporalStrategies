# Paper: Temporal Strategies
# Journal: Legislative Studies Quarterly
# Authors: Christoph Garwe [1] / Benjamin G. Engst [2] / Yannick Stawicki [3] / Christoph Hönnige [1]
#   [1] Leibniz University Hannover; [2] University of Mannheim; [3] Political Affairs Consultant, Barmer (Lower Saxony)

# Clear matrix #
################
rm(list = ls())

# Libraries #
#############
library(MASS) # Version: 7.3 - 51.1 
library("stargazer") # Version 5.2.2

# Session Info on system used #
###############################
sessionInfo()

# R version 3.5.3 (2019-03-11)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6
#---------------------------------------------#
# Attached packages:
# stats; graphics; grDevices; utils; datasets; methods; base
# MASS_7.3-51.1; stargazer_5.2.2
#----------------------------------------------#

# Set automatically to current working-directory #
##################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data #
#################
dat <- read.csv2("Data/LegTime20200701.csv")
  safe <- dat # Safe the data prior to subsetting
  dat <- subset(dat, dat$zustimmg == 1) # Reduce data to consent bills

# Define seperate independent variables for gain / loss #
#-------------------------------------------------------#
table(dat$potGain_same90, dat$potLoss_same90)
dat$independentGain <- 0
  dat$independentGain[dat$potGain_same90 == 1] <- 1
  table(dat$independentGain)
dat$independentLoss <- 0
  dat$independentLoss[dat$potLoss_same90 == 1] <- 1
  table(dat$independentLoss)

# CONSENT BILLS (SYMETRIC BICAMERALISM) #
#########################################  
  
# Estimate the negative binomial model (App.3.4) #
##################################################
summary(nbmodel2 <- glm.nb(phase_total 
 
                           ~ independentGain
                           + independentLoss

                           + election
                           + compresOpp
                           + completed
                           + factor(leadingcom) 
                           + factor(lp)
                           
                           , data = dat))
nobs(nbmodel2)
AIC(nbmodel2)


# Calcualte first differences (App 2.2) #
#########################################
# Define the szenarios
# Set the szenario for independentGain
scenario <-  c(0, 1, 0)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independentGain + independentLoss + election + compresOpp + completed + factor(leadingcom) + factor(lp) 
m <- model.frame(ff, data = dat)
mm <- model.matrix(ff, data = m)

dat_sim <- array(NA, dim = c(dim(mm), length(scenario))) 
dat_sim[, , ] <- mm
sel <- 1

sel <- which(colnames(mm) == "independentGain") # select variable of interest
# Put them in the data frames interesting scenarios
for (i in 1:length(scenario)) {
  dat_sim[, sel, i] <- scenario[i]
}

# Set the szenario for independentLoss
scenario2 <-  c(0, 0, 1)
sel <- which(colnames(mm) == "independentLoss") # select variable of interest
for (i in 1:length(scenario)) {
  dat_sim[, sel, i] <- scenario2[i]
}

# Set number of simulations
nsim <- 1000
# Draw simulated coefficients from MVN
set.seed(12345)
S <- mvrnorm(nsim, coef(nbmodel2), vcov(nbmodel2))
# Create Matrix to store the results
val <- matrix(NA, nrow = nsim, ncol = length(scenario))
for (i in 1:length(scenario)) {
  val[, i] <-
    apply(S, 1, function(s)
      mean(exp(dat_sim[, , i] %*% s)))
}

# Extratct the quantities of interest #
#-------------------------------------#
# Point estimate
Loss <-  mean(val[, 3])
  # Confidence interval 
  LLow <- quantile(val[, 3],.025)
  LHigh <- quantile(val[, 3],.975)
  
# First difference of Gain to category neither / nor    
FDLoss <- round(mean(val[, 3] - val[, 1]),0)
  FDLLow <- round(quantile(val[, 3] - val[, 1],.05),0)
  FDLHigh <- round(quantile(val[, 3] - val[, 1],.95),0)

# Point estimate
Gain <-  mean(val[, 2])
  # Confidence interval 
  GLow <- quantile(val[, 2],.025)
  GHigh <- quantile(val[, 2],.975)
  
# First difference of Gain to category neither / nor    
FDGain <- round(mean(val[, 2] - val[, 1]),0)
  FDGLow <- round(quantile(val[, 2] - val[, 1],.05),0)
  FDGHigh <- round(quantile(val[, 2] - val[, 1],.95),0)

# Point estimate
Neither <-  mean(val[, 1])
  # Confidence interval 
  NLow <- quantile(val[, 1],.025)
  NHigh <- quantile(val[, 1],.975)



# OBJECTION BILLS (ASYMETRIC BICAMERALISM) #
############################################
dat <- safe # Load the data originally safed
dat <- subset(dat, dat$zustimmg == 0) # Select objection bills

# Define seperate independent variables for gain / loss #
#-------------------------------------------------------#
table(dat$potGain_same90, dat$potLoss_same90)
dat$independentGain <- 0
  dat$independentGain[dat$potGain_same90 == 1] <- 1
  table(dat$independentGain)
dat$independentLoss <- 0
  dat$independentLoss[dat$potLoss_same90 == 1] <- 1
  table(dat$independentLoss)

# Estimate the negative binomial model (App.3.4) #
##################################################
summary(nbmodel4 <- glm.nb(phase_total 
                           
                           ~ independentGain
                           + independentLoss
                           
                           + election
                           + compresOpp
                           + completed
                           + factor(leadingcom) 
                           + factor(lp)
                           
                           , data = dat))
nobs(nbmodel4)
AIC(nbmodel4)

# Calcualte first differences (App 2.2) #
#########################################
# Set the szenario for independentGain
scenario <-  c(0, 1, 0)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independentGain + independentLoss + election + compresOpp + completed + factor(leadingcom) + factor(lp) 
m <- model.frame(ff, data = dat)
mm <- model.matrix(ff, data = m)

dat_sim <- array(NA, dim = c(dim(mm), length(scenario))) 
dat_sim[, , ] <- mm
sel <- 1

sel <- which(colnames(mm) == "independentGain") # select variable of interest
# Put them in the data frames interesting scenarios
for (i in 1:length(scenario)) {
  dat_sim[, sel, i] <- scenario[i]
}

# Set the szenario for independentLoss
scenario2 <-  c(0, 0, 1)
sel <- which(colnames(mm) == "independentLoss") # select variable of interest
for (i in 1:length(scenario)) {
  dat_sim[, sel, i] <- scenario2[i]
}

# Set number of simulations
nsim <- 1000
# Draw simulated coefficients from MVN
set.seed(12345)
S <- mvrnorm(nsim, coef(nbmodel4), vcov(nbmodel4))
# Create Matrix to store the results
val <- matrix(NA, nrow = nsim, ncol = length(scenario))
for (i in 1:length(scenario)) {
  val[, i] <-
    apply(S, 1, function(s)
      mean(exp(dat_sim[, , i] %*% s)))
}

# Extratct the quantities of interest #
#-------------------------------------#
# Point estimate
Loss <-  mean(val[, 3])
  # Confidence interval 
  LLow <- quantile(val[, 3],.025)
  LHigh <- quantile(val[, 3],.975)

# First difference of Gain to category neither / nor    
FDLoss <- round(mean(val[, 3] - val[, 1]),0)
  FDLLow <- round(quantile(val[, 3] - val[, 1],.05),0)
  FDLLHigh <- round(quantile(val[, 3] - val[, 1],.95),0)  

# Point estimate
Gain <-  mean(val[, 2])
  # Confidence interval 
  GLow <- quantile(val[, 2],.025)
  GHigh <- quantile(val[, 2],.975)

# First difference of Gain to category neither / nor    
FDGain <- round(mean(val[, 2] - val[, 1]),0)
  FDGLow <- round(quantile(val[, 2] - val[, 1],.05),0)
  FDGLHigh <- round(quantile(val[, 2] - val[, 1],.95),0)

# Point estimate
Neither <-  mean(val[, 1])
  # Confidence interval 
  NLow <- quantile(val[, 1],.025)
  NHigh <- quantile(val[, 1],.975)

# Safe all models in tables (html) #
####################################
# Table App. 3.4
stargazer(nbmodel2, nbmodel4
          , title = "Table X: Results from negative binomial of the effect of potential gain or loss on government’s legislative timing"
          , style = "ajs"
          , summary = TRUE
          , model.numbers = FALSE
          , dep.var.labels = c("Time btw. initiation and termination")
          , column.labels = c("symetric bicameralism <br> w/controls", "asymetric  bicameralism <br> w/controls")
          , covariate.labels = c("Independent gain (=1)"
                                 , "Independent loss (=1)"
                                 
                                 , "Election in the next three months (=1)"
                                 , "Leading committee chair is of an opposition party (=1)"
                                 , "Bill passed through the legislative process (=1)"
                                 
                                 , "Topic - Labor, Health and Social Affairs (=1)"
                                 , "Topic - Internal Affairs (=1)"
                                 , "Topic - Finance (=1)"
                                 , "Topic - Agriculture and Food (=1)"
                                 , "Topic - Environmental Affairs (=1)"
                                 , "Topic - Foreign Affairs and Defense (=1)"
                                 , "Topic - Education, Cluture and Family Affairs (=1)"
                                 , "Topic - Economics (=1)"
                                 , "Topic - Legal Affairs (=1)"
                                 
                                 , "Legislative Periode 15 (=1)"
                                 , "Legislative Periode 16 (=1)"
                                 , "Legislative Periode 17 (=1)"
          )
          , intercept.bottom = TRUE
          , notes.append = FALSE
          , star.cutoffs = c(0.1, 0.05, 0.01)
          , nobs = TRUE
          , omit.stat = c("ll","theta")
          , notes = c("$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1 <br> Reference Topic - Transportation, Building and Housing <br> Reference Legislative Periode 14")
          , no.space=FALSE
          , single.row=FALSE
          , out = "Outcome/App3-4.htm") 
