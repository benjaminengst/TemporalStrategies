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
dat <- read.csv2("Data/LegTimeState20200701.csv")

# How many consent bills are there per legislative periode?
table(dat$lp == 14 & dat$zustimmg == 1)
table(dat$lp == 15 & dat$zustimmg == 1)
table(dat$lp == 16 & dat$zustimmg == 1)
table(dat$lp == 17 & dat$zustimmg == 1)
# Safe for later estimatiions to not always reload dataset
safe <- dat

# Estimation only for Legislative Period 14 (App 7.3 & 7.4) #
#############################################################
dat <- safe
  # Reduce to consent bills and one legislatiive period  
  dat <- subset(dat, dat$zustimmg == 1)
  dat <- subset(dat, dat$lp == 14)

# Define one independent variable for gain / loss / neither or nor #
#------------------------------------------------------------------#
# Potential gain == 1; potential loss == -1; neither or nor == 0
table(dat$potGain_same90, dat$potLoss_same90)
dat$independent <- 0
  dat$independent[dat$potGain_same90 == 1] <- 1
  dat$independent[dat$potLoss_same90 == 1] <- -1
table(dat$independent)

# Estimate the negative binomial model (App 7.3 - LP14) #
#-------------------------------------------------------#
summary(nbmodel14 <- glm.nb(phase_total ~ independent
                          + election
                          + compresOpp
                          + completed
                          + factor(leadingcom) 
                          , data = dat))
nobs(nbmodel14)
AIC(nbmodel14)

# Calcualte first differences (App 7.4) #
#---------------------------------------#
# Define the szenarios
scenario <- c(-1, 0, 1)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independent + election + compresOpp + completed + factor(leadingcom) 
m <- model.frame(ff, data = dat)
mm <- model.matrix(ff, data = m)

dat_sim <- array(NA, dim = c(dim(mm), length(scenario))) 

dat_sim[, , ] <- mm
sel <- 1
dat_sim[, sel, ] <- 1 # Exchange y for column of 1s

sel <- which(colnames(mm) == "independent") # select variable of interest

# Put them in the data frames interesting scenarios
for (i in 1:length(scenario)) {
  dat_sim[, sel, i] <- scenario[i]
}

# Set number of simulations
nsim <- 1000
# Draw simulated coefficients from MVN
set.seed(12345)
S <- mvrnorm(nsim, coef(nbmodel14), vcov(nbmodel14))
# Create Matrix to store the results
val <- matrix(NA, nrow = nsim, ncol = length(scenario))
for (i in 1:length(scenario)) {
  val[, i] <-
    apply(S, 1, function(s)
      mean(exp(dat_sim[, , i] %*% s)))
}

# Estimate the quantities of interest #
# First difference of loss to category neither / nor    
FDLoss <- round(mean(val[, 1] - val[, 2]),0)
  FDLLow <- round(quantile(val[, 1] - val[, 2],.05),0)
  FDLHigh <- round(quantile(val[, 1] - val[, 2],.95),0)

# First difference of gain to category neither / nor    
FDGain <- round(mean(val[, 3] - val[, 2]),0)
  FDGLow <- round(quantile(val[, 3] - val[, 2],.05),0)
  FDGHigh <- round(quantile(val[, 3] - val[, 2],.95),0)
  
# Estimation only for Legislative Period 15 #
#############################################  
# There is no potential gain or loss in this legislative period  
dat <- safe
  # Reduce to consent bills and one legislatiive period  
  dat <- subset(dat, dat$zustimmg == 1)
  dat <- subset(dat, dat$lp == 15)  
  
# Define one independent variable for gain / loss / neither or nor #
#------------------------------------------------------------------#
# Potential gain == 1; potential loss == -1; neither or nor == 0
table(dat$potGain_same90, dat$potLoss_same90)
dat$independent <- 0
  dat$independent[dat$potGain_same90 == 1] <- 1
  dat$independent[dat$potLoss_same90 == 1] <- -1
table(dat$independent)  
  
# Estimation only for Legislative Period 16 (App 7.3 & 7.4) #
#############################################################
# There is no potential gain in this legislative period  
dat <- safe
  # Reduce to consent bills and one legislatiive period  
  dat <- subset(dat, dat$zustimmg == 1)
  dat <- subset(dat, dat$lp == 16)

# Define one independent variable for gain / loss / neither or nor #
#------------------------------------------------------------------#
# Potential gain == 1; potential loss == -1; neither or nor == 0
table(dat$potGain_same90, dat$potLoss_same90)
dat$independent <- 0
  dat$independent[dat$potGain_same90 == 1] <- 1
  dat$independent[dat$potLoss_same90 == 1] <- -1
table(dat$independent)

# Estimate the negative binomial model (App 7.3 - LP16) #
#-------------------------------------------------------#
summary(nbmodel16 <- glm.nb(phase_total ~ independent
                            + compresOpp
                            + factor(leadingcom) 
                          , data = dat))
nobs(nbmodel16)
AIC(nbmodel16)

# Calcualte first differences (App 7.4) #
#---------------------------------------#
# Define the szenarios
scenario <- c(-1, 0)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independent + compresOpp + factor(leadingcom) 
m <- model.frame(ff, data = dat)
mm <- model.matrix(ff, data = m)

dat_sim <- array(NA, dim = c(dim(mm), length(scenario))) 

dat_sim[, , ] <- mm
sel <- 1
dat_sim[, sel, ] <- 1 # Exchange y for column of 1s

sel <- which(colnames(mm) == "independent") # select variable of interest

# Put them in the data frames interesting scenarios
for (i in 1:length(scenario)) {
  dat_sim[, sel, i] <- scenario[i]
}

# Set number of simulations
nsim <- 1000
# Draw simulated coefficients from MVN
set.seed(12345)
S <- mvrnorm(nsim, coef(nbmodel16), vcov(nbmodel16))
# Create Matrix to store the results
val <- matrix(NA, nrow = nsim, ncol = length(scenario))
for (i in 1:length(scenario)) {
  val[, i] <-
    apply(S, 1, function(s)
      mean(exp(dat_sim[, , i] %*% s)))
}

# Estimate the quantities of interest #
# First difference of loss to category neither / nor    
FDLoss <- round(mean(val[, 1] - val[, 2]),0)
  FDLLow90 <- round(quantile(val[, 1] - val[, 2],.05),0)
  FDLHigh90 <- round(quantile(val[, 1] - val[, 2],.95),0)

# Estimation only for Legislative Period 16 (App 7.3 & 7.4) #
#############################################################  
dat <- safe
  # Reduce to consent bills and one legislatiive periode  
  dat <- subset(dat, dat$zustimmg == 1)
  dat <- subset(dat, dat$lp == 17)
  
# Define one independent variable for gain / loss / neither or nor #
#------------------------------------------------------------------#
# Potential gain == 1; potential loss == -1; neither or nor == 0
table(dat$potGain_same90, dat$potLoss_same90)
dat$independent <- 0
  dat$independent[dat$potGain_same90 == 1] <- 1
  dat$independent[dat$potLoss_same90 == 1] <- -1
table(dat$independent)
  
# Estimate the negative binomial model (App 7.3 - LP17) #
#-------------------------------------------------------#
summary(nbmodel17 <- glm.nb(phase_total ~ independent
                          + compresOpp
                          + completed
                          + factor(leadingcom) 
                          , data = dat))
nobs(nbmodel17)
AIC(nbmodel17)

# Calcualte first differences (App 7.4) #
#---------------------------------------#
# Define the szenarios
scenario <- c(-1, 0, 1)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independent + compresOpp + completed + factor(leadingcom) 
m <- model.frame(ff, data = dat)
mm <- model.matrix(ff, data = m)
  
  dat_sim <- array(NA, dim = c(dim(mm), length(scenario))) 
  
  dat_sim[, , ] <- mm
  sel <- 1
  dat_sim[, sel, ] <- 1 # Exchange y for column of 1s
  
  sel <- which(colnames(mm) == "independent") # select variable of interest
  
  # Put them in the data frames interesting scenarios
  for (i in 1:length(scenario)) {
    dat_sim[, sel, i] <- scenario[i]
  }
  
  # Set number of simulations
  nsim <- 1000
  # Draw simulated coefficients from MVN
  set.seed(12345)
  S <- mvrnorm(nsim, coef(nbmodel17), vcov(nbmodel17))
  # Create Matrix to store the results
  val <- matrix(NA, nrow = nsim, ncol = length(scenario))
  
  for (i in 1:length(scenario)) {
    val[, i] <-
      apply(S, 1, function(s)
        mean(exp(dat_sim[, , i] %*% s)))
  }
  
# Estimate the quantities of interest #
# First difference of loss to category neither / nor    
FDLoss <- round(mean(val[, 1] - val[, 2]),0)
  FDLLow90 <- round(quantile(val[, 1] - val[, 2],.05),0)
  FDLHigh90 <- round(quantile(val[, 1] - val[, 2],.95),0)
  
# First difference of gain to category neither / nor    
FDGain <- round(mean(val[, 3] - val[, 2]),0)
  FDGLow90 <- round(quantile(val[, 3] - val[, 2],.05),0)
  FDGHigh90 <- round(quantile(val[, 3] - val[, 2],.95),0)
 

# Safe all models in tables (html) #
####################################
# Table App. 7.3  
stargazer(nbmodel14, nbmodel16, nbmodel17
            , title = "Table X: Results from negative binomial of the effect of potential gain or loss on government’s legislative timing"
            , style = "ajs"
            , summary = TRUE
            , model.numbers = FALSE
            , dep.var.labels = c("Time btw. initiation and termination")
            , column.labels = c("symetric bicameralism <br> LP14", "symetric bicameralism <br> LP16", "symetric bicameralism <br> LP17")
            , covariate.labels = c("Potential 2$^{nd}$ Chamber Change"
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
                                   
            )
            , intercept.bottom = TRUE
            , notes.append = FALSE
            , star.cutoffs = c(0.1, 0.05, 0.01)
            , nobs = TRUE
            , omit.stat = c("ll","theta")
            , notes = c("$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1 <br> Reference Topic - Transportation, Building and Housing")
            , no.space=FALSE
            , single.row=FALSE
            , out = "Outcome/App7-3.htm")

