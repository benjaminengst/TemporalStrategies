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

# Define one independent variable for gain / loss / neither or nor #
#------------------------------------------------------------------#
# Potential gain == 1; potential loss == -1; neither or nor == 0
table(dat$potGain_same90, dat$potLoss_same90)
dat$independent <- 0
  dat$independent[dat$potGain_com == 1] <- 1
  dat$independent[dat$potLoss_com == 1] <- -1
table(dat$independent)

# CONSENT BILLS (SYMETRIC BICAMERALISM) #
#########################################

# Estimate the negative binomial model (App 4.1 - Model 3a) #
#############################################################
summary(nbmodel2 <- glm.nb(phase_total ~ independent
                          + election
                          + compresOpp
                          + completed
                          + factor(lp)
                          , data = dat))
nobs(nbmodel2)
AIC(nbmodel2)

# Calcualte expected values and first differences (App 4.4; App 2.2) #
#######################################################################
# Define the szenarios
scenario <- c(-1, 0, 1)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independent + election + compresOpp + completed + factor(lp) 
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
Loss <-  mean(val[, 1])
  # Confidence interval 
  LLow <- quantile(val[, 1],.025)
  LHigh <- quantile(val[, 1],.975)
# First difference of loss to category neither / nor    
FDLoss <- round(mean(val[, 1] - val[, 2]),0)
  FDLLow <- round(quantile(val[, 1] - val[, 2],.05),0)
  FDLHigh <- round(quantile(val[, 1] - val[, 2],.95),0)

# Point estimate
Neither <-  mean(val[, 2])
  # Confidence interval 
  NLow <- quantile(val[, 2],.025)
  NHigh <- quantile(val[, 2],.975)

# Point estimate
Gain <-  mean(val[, 3])
  # Confidence interval 
  GLow <- quantile(val[, 3],.025)
  GHigh <- quantile(val[, 3],.975)
# First difference of gain to category neither / nor    
FDGain <- round(mean(val[, 3] - val[, 2]),0)
  FDGLow <- round(quantile(val[, 3] - val[, 2],.05),0)
  FDGHigh <- round(quantile(val[, 3] - val[, 2],.95),0)
  
# Plot the results in App4.4 (as PDF) #
#######################################
pdf("Outcome/App4-4_consent.pdf", width=8, height=7); par(mar=c(1, 8.5 , 1, 0) + 1)
par(mfrow = c(2, 1))
barplot(cbind(
  rbind(mean(dat$phase1[dat$independent == 1])
        , mean(dat$phase2[dat$independent == 1])
        , mean(dat$phase3[dat$independent == 1])
        , mean(dat$phase4[dat$independent == 1])),
  
  rbind(mean(dat$phase1[dat$independent == 0])
        , mean(dat$phase2[dat$independent == 0])
        , mean(dat$phase3[dat$independent == 0])
        , mean(dat$phase4[dat$independent == 0])),
  
  rbind(mean(dat$phase1[dat$independent == -1])
        , mean(dat$phase2[dat$independent == -1])
        , mean(dat$phase3[dat$independent == -1])
        , mean(dat$phase4[dat$independent == -1]))
)
, las = 1
, col = c(gray(.8), gray(.6), gray(.4), gray(.2))
, border = NA
, xaxt = "n"
, xlim=c(0,200)
, axes = TRUE
, space = TRUE
, horiz = TRUE)  

title(main = ("Symmetric Bicameralism"),  line = 1.25, cex.main = 1)
title(main = ("Length of legislative process when government expects ..."),  line = .25, cex.main = 0.8)

text(-35, 1.5, labels = "...potential gain \n (deceleration)", srt = 360,  xpd = TRUE, cex=1)
text(-35, 3.5, labels = "...neither gain, \n nor loss", srt = 360,  xpd = TRUE, cex=1)
text(-35, 5.5, labels = "...potential loss \n (acceleration)", srt = 360, xpd = TRUE, cex=1)

legend( "bottomright"
        , legend = c("Initiation Phase"
                     ,"Pre-Committee P."
                     ,"Intra-Committee P."
                     ,"Termination P.")
        , fill = c(gray(.8), gray(.6), gray(.4), gray(.2))
        , border = FALSE
        #    , lwd = 1 
        , bg = NULL # Keine Hintergrungfarbe
        , cex = .9
        , bty = "n")


par(mar=c(4, 7.5 , 2, 0) + 1)
plot(seq(0:5)
     , las = 1.1
     , type="n"
     , ylim=c(0,1)
     , xlim=c(0,200)
     , yaxt="n" 
     , ylab=""
     , xlab="Length in first chamber in days"
     , sub = "" 
     , main= "Simulated length of legislative process \n from negative binomal regression (observed value approach) \n when government expects ..."
     , cex.lab = 1
     , cex.main = .8
     , frame.plot=F)

points(Gain, .2, pch=20, cex=2.5, col="black")           
segments(GHigh, .2, GLow, .2, col="black", lty=1, lwd=1.5, lend=0)
points(Neither, .5, pch=20, cex=2.5, col="black")           
segments(NHigh, .5, NLow, .5, col="black", lty=1, lwd=1.5, lend=0)
points(Loss, .8, pch=20, cex=2.5, col="black")           
segments(LHigh, .8, LLow, .8, col="black", lty=1, lwd=1.5, lend=0)

text(-35, .2, labels = "... potential gain \n (deceleration)", srt = 360,  xpd = TRUE, cex=1)
text(-35, .5, labels = "... neither gain, nor loss", srt = 360,  xpd = TRUE, cex=1)
text(-35, .8, labels = "... potential loss \n (acceleration)", srt = 360, xpd = TRUE, cex=1)

dev.off()

# OBJECTION BILLS (ASYMETRIC BICAMERALISM) #
############################################
dat <- safe # Load the data originally safed
  dat <- subset(dat, dat$zustimmg == 0) # Select objection bills

# Define one independent variable for gain / loss / neither or nor #
#------------------------------------------------------------------#
# Potential gain == 1; potential loss == -1; neither or nor == 0
table(dat$potGain_com, dat$potLoss_com)
dat$independent <- 0
  dat$independent[dat$potGain_com == 1] <- 1
  dat$independent[dat$potLoss_com == 1] <- -1
table(dat$independent)

# Estimate the negative binomial model (App 4.1 - Model 3b) #
#############################################################
summary(nbmodel4 <- glm.nb(phase_total ~ independent
                          + election
                          + compresOpp
                          + completed
                          + factor(lp)
                          , data = dat))
nobs(nbmodel4)
AIC(nbmodel4)

# Calcualte expected values and first differences (App 4.4; App 2.2) #
######################################################################
# Define the szenarios
scenario <- c(-1, 0, 1)
# Shape dataset to simulate quantities of interest
ff <- phase_total ~ independent + election + compresOpp + completed + factor(lp) 
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
Loss <-  mean(val[, 1])
  # Confidence interval 
  LLow <- quantile(val[, 1],.025)
  LHigh <- quantile(val[, 1],.975)
# First difference of loss to category neither / nor    
FDLoss <- round(mean(val[, 1] - val[, 2]),0)
  FDLLow <- round(quantile(val[, 1] - val[, 2],.05),0)
  FDLHigh <- round(quantile(val[, 1] - val[, 2],.95),0)

# Point estimate
Neither <-  mean(val[, 2])
  # Confidence interval 
  NLow <- quantile(val[, 2],.025)
  NHigh <- quantile(val[, 2],.975)

# Point estimate
Gain <-  mean(val[, 3])
  # Confidence interval 
  GLow <- quantile(val[, 3],.025)
  GHigh <- quantile(val[, 3],.975)
# First difference of gain to category neither / nor    
FDGain <- round(mean(val[, 3] - val[, 2]),0)
  FDGLow <- round(quantile(val[, 3] - val[, 2],.05),0)
  FDGHigh <- round(quantile(val[, 3] - val[, 2],.95),0)
  
# Plot the results in App 4.4 (as PDF) #
########################################
pdf("Outcome/App4-4_objection.pdf", width=8, height=7); par(mar=c(1, 8.5 , 1, 0) + 1)
par(mfrow = c(2, 1))
barplot(cbind(
  rbind(mean(dat$phase1[dat$independent == 1])
        , mean(dat$phase2[dat$independent == 1])
        , mean(dat$phase3[dat$independent == 1])
        , mean(dat$phase4[dat$independent == 1])),
  
  rbind(mean(dat$phase1[dat$independent == 0])
        , mean(dat$phase2[dat$independent == 0])
        , mean(dat$phase3[dat$independent == 0])
        , mean(dat$phase4[dat$independent == 0])),
  
  rbind(mean(dat$phase1[dat$independent == -1])
        , mean(dat$phase2[dat$independent == -1])
        , mean(dat$phase3[dat$independent == -1])
        , mean(dat$phase4[dat$independent == -1]))
)
, las = 1
, col = c(gray(.8), gray(.6), gray(.4), gray(.2))
, border = NA
, xaxt = "n"
, xlim=c(0,200)
, axes = TRUE
, space = TRUE
, horiz = TRUE)  

title(main = ("Asymmetric Bicameralism"),  line = 1.25, cex.main = 1)
title(main = ("Length of legislative process when government expects ..."),  line = .25, cex.main = 0.8)

text(-35, 1.5, labels = "...potential gain \n (deceleration)", srt = 360,  xpd = TRUE, cex=1)
text(-35, 3.5, labels = "...neither gain, \n nor loss", srt = 360,  xpd = TRUE, cex=1)
text(-35, 5.5, labels = "...potential loss \n (acceleration)", srt = 360, xpd = TRUE, cex=1)

legend( "bottomright"
        , legend = c("Initiation Phase"
                     ,"Pre-Committee P."
                     ,"Intra-Committee P."
                     ,"Termination P.")
        , fill = c(gray(.8), gray(.6), gray(.4), gray(.2))
        , border = FALSE
        #    , lwd = 1 
        , bg = NULL # Keine Hintergrungfarbe
        , cex = .9
        , bty = "n")


par(mar=c(4, 7.5 , 2, 0) + 1)
plot(seq(0:5)
     , las = 1.1
     , type="n"
     , ylim=c(0,1)
     , xlim=c(0,200)
     , yaxt="n" 
     , ylab=""
     , xlab="Length in first chamber in days"
     , sub = "" 
     , main= "Simulated length of legislative process \n from negative binomal regression (observed value approach) \n when government expects ..."
     , cex.lab = 1
     , cex.main = .8
     , frame.plot=F)

points(Gain, .2, pch=20, cex=2.5, col="black")           
segments(GHigh, .2, GLow, .2, col="black", lty=1, lwd=1.5, lend=0)
points(Neither, .5, pch=20, cex=2.5, col="black")           
segments(NHigh, .5, NLow, .5, col="black", lty=1, lwd=1.5, lend=0)
points(Loss, .8, pch=20, cex=2.5, col="black")           
segments(LHigh, .8, LLow, .8, col="black", lty=1, lwd=1.5, lend=0)

text(-35, .2, labels = "... potential gain \n (deceleration)", srt = 360,  xpd = TRUE, cex=1)
text(-35, .5, labels = "... neither gain, nor loss", srt = 360,  xpd = TRUE, cex=1)
text(-35, .8, labels = "... potential loss \n (acceleration)", srt = 360, xpd = TRUE, cex=1)

dev.off()

# Safe all models in tables (html) #
####################################
# Table App. 4.1 - Model 3a / Model 3b
stargazer(nbmodel2,nbmodel4
          , title = "Table X: Results from negative binomial of the effect of potential gain or loss on government’s legislative timing"
          , style = "ajs"
          , summary = TRUE
          , model.numbers = FALSE
          , dep.var.labels = c("Time btw. initiation and termination")
          , column.labels = c("symetric bicameralism <br> w/controls", "asymetric  bicameralism <br> w/controls")
          , covariate.labels = c("Potential 2$^{nd}$ Chamber Change"
                                 , "Election in the next three months (=1)"
                                 , "Leading committee chair is of an opposition party (=1)"
                                 , "Bill passed through the legislative process (=1)"
                                 
                                 , "Legislative Periode 15 (=1)"
                                 , "Legislative Periode 16 (=1)"
                                 , "Legislative Periode 17 (=1)"
          )
          , intercept.bottom = TRUE
          , notes.append = FALSE
          , star.cutoffs = c(0.1, 0.05, 0.01)
          , nobs = TRUE
          , omit.stat = c("ll","theta")
          , notes = c("$^{***}$p $<$ .01; $^{**}$p $<$ .05; $^{*}$p $<$ .1 <br> Reference Legislative Periode 14")
          , no.space=FALSE
          , single.row=FALSE
          , out = "Outcome/App4-1_Models3.htm")
 