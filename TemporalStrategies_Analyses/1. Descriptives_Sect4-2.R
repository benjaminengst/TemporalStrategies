# Paper: Temporal Strategies
# Journal: Legislative Studies Quarterly
# Authors: Christoph Garwe [1] / Benjamin G. Engst [2] / Yannick Stawicki [3] / Christoph Hönnige [1]
#   [1] Leibniz University Hannover; [2] University of Mannheim; [3] Political Affairs Consultant, Barmer (Lower Saxony)

# Clear matrix #
################
rm(list = ls())

# Session Info on system used #
###############################
sessionInfo()

# R version 3.5.3 (2019-03-11)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6
#---------------------------------------------#
# Attached packages:
# stats; graphics; grDevices; utils; datasets; methods; base
#----------------------------------------------#

# Set automatically to current working-directory #
##################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data #
#################
dat <- read.csv2("Data/LegTime20200701.csv")

# Replicated information #
##########################

# 1. Section 4.2 # 
#----------------#
# "... our dataset contains information on 1,966 government 
# bills ..."  
length(unique(dat$docno))

# "... 41 did not become law ..."
table(dat$completed)  

# "...968 of those bills are consent bills mirroring 
# symmetric bicameralism and 998 are objection bills..."
table(dat$zustimmg) # 0 = objection bill; 1 = consent bill
  
# NOTE: The Gesta replication is done in an extra script
# which is part of the replications files
# See: 9. Analysis_App5-1_5-2.R

# 2. Section 4.2.1 #
#------------------#
# "The legislative length in the first chamber ranges 
# from 2 to 1,149 days."
min(dat$phase_total); max(dat$phase_total)

# "The mean time that a bill takes to pass in the first 
# chamber is about 79 days, and the median length is 55 days."
round(mean(dat$phase_total)); round(median(dat$phase_total))

# Replication of Figure 2 (as PDF) # 
pdf("Outcome/Figure2.pdf", width=8, height=7); par(mar=c(4, 4 , 1, 0))
hist(dat$phase_total
     , breaks=seq(0,1200,length=55)
     , ylim=c(0,800)
     , xlim = c(1,500)
     , main = ""
     , xlab="Days"
     , col = "lightgray"
     , border="gray"
     , las = 1
) 

segments(mean(dat$phase_total), 0, mean(dat$phase_total), 710, col="black", lty=1, lwd=2, lend=0)
segments(mean(dat$phase_total) - sd(dat$phase_total), 700, mean(dat$phase_total) + sd(dat$phase_total), 700, col="black", lty=1, lwd=2, lend=0)
segments(median(dat$phase_total), 0, median(dat$phase_total), 785, col="black", lty=3, lwd=2, lend=0)

text(mean(dat$phase_total), 720, labels = paste("Mean:", round(mean(dat$phase_total),0) ,"days"), srt = 360,  xpd = TRUE, cex=1)
text(median(dat$phase_total), 800, labels = paste("Median:", round(median(dat$phase_total),0) ,"days"), srt = 360,  xpd = TRUE, cex=1)
text(mean(dat$phase_total) + sd(dat$phase_total) + 50, 700, labels = paste("SD:", round(sd(dat$phase_total),0) ,"days"), srt = 360,  xpd = TRUE, cex=1)

text(450, 50, labels = paste("max.", round(max(dat$phase_total),0) ,"days"), srt = 360,  xpd = TRUE, cex=1)
dev.off()     

# 3. Section 4.2.2 #
#------------------#
# "...potential second chamber change indicates whether the 
# federal government expects a potential loss (-1), a 
# potential gain (1) or neither a loss nor a gain (0) in the 
# second chamber between time t0 and t1 [...] Overall, we 
# have 71 bills considered in the first chamber under the 
# threat of a potential majority loss in the second 
# chamber (-1) and 49 bills considered under the impression 
# of a potential majority gain (1). The remaining 1,846 bills 
# are coded 0."
dat$independent <- 0
  dat$independent[dat$potGain_same90 == 1] <- 1
  dat$independent[dat$potLoss_same90 == 1] <- -1
table(dat$independent)

# NOTE: Figure 3 is computed in an extra script
# which is part of the replications files;
# see: 2. Figure3.R  

# 3. Section 4.2.3 #
#------------------#
# "... we include an indicator variable showing whether a 
# bill is considered within 90 days prior to a federal 
# election. Two out of our 1,966 bills were considered in 
# this period..." 
table(dat$election) 

# "...Overall, 651 of our 1,966 bills were considered in 
# committees chaired by the opposition..." 
table(dat$compresOpp) 

# "...we also control for the 41 bills that did not become law..."
table(dat$completed) 

# "...Eventually, we control for ten different policy areas 
# in our analysis..."
table(dat$leadingcom) # 1 to 13 but 4, 9, 11 are not assigned

# "...Overall, 510 bills were considered during the 14th 
# legislative period, 367 during the 15th, 574 during the 
# 16th, and 515 bills were considered during the 17th 
# legislative period..."
table(dat$lp)
