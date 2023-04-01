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
chamber2 <- read.csv2("Data/SecChamber20200701.csv") 

# 1. Prepare data for plot #
############################

# For this script to better monitor data reduce to variables
# necessary for the plot 
dat <- subset(dat, select = c("date_bt_ini",
                              "potLoss_same90",
                              "potGain_same90",
                              "lp"))

# Check data with a quick table. We should find:   
# "...potential second chamber change indicates whether the 
# federal government expects a potential loss (-1), a 
# potential gain (1) or neither a loss nor a gain (0) in the 
# second chamber between time t0 and t1 [...] Overall, we 
# have 71 bills considered in the first chamber under the 
# threat of a potential majority loss in the second 
# chamber (-1) and 49 bills considered under the impression 
# of a potential majority gain (1). The remaining 1,846 bills 
# are coded 0."
# Hence, we shold find 0,0 = 1846; 1,1 = 0
# 0,1 = 49; 1,0 = 71
table(dat$potGain_same90, dat$potLoss_same90)

# Make sure data variables are really data format
dat$date_bt_ini <- as.Date(dat$date_bt_ini)
chamber2$date <- as.Date(chamber2$date)

# Reduce the 2nd chamber datat to 14th - 16th leg. period 
# For this we use the election date "Tag der Wahl" from
# the official data handbook by the German Bundestag
# https://www.bundestag.de/resource/blob/196080/805ecd29a2deeb3050390aa4100200ef/Kapitel_01_02_Tag_der_Wahl_zum_Bundestag-data.pdf
# Reduce to election date at the end of 13th legislative period 
chamber2 <- subset(chamber2, chamber2$date >= as.Date("1998-09-27"))
# Reduce to election date at the end of 17th legislative period 
chamber2 <- subset(chamber2, chamber2$date <= as.Date("2013-09-22"))

# Next, we use a trick. To include the tick-marks of all laws
# in the figure we define a continues variable as -1.75. This
# will allow to simply plot those tick-marks at y = -1.75
dat$laws <- -1.75

# 2. Compute Figure 3 (as PDF) #
################################
pdf("Outcome/Figure3.pdf", width=10, height=6)   
par(mar=c(3, 4, 0, 0) + 1)   
plot(chamber2$R ~ chamber2$date
     , las = 1
     , type ="n"
     , ylab ="Votes of parties in the federal government \n in the second chamber"
     , ylim = c(-1,69)
     , yaxt="n"
     , xlab = "" 
     , xlim = c(as.Date("1998-09-27", "%Y-%m-%d"), as.Date("2013-09-22", "%Y-%m-%d"))
     , xaxt="n"
     , frame.plot=F
  )
  
  # Temporal x-axis
  axis(1, at = c(
    as.Date("1998-09-01", "%Y-%m-%d")
    , as.Date("1999-01-01", "%Y-%m-%d")
    , as.Date("2000-01-01", "%Y-%m-%d")
    , as.Date("2001-01-01", "%Y-%m-%d")
    , as.Date("2002-01-01", "%Y-%m-%d")
    , as.Date("2003-01-01", "%Y-%m-%d")
    , as.Date("2004-01-01", "%Y-%m-%d")
    , as.Date("2005-01-01", "%Y-%m-%d")
    , as.Date("2006-01-01", "%Y-%m-%d")
    , as.Date("2007-01-01", "%Y-%m-%d")
    , as.Date("2008-01-01", "%Y-%m-%d")
    , as.Date("2009-01-01", "%Y-%m-%d")
    , as.Date("2010-01-01", "%Y-%m-%d")
    , as.Date("2011-01-01", "%Y-%m-%d")
    , as.Date("2012-01-01", "%Y-%m-%d")
    , as.Date("2013-01-01", "%Y-%m-%d")
    , as.Date("2013-09-30", "%Y-%m-%d"))
    , labels = c(
      "9/1998"
      , "1999"
      , "2000"
      , "2001"
      , "2002"
      , "2003"
      , "2004"
      , "2005"
      , "2006"
      , "2007"
      , "2008"
      , "2009"
      , "2010"
      , "2011"
      , "2012"
      , "2013"
      , "9/2013"
        ), las  = 2)
  
  # y-acis lables
  axis(2, at = c(0, 15, 35, 55, 69), labels = TRUE, las  = 1)
  
  # Legislative periods are based on the election date 
  # "Tag der Wahl" from the official data handbook by the 
  # German Bundestag
  # https://www.bundestag.de/resource/blob/196080/805ecd29a2deeb3050390aa4100200ef/Kapitel_01_02_Tag_der_Wahl_zum_Bundestag-data.pdf 
  
  # Legislative Period 14
  segments(as.Date("2002-09-22", "%Y-%m-%d"), 55
           ,  as.Date("2002-09-22", "%Y-%m-%d")
           , 69, col="gray70", lty=2, lwd=1, lend=0)
  text(as.Date("2000-10-15", "%Y-%m-%d"), 69
       , "Legislative Period 14\n SPD & Gruene"
       , pos = 1, srt=0, xpd = TRUE, cex = .85, col = "black")
  # Legislative Period 15
  text(as.Date("2004-04-01", "%Y-%m-%d"), 64
       , "Legislative Period 15\nSPD & Gruene"
       , pos = 1, srt=0, xpd = TRUE, cex = .85, col = "black")
  # Legislative Period 16
  segments(as.Date("2005-09-18", "%Y-%m-%d"), 55
           ,  as.Date("2005-09-18", "%Y-%m-%d")
           , 69, col="gray70", lty=2, lwd=1, lend=0)
  text(as.Date("2007-10-15", "%Y-%m-%d"), 69
       , "Legislative Period 16\nCDU/CSU & SPD"
       , pos = 1, srt=0, xpd = TRUE, cex = .85, col = "black")
  segments(as.Date("2009-09-27", "%Y-%m-%d"), 55
           ,  as.Date("2009-09-27", "%Y-%m-%d")
           , 69, col="gray70", lty=2, lwd=1, lend=0)
  # Legislative Period 17
  segments(as.Date("2013-09-22", "%Y-%m-%d"), 55
           ,  as.Date("2013-09-22", "%Y-%m-%d")
           , 69, col="gray70", lty=2, lwd=1, lend=0)
  text(as.Date("2011-10-15", "%Y-%m-%d"), 64
       , "Legislative Period 17\nCDU/CSU & FDP"
       , pos = 1, srt=0, xpd = TRUE, cex = .85, col = "black")
  
  # Bills introduced under potential gain in legislative period 14
  rect(as.Date(min(dat$date_bt_ini[dat$potGain_same90 == 1 & dat$lp==14]))
       , chamber2$R[chamber2$date == min(dat$date_bt_ini[dat$potGain_same90 == 1 & dat$lp==14])]-.2
       , as.Date(max(dat$date_bt_ini[dat$potGain_same90 == 1 & dat$lp==14]))
       , -2.4
       , border= c("grey80"), lty= NULL, col = c("grey80"), xpd=FALSE)
  # Bills introduced under potential loss in legislative period 14
  rect(as.Date(min(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==14]))
       , chamber2$R[chamber2$date == min(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==14])]-.2
       , as.Date(max(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==14]))
       , -2.4
       , border= c("grey40"), lty= NULL, col = c("grey40"), xpd=FALSE)

  # Remember: No potential gain/loss in legislative period 15
  # Remember: No potential gain in legislatibe 16
  
  # Bills introduced under potential loss in legislative period 16
  rect(as.Date(min(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==16]))
       , chamber2$R[chamber2$date == min(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==16])]-.2
       , as.Date(max(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==16]))
       , -2.4
       , border= c("grey40"), lty= NULL, col = c("grey40"), xpd=FALSE)
  
  # Bills introduced under potential gain in legislative period 17
  rect(as.Date(min(dat$date_bt_ini[dat$potGain_same90 == 1 & dat$lp==17]))
       , chamber2$R[chamber2$date == min(dat$date_bt_ini[dat$potGain_same90 == 1 & dat$lp==17])]-.2
       , as.Date(max(dat$date_bt_ini[dat$potGain_same90 == 1 & dat$lp==17]))
       , -2.4
       , border= c("grey80"), lty= NULL, col = c("grey80"), xpd=FALSE)
  
  # Bills introduced under potential loss in legislative period 17
  rect(as.Date(min(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==17]))
       , chamber2$R[chamber2$date == min(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==17])]-.2
       , as.Date(max(dat$date_bt_ini[dat$potLoss_same90 == 1 & dat$lp==17]))
       , -2.4
       , border= c("grey40"), lty= NULL, col = c("grey40"), xpd=FALSE)
  
  # Include the solide line of feder government votes in 
  # the second chamber
  lines(chamber2$R ~ chamber2$date, lwd = 3)
  
  # Include dotted line at 35 which is the majority threshold
  # The line starst at 01/01/1998 even so that is not in the 
  # data just so the line reaches in the y-axis to enable
  # an easier reading of the data.
  segments(as.Date("1998-01-01", "%Y-%m-%d"), 35
           , as.Date("2013-09-22", "%Y-%m-%d"), 35
           , col="black", lty=3, lwd=2, lend=0)
    text(as.Date("2013-09-22", "%Y-%m-%d"), 35
         , labels = "majority \n threshold"
         , srt = 360,  xpd = TRUE, cex=1)
 
  # Include the tick-marks for all laws in our data
  # These are all laws introduced in the first 
  # chamber by the federal governemt and as plot date
  # the date is used that a bill was introduced to the 
  # first chamber
  points(dat$date_bt_ini, dat$laws, pch = "|")

dev.off()
