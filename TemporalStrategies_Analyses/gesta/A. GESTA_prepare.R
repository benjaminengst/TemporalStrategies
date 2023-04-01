# Paper: Temporal Strategies
# Journal: Legislative Studies Quarterly
# Authors: Christoph Garwe [1] / Benjamin G. Engst [2] / Yannick Stawicki [3] / Christoph Hönnige [1]
#   [1] Leibniz University Hannover; [2] University of Mannheim; [3] Political Affairs Consultant, Barmer (Lower Saxony)

# Clear matrix #
################
rm(list = ls())

# Libraries #
#############
library("foreign") # Version 0.8-71

# Session Info on system used #
###############################
sessionInfo()

# R version 3.5.3 (2019-03-11)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Mojave 10.14.6
#---------------------------------------------#
# Attached packages:
# stats; graphics; grDevices; utils; datasets; methods; base
# foreign_0.8-71
#----------------------------------------------#

# Set automatically to current working-directory #
##################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the data #
#################
# Please download the GESTA dataset at GESIS 
# Leibniz Institute for the Social Sciences
# ZA4569: https://doi.org/10.4232/1.4569
# and add the .dta dataset to this  
# replication folder called "gesta"
# Afterwards execute this script and 
# the script "B. GESTA_potential.R"
gesta <- read.dta("ZA4569.dta")

# Create the datasset of interest #
###################################
# Subset to Legislative periods 8 to 15 as there is 
# to much missing information in legislativ period 17
unique(gesta$lp)
gesta <- subset(gesta, lp >= 8)
# Reduce to bills introduced by the federal government 
# (initative = 2) or governing parties (inbtkat = 3)
gesta <- subset(gesta, initiative == 2 | inbtkat == 3)
# Reduce to bills that made it through the third reading
# in the Bundestag
  table(is.na(gesta$dbbt1)) # 268 were not completed 
gesta <- subset(gesta, !is.na(gesta$dbbt1))
# In two cases we do not know the date a bill was presented.
# We have no other option than to drop those.
table(is.na(gesta$deinbt))
gesta <- subset(gesta, !is.na(gesta$deinbt))

# Check how many of those were not published  
# We use a check whether a publication date exists
table(is.na(gesta$dv))
# The once who where not anounced (=0) using another variable
table(gesta$verk,useNA = "always") # this leads to a difference of 1

# Calculate the equibalent to our legislative length variable #
###############################################################
# We use the dates on the GESTA variables deinbt & dbbt1
gesta$phase_total <- as.Date(gesta$dbbt1, "%Y-%m-%d") - as.Date(gesta$deinbt, "%Y-%m-%d")
gesta$phase_total <- as.numeric(gesta$phase_total) # This is our phase_total variable
  summary(gesta$phase_total)

# Subset the final data to the variables we need #
#------------------------------------------------#
gesta <- subset(gesta, select = c("nummer" # idtentifier in GESTA
                                   , "lp"
                                   , "deinbt" # our date_bt_ini
                                   , "dbbt1"  # our date_bt_out
                                   , "phase_total"
                                   , "zudich" # our zustimmg 
                                   , "bereich" # Ministerial to compute our leadingcom
                                   , "verk" # our completed
                                   ))

# Rename to match our dataset #
#-----------------------------#
colnames(gesta)[3] <- "date_bt_ini"
 table(is.na(gesta$date_bt_ini))
colnames(gesta)[4] <- "date_bt_out"  
 table(is.na(gesta$date_bt_out))
colnames(gesta)[6] <- "zustimmg" 
  table(is.na(gesta$zustimmg))
colnames(gesta)[7] <- "leadingcom" 
  table(is.na(gesta$leadingcom))
colnames(gesta)[8] <- "completed" 
  table(is.na(gesta$completed))
   
# Election variable #
#-------------------#
# Add a variable to include a dummy for the election coming 
# up. For this we use the election date "Tag der Wahl" from
# the official data handbook by the German Bundestag
# https://www.bundestag.de/resource/blob/196080/805ecd29a2deeb3050390aa4100200ef/Kapitel_01_02_Tag_der_Wahl_zum_Bundestag-data.pdf
# und https://www.bundestag.de/resource/blob/273012/39260d19455ef821affdee3a09588c20/Kapitel_23_Statistische_Gesamt__bersicht_1______11__Wahlperiode-pdf-data.pdf    
gesta$lp_date <- NA  
  gesta$lp_date <- ifelse(gesta$lp == 8, as.character("1980-10-05"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 9, as.character("1983-03-06"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 10, as.character("1987-01-25"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 11, as.character("1990-12-02"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 12, as.character("1994-10-16"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 13, as.character("1998-09-27"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 14, as.character("2002-09-22"), as.character(gesta$lp_date))  
  gesta$lp_date <- ifelse(gesta$lp == 15, as.character("2005-09-18"), as.character(gesta$lp_date))  

  # Check whether laws left the Bundestag after the end of 
  # the lp and this is never the case accodring to the test
  gesta$nummer[as.Date(gesta$date_bt_out) > as.Date(gesta$lp_date)]
    
  # Dummy that indicates 3 month (90 days) before an election #
  #-----------------------------------------------------------#
  gesta$rangelp_date <- as.Date(gesta$lp_date) - 90
  # Create the variable election  
  gesta$election <- ifelse(
    ( as.Date(gesta$rangelp_date) < as.Date(gesta$date_bt_ini) &
      as.Date(gesta$lp_date) > as.Date(gesta$date_bt_ini)  ),
      1,0
    )
  # Delete the variables that we only
  # needed to create this variable
  gesta$rangelp_date <- NULL
  gesta$lp_date <- NULL
table(gesta$election)# Descriptives of the election variable   

# Creating the leadingcom variable #
#----------------------------------#
# We aggregate this variable as good as possible from the 
# GESTA variable bereich to match our personal aggregation
# from the main analyses based on the committees
table(gesta$leadingcom)
  
  # Angelegenheiten der neuen Länder == Innen
  gesta$leadingcom[gesta$leadingcom == 1] <- 14
  gesta$leadingcom[gesta$leadingcom == 3] 
  gesta$leadingcom[gesta$leadingcom == 4] 
  # Bildung == Bildung
  gesta$leadingcom[gesta$leadingcom == 5] <- 4
  # Deutsche Einheit == Innen
  gesta$leadingcom[gesta$leadingcom == 6] <- 14
  # Familien, Frauen, Jugend == Bildung
  gesta$leadingcom[gesta$leadingcom == 8] <- 4
  gesta$leadingcom[gesta$leadingcom == 10] <- 4
  # Familien, Frauen, Jugend == Bildung
  gesta$leadingcom[gesta$leadingcom == 11] <- 4
  # Gesundheit == Arbeit & Soziales
  gesta$leadingcom[gesta$leadingcom == 12] <- 2
  # Innerdeutsche Beziehungen == Innen
  gesta$leadingcom[gesta$leadingcom == 13] <- 14
  # Jugend, Familie, Gesundheit == Arbeit & Soziales
  gesta$leadingcom[gesta$leadingcom == 15] <- 2
  # Kultur = Bildung
  gesta$leadingcom[gesta$leadingcom == 17] <- 4
  # Post == Verkehr
  # Attention: To later make the reference I relevel Transportation etc. to 1
  gesta$leadingcom[gesta$leadingcom == 18] <- 1
  # Bauwesen == Verkehr 
  gesta$leadingcom[gesta$leadingcom == 19] <- 1
  # Schutz Kind == familie
  gesta$leadingcom[gesta$leadingcom == 20] <- 4
  # Umwelt Naturschutz
  gesta$leadingcom[gesta$leadingcom == 21]
  # Attention: To later make the reference I relevel Transportation etc. to 1
  gesta$leadingcom[gesta$leadingcom == 22] <- 1
  # Verteidigung == Auswärtiges
  gesta$leadingcom[gesta$leadingcom == 23] <- 3
  # Völkerrechtliche Vereinbarungen == Auswärtiges
  gesta$leadingcom[gesta$leadingcom == 24] <- 3
  # Zusammenarbeit & Entwicklung == Auswärtiges
  gesta$leadingcom[gesta$leadingcom == 26] <- 3
  # Verkehr, Post, Fernmeldewesen
  gesta$leadingcom[gesta$leadingcom == 27] <- 1
  # Wirtschaft & Arbeit == Arbeit & Soziales 
  gesta$leadingcom[gesta$leadingcom == 28] <- 2
  # Gesundheit == Arbeit & Soziales 
  gesta$leadingcom[gesta$leadingcom == 29] <- 2
  
table(gesta$leadingcom)

# Safe the data #
#---------------#
write.csv2(gesta, file = "GESTA.csv", row.names = FALSE)

# The variable of potential gain or loss is added in 
# the next script called "B.GESTA_potential.R"
