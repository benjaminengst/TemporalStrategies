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
# Load our prepared GESTA data
gesta <- read.csv2("GESTA.csv")

# Load the dataset in which we summarize the potential 
# of a federal election to either lead to a gain or loss
states <- read.csv2("LegTimePotential.csv")

sort(unique(states$R.Lager))

# The governments hypothetical view in the future # 
###################################################
# This way we add a potential gain or loss

# Hypothetical view 90 days into the future #
#-------------------------------------------#
# To the days a bill is submitted to the Bundestag 
# by the federal government we add 90 days. 
gesta$hyp_bt_loss <- as.Date(gesta$date_bt_ini) + 90
gesta$hyp_bt_loss <- as.Date(gesta$hyp_bt_loss, "%Y-%m-%d") 
# The aim is to check whether bettween the submission of
# a bill to the Bundestag (bt_ini) and the hypothetical
# 90 days (hyp_bt_loss) there was any election that could
# lead the government to lose seats in the second chamber.

# This is the variable that will later take the value 
# of 1 if a bill was exposed to the possibility of a 
# potentiall loss or 0 otherwise
gesta$potLoss_same <- NA
# The assignment is done in the following loop

# Loop to each entry in the gesta dataset 
for (i in 1:nrow(gesta)) {
  # For each run that a variable
  # in states to 0 to later 
  # identify state elections that
  # are relevant to an entry in gesta
  states$important <- 0
  # For each entry in gesta loop through
  # each entry in the states dataset
  for (k in 1:nrow(states)) {
    # If the date of a state election in states is located
    # between the data when a bill was presented in GESTA
    # (bt_ini) and between the hypothetical length from
    # the 90 days (hyp_bt_loss) then set important variable
    # to 1. This identifies all state elections that are
    # relevant to a respective entry in GESTA addressed 
    # in a run of the loop. 
    states$important[k] <- ifelse(
      ( as.Date(gesta$date_bt_ini, "%Y-%m-%d")[i] < as.Date(states$Date, "%Y-%m-%d")[k] ) & 
        ( as.Date(states$Date, "%Y-%m-%d")[k] < as.Date(gesta$hyp_bt_loss, "%Y-%m-%d")[i] ), 
      1,
      states$important[k]
    )
    
  }
  
  # Now subset to the elections that are relevant 
  # to one entry in the GESAT dataset
  relevance <- subset(states, states$important == 1)
  # We create this variable for a check. This is 
  # just an additional check. What does it do?
  # Our list of elections only accounts for a 
  # maximum of 8 elections at the same time.
  # We later what to be able to check that
  # there are not more than that.
  relevance_count <- nrow(relevance)
  
  # Now check that there is at least one  state election
  # for an entry and check that there are less than 9 
  # state elections (for the reason just mentioned).
  if(nrow(relevance) != 0 & nrow(relevance) < 9) {  
    
    # If there is at least one state election for an
    # entry in GESTA (and less than 9) the do the following:
    # Take the line from the relevance dataset and subset
    # to the variables that incate whether there was 
    # a potential loss or gain
    relevance <- relevance[1, ] 
    relevance <- subset(relevance, select = c("potGain1", "potGain2", "potGain3", "potGain4", "potGain5", "potGain6", "potGain7", "potGain8"
                                              ,"potLoss1", "potLoss2", "potLoss3", "potLoss4", "potLoss5", "potLoss6", "potLoss7", "potLoss8"))
    
    # Now select the column among losses that indicates 
    # how many state elections there where for an entry
    # in GESTA. As the first part of the dataframe includes
    # the 8 cases for a potential gain but in this moment
    # we only focuse on losses we add 8 to the count
    # to "jump over" the gain variables.
    # Finally the value in the column identified is the one
    # for a potential loss (either 0 or 1) and assigned
    # to the respective entry in GESTA currently addressed
    # in the loop. 
    gesta$potLoss_same[i] <- relevance[relevance_count + 8]
    
  }else{
    
    # When relevance is empty than assign the potential loss
    # a zero as there was no state election coming up. 
    # Also now our check that the hypothetical eight elections
    # we estimate in our states dataset are enough. If this 
    # is not the case and for a law there where more 
    # than 8 elections write a 2 in potential loss so 
    # we can look at these entries (check)
    gesta$potLoss_same[i] <- ifelse(nrow(relevance) > 8, 2, 0) 
    
  }
  
  # This prints the run of each loop. 
  # Just to get some feedback the loop is running.
  print(paste(i, "Loss_same"))
  
}

# Rename the variable to match our common pattern used
gesta$potLoss_same90 <- as.numeric(gesta$potLoss_same)
  gesta$potLoss_same <- NULL 
# Next we check taht there are only 0 and 1 and no 2 (the
# little check we had implemented that the hypothetical
# combinations we calcualted based of at max 8 elections
# that effect one law is enoug:
table(gesta$potLoss_same90) # There are no 2

# Now we do the the same for a potential gain #
#---------------------------------------------#
# The time window is the same which is 90 days
gesta$hyp_bt_gain <- gesta$hyp_bt_loss 
  gesta$hyp_bt_gain <- as.Date(gesta$hyp_bt_gain, "%Y-%m-%d") 

# Variable to safe the data   
gesta$potGain_same <- NA

# The loop functions the same 
# way as the previous loop
# (the only exception is commented)
for (i in 1:nrow(gesta)) {
  
  states$important <- 0
  
  for (k in 1:nrow(states)) {
    
    states$important[k] <- ifelse(
      
      ( as.Date(gesta$date_bt_ini, "%Y-%m-%d")[i] < as.Date(states$Date, "%Y-%m-%d")[k] ) & 
        ( as.Date(states$Date, "%Y-%m-%d")[k] < as.Date(gesta$hyp_bt_gain, "%Y-%m-%d")[i] ), 
      1,
      states$important[k]
    )
    
  }
  
  relevance <- subset(states, states$important == 1)
  relevance_count <- nrow(relevance) 
  
  if(nrow(relevance) != 0 & nrow(relevance) < 9) {   
    
    # If there is at least one state election for an
    # entry in GESTA (and less than 9) the do the following:
    # Take the line from the relevance dataset and subset
    # to the variables that incate whether there was 
    # a potential loss or gain
    relevance <- relevance[1,]
    relevance <- subset(relevance, select = c("potGain1", "potGain2", "potGain3", "potGain4", "potGain5", "potGain6", "potGain7", "potGain8",
                                              "potLoss1", "potLoss2", "potLoss3", "potLoss4", "potLoss5", "potLoss6", "potLoss7", "potLoss8"))
    
    # Now select the column among gains that indicates 
    # how many state elections there where for an entry
    # in GESTA. These are the first 8 variables in the
    # dataset which is why this time we do not need to 
    # add 8 to the count
    # Finally the value in the column identified is the one
    # for a potential gain (either 0 or 1) and assigned
    # to the respective entry in GESTA currently addressed
    # in the loop. 
    gesta$potGain_same[i] <- relevance[relevance_count]
    
  }else{
    
    gesta$potGain_same[i] <- ifelse(nrow(relevance) > 8, 2, 0) 
    
  }
  
  print(paste(i, "Gain_same"))
  
}

# Rename the variable to match our common pattern used
gesta$potGain_same90 <- as.numeric(gesta$potGain_same)
  gesta$potGain_same <- NULL 
# Next we check taht there are only 0 and 1 and no 2 (the
# little check we had implemented that the hypothetical
# combinations we calcualted based of at max 8 elections
# that effect one law is enoug:
table(gesta$potGain_same90) # There are no 2

# We don't need the following variables anymore
gesta$hyp_bt_gain <- NULL
gesta$hyp_bt_loss <- NULL

# Now our dataset is done. In the script "A. GESTA_prepare.R"
# we had selected the variables and data from GESTA we need 
# for our replication and in this script "B. GESTA_potential.R"
# we added the potential gain / loss or neither to the 
# dataset. This is the dataset which can be used to
# replicate the findings from Appendix App.5.1 and App.5.2
# This can be done with the script "9. Analysis_App5-1_5-2.csv"
# from the main folder using the data created here which
# is safed as "LegTimeGESTA.csv" in the next line. 

# Safe the data created #
#-----------------------#
write.csv2(gesta, file = "LegTimeGESTA.csv")

# Instead of now moving datasets we did something else here.
# We already prepared the GESTA data in the folder 
# "Data/LegTimeGESTA20200701.csv" for the analysis.
# Thus instead of copying data back and forth we can test
# whether the data created here is identical to the data
# provided in our per-prepared dataset in the folder "Data." 

# Check that replication data is identical to data use #
########################################################

# We open the data we use for the analysis from the Data fodler
analysis <- read.csv2("../Data/LegTimeGESTA20200701.csv")

# Order the data in the gesta dataset we just created and
# in the dataset we use for the analysis using the number
analysis <- analysis[order(analysis$nummer),]
gesta <- gesta[order(gesta$nummer),]

# Next we compare every variable and all the values need
# to be exactly identical and there can never be a "FALSE"
# in the following table comparing all 11 variables
table(analysis$nummer == gesta$nummer) # Always TRUE
table(analysis$lp == gesta$lp) # Always TRUE
table(analysis$date_bt_ini == gesta$date_bt_ini) # Always TRUE
table(analysis$date_bt_out == gesta$date_bt_out) # Always TRUE
table(analysis$phase_total == gesta$phase_total) # Always TRUE
table(analysis$zustimmg == gesta$zustimmg) # There are NAs but no FALSE
table(analysis$leadingcom == gesta$leadingcom) # Always TRUE
table(analysis$completed == gesta$completed) # Always TRUE
table(analysis$election == gesta$election) # Always TRUE
table(analysis$potLoss_same90 == gesta$potLoss_same90) # Always TRUE
table(analysis$potGain_same90 == gesta$potGain_same90) # Always TRUE

# All values are always true. Thus, the datasets are exactly
# identical and the replication was successful and the GESTA
# assessment will lead to the same results as shown in Appendix 5 
