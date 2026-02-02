# note: this script is based on scripts for capr prl data, which passes through a cleaning function
# to produce clean long format ready to do computational modelling, but also gets
# a wide format win or loose and stay or stay rates

#Need to set these to your personal paths
setwd("") #set to the folder with scripts that I want this to pull from
path <- "" #set to subfolder where individual participant CSV files are located
file.names <- dir(path, pattern =".csv")

prl_col <- data.frame()

for (i in 1:length(file.names)) {
  
  temp_prl <- read.csv(paste(path,"/",file.names[i],sep=""),header=TRUE, sep=",", stringsAsFactors=FALSE)
  temp_prl$success <- as.logical(temp_prl$success)
  temp_prl$timeout <- as.logical(temp_prl$timeout)
  temp_prl$reward_type <- as.logical(temp_prl$reward_type)
  temp_prl$reversal_type <- as.logical(temp_prl$reversal_type)
  
  #prl_col <- rbind(prl_col, temp_prl)
  prl_col <- bind_rows(prl_col, temp_prl)
  
}

# prl_col <- subset(prl_col, !is.na(PROLIFIC_PID))

source("cleanPrl.R")
source("winLossSwitchStay.R")

prl_long_wide_clean <- cleanPrl(prl_col, p_threshold = .05)
prl_long <- prl_long_wide_clean$lf
prl_wide <- prl_long_wide_clean$wf


IDs <- read.csv("../ESB Phase 1.csv")
IDlist <- IDs$ID

prl_long <- prl_long %>%
  filter(PROLIFIC_PID %in% IDlist)

prl_wide <- prl_wide %>%
  filter(PROLIFIC_PID %in% IDlist)


# write.csv(prl_long, "PRLLongData_20251006.csv", row.names=F)
# write.csv(prl_wide, "PRLWideData_20251006.csv", row.names=F)
