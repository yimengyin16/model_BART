# Actuarial valuation for BART

rm(list = ls())
source("libraries.R")


#*******************************************************************************
#                           ### Valuation parameters ####                      
#*******************************************************************************
## File path of the run control file

# Path to run control file
dir_runControl <- "model/"
fn_runControl  <- "RunControl.xlsx"
filePath_runControl <- paste0(dir_runControl, fn_runControl)

# Path to amortization and asset smoothing info
dir_planInfo <- "inputs/data_proc/"
filePath_planInfo <- paste0(dir_planInfo, "Data_BART_planInfo_AV2019.RData")

# Output folder  
dir_outputs_val <- "model/valuation/outputs_val/"

# Load valuation parameters
val_runList <- read_excel(filePath_runControl, sheet="params_val", skip  = 3) %>% 
  filter(include == TRUE)


#*******************************************************************************
#                    ### Run valuations   ####                      
#*******************************************************************************
source("model/valuation/model_val_01_master_multiTier.R")


# val_name_run <- val_runList$val_name[1]

for (val_name_run in val_runList$val_name){
  create_val(val_name_run)
}










