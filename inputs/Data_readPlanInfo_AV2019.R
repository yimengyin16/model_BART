# Load the following plan information of PERF A:
 # Schedule of amortization payments for existing UAALs 



## Inputs
#   - inputs/data_raw/Data_BART_planInfo_AV2019.xlsx"


## Outputs
#  - All tables in "tidy" format  





#*******************************************************************************
#                      ## Tools ####
#*******************************************************************************
# source("libraries.R")




#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_dataRaw  <- "inputs/data_raw/"
fn_dataRaw   <- "Data_BART_planInfo_AV2019.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"


names_sheet <- excel_sheets(filePath_dataRaw)



#*******************************************************************************
#                      ## Initial amortization payments ####
#*******************************************************************************
init_amort_misc_raw <- 
  read_excel_range(filePath_dataRaw, "Init_amort_misc")$df %>% 
  mutate(grp = "misc") %>% 
  relocate(grp)

init_amort_sfty_raw <- 
  read_excel_range(filePath_dataRaw, "Init_amort_sfty")$df %>% 
  mutate(grp = "sfty") %>% 
  relocate(grp)





#*******************************************************************************
#                      ## Unrecognized investment gains/losses  ####
#*******************************************************************************
init_unrecReturns.unadj <- read_excel_range(filePath_dataRaw, "Init_unrecReturn")
init_unrecReturns.unadj <-
  init_unrecReturns.unadj$df %>% 
  mutate(AV_date = init_unrecReturns.unadj$tblInfo$AV_date)



#*******************************************************************************
#                      ## Benefit factor  ####
#*******************************************************************************

benFactor_misc <-  read_excel_range(filePath_dataRaw, "benFactor_misc")
benFactor_sfty <-  read_excel_range(filePath_dataRaw, "benFactor_sfty")



#*******************************************************************************
#                      ## Save Data ####
#*******************************************************************************

save(init_amort_misc_raw,
     init_amort_sfty_raw,
		 init_unrecReturns.unadj,
		 
		 benFactor_misc,
		 benFactor_sfty,
		 
		 file = paste0(dir_dataOut, "Data_BART_planInfo_AV2019.RData"))

