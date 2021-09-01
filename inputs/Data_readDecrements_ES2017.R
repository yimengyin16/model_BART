## Loading decrement tables salary scales from the 2017 experience study for CalPERS


## Inputs
#   - inputs/data_raw/Data_BART_decrements_ES2017.xlsx"


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
fn_dataRaw   <- "Data_BART_Decrements_ES2017.xlsx" 
filePath_dataRaw <- paste0(dir_dataRaw, fn_dataRaw)

dir_dataOut <- "Inputs/data_proc/"


names_sheet <- excel_sheets(filePath_dataRaw)




#*******************************************************************************
#                      ## Importing service retirement rates ####
#*******************************************************************************

# Local helper function
get_servRetRates <- function(sheetName, fileName = filePath_dataRaw){
  
  ls <- read_excel_range(fileName, sheetName) 
  
  var_name <- ls$tblInfo$var_name # ls$tblInfo[ls$tblInfo$var == "var_name", "value"][[1]] 
  grp_name <- ls$tblInfo$grp_name # ls$tblInfo[ls$tblInfo$var == "grp_name", "value"][[1]] 
  
  
  ls$df %>% 
    gather(yos, value, -age) %>% 
    rename(!!var_name := value) %>% 
    mutate(grp = grp_name) %>% 
    select(grp, everything())
}



sheetNames_serRet <- names_sheet[str_detect(names_sheet, "servRet")]

df_qxr_raw <- map(sheetNames_serRet, get_servRetRates) %>% 
  bind_rows() %>% 
  mutate(yos = as.numeric(yos))

df_qxr_raw


#*******************************************************************************
#                      ## Importing disability retirement rates ####
#*******************************************************************************

# df_qxd_raw <- 
# left_join(
#   read_excel_range(filePath_dataRaw, "disbRet_misc")$df,
#   read_excel_range(filePath_dataRaw, "disbRet_sfty")$df 
# ) %>% 
#   mutate(age = as.numeric(age)) %>% 
#   arrange(grp, age)
# 
# df_qxd_raw 

# For BART
df_qxd_misc_raw <-  read_excel_range(filePath_dataRaw, "disbRet_misc")$df
df_qxd_sfty_raw <-  read_excel_range(filePath_dataRaw, "disbRet_sfty")$df 



#*******************************************************************************
#                      ## Importing termination rates ####
#*******************************************************************************

# row indices
#  - groups
#   - misc
#   - sfty
#  - yos: 1~5, 10~35 by 5
#  - ea: 20~45 by 5

df_qxt.refund_misc_raw <- 
  read_excel_range(filePath_dataRaw, "defrRet_refund_misc")$df %>%
  gather(ea, qxt.refund,-yos, convert = TRUE) %>%
  mutate(grp = "misc") %>%
  relocate(grp)


df_qxt.refund_sfty_raw <- 
  read_excel_range(filePath_dataRaw, "defrRet_refund_sfty")$df %>%
  mutate(grp = "sfty") %>%
  relocate(grp)



df_qxt.vest_misc_raw <- 
  read_excel_range(filePath_dataRaw, "defrRet_vest_misc")$df %>%
  gather(ea, qxt.vest,-yos, convert = TRUE) %>%
  mutate(grp = "misc") %>%
  relocate(grp)


df_qxt.vest_sfty_raw <- 
  read_excel_range(filePath_dataRaw, "defrRet_vest_sfty")$df %>%
  mutate(grp = "sfty") %>%
  relocate(grp)



#*******************************************************************************
#                      ## Importing pre-retirement mortality  ####
#*******************************************************************************

#' Row indices:
#'  - age: 20-80 by 5
#' Labels in col names:
#'  - pre: pre-retirement
#'  - occ/nonocc: occupational and non-occupational 
#'                (industrial and non-industrial related in CalPERS terms)
#'  - female/male


df_qxm.pre_raw <-
  left_join(
    read_excel_range(filePath_dataRaw, "mortality_preRet_nonInds")$df,
    read_excel_range(filePath_dataRaw, "mortality_preRet_inds")$df,
    by = "age"
  )



#*******************************************************************************
#                      ## Importing post-retirement mortality  ####
#*******************************************************************************


df_qxm.post_raw_proj <- 
  read_excel_range(filePath_dataRaw, "mortality_postRet_15yProj")$df

df_qxm.post_raw <- 
  read_excel_range(filePath_dataRaw, "mortality_postRet_noProj")$df





#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************


df_salScale.merit_raw <-
  
  bind_rows(
    read_excel_range(filePath_dataRaw, "salScale_merit_misc")$df %>%
      gather(ea, salScale.merit, -yos, convert = TRUE) %>%
      mutate(grp = "misc") %>%
      relocate(grp),
    
    read_excel_range(filePath_dataRaw, "salScale_merit_sfty")$df %>%
      gather(ea, salScale.merit, -yos, convert = TRUE) %>%
      mutate(grp = "sfty") %>%
      relocate(grp)
  )

df_salScale.merit_raw


#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************



df_qxr_raw
df_qxd_misc_raw
df_qxd_sfty_raw
df_qxt.refund_misc_raw
df_qxt.refund_sfty_raw
df_qxt.vest_misc_raw
df_qxt.vest_sfty_raw
df_qxm.pre_raw
df_qxm.post_raw
df_qxm.post_raw_proj

df_salScale.merit_raw


save(
  df_qxr_raw,
  df_qxd_misc_raw,
  df_qxd_sfty_raw,
  df_qxt.refund_misc_raw,
  df_qxt.refund_sfty_raw,
  df_qxt.vest_misc_raw,
  df_qxt.vest_sfty_raw,
  df_qxm.pre_raw,
  df_qxm.post_raw,
  df_qxm.post_raw_proj,
  
  df_salScale.merit_raw,
		 
	file = paste0(dir_dataOut, "Data_BART_decrements_ES2017_raw.RData")
)




