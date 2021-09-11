## Imputation of decrement tables and salary scales for BART misc and safety


## Inputs
#   - inputs/data_raw/Data_BART_decrements_ES2017_raw.RData"


## Outputs
 #  -  tables imputed across age/ea/yos ranges, all in "tidy" format
 


##' Notes
#'  Data processing in this script must be "model agnostics"
#'  Data processing can take into account 
#'    - the structure of demographic inputs (e.g. age, yos ranges)
#'    - relevant CalPERS rules and valuation methods 


## Data frames to be processed
# df_qxr_raw,
# df_qxd_raw,
# df_qxt.refund_raw,
# df_qxt.vest_raw,
# df_qxm.pre_raw,
# df_qxm.post_raw,
# df_qxm.post_raw_proj,
# 
# df_salScale.merit_raw,



## Imputation rules ####

##' Steps to determine ranges of age, ea, and yos for active members
#' 1. Retirement age: based on benefit rules and retirement rate assumptions
#'    - Max: 70 (retirement rate assumptions)
#'    - Min: 50
#' 2. Max age: set to the max retirement age  (70)
#' 3. Max ea : set to (max retAge - 1)        (69)
#' 4. Max yos: (max age for actives - min ea) (70-20 = 50)

#' Note: In the model, members start receiving benefits at the retirement age. 
#'       The retirement rate for age t is applied at the end the of the year when 
#'       the age was t-1. 


#'  - Range of age, general:  20-100
#'  - Actives: 
#'     - Range of age:  20-70
#'     - Range of ea:   20-69 
#'     - Range of yos:  0-50
#'  - Retirees and beneficiaries:
#'     - Range of age,  20-100 (to take into account young beneficiaries and disability retirees)



#' Ranges of ea and yos should ALSO take into account the age-yos ranges of the data for active members
#'  - In active memeber data: max age is 69, max yos is 29
#'  - For the age group 69, the min yos is 0, implying the max ea in the data is 69.
#'  - All fall in the ranges determined by decrement assumptions and benefit rules 

#' If max age in active member data is greater than the max age determined above, 
#' then we should consider adjusting the member data(e.g. merged to lower age groups)







#*******************************************************************************
#                      ## Global settings ####
#*******************************************************************************

dir_inputs  <- "inputs/data_proc/"
fn_inputs   <- "Data_BART_decrements_ES2017_raw.RData" 
filePath_inputs <- paste0(dir_inputs, fn_inputs)

dir_outputs <- "Inputs/data_proc/"


load(filePath_inputs)



#*******************************************************************************
#                      ## Local tools ####
#*******************************************************************************

# ## spline smoothing 
# splong<-function(df,fillvar,fitrange=NULL, method = "natural"){
#   # df should have only 3 columns: fillvar, nonfillvar [in either order], and value
#   # or just 2 columns, with no nonfillvar
#   # last column ALWAYS must be the value var
#   valvar<-names(df)[length(names(df))]
#   nonfillvar<-setdiff(names(df),c(fillvar,valvar))
#   f<-function(x) {
#     if(is.null(fitrange)) fitrange<-min(x[,fillvar]):max(x[,fillvar])
#     spl<-spline(x[,fillvar], x[,valvar], xout=fitrange, method = method)
#     dfout<-data.frame(x=spl$x, y=spl$y)
#     names(dfout)<-c(fillvar,valvar)
#     return(dfout)
#   }
#   if(length(nonfillvar)>0) dfl2<-ddply(df,c(nonfillvar),f) else dfl2<-f(df)
#   return(dfl2)
# }


#*******************************************************************************
#                      ## Importing service retirement rates ####
#*******************************************************************************

# Indices:
#   - age: 50-75, by 1y, NO imputation needed  
#   - yos: 5-30,  by 5y, imputation needed

# Imputation:
#   - across yos 0-35 within each grp-age group
#   - negative values to 0
#   - yos should be extended to 50 to allow for age=70 with ea = 20, 
#     rates at yos=30 will be used for all yos>30 in each age group.  


df_qxr_imputed <- 
  df_qxr_raw %>% 
  unite("grp_age", grp, age, sep = "*") %>% 
  splong("yos", fitrange = 0:50) %>% 
  group_by(grp_age) %>% 
  mutate(qxr = ifelse(qxr<=0, 0, qxr),
         qxr = ifelse(yos > 30, qxr[yos == 30], qxr)
         ) %>% 
  separate(grp_age, into = c("grp", "age"), sep = "\\*", convert = TRUE) 



#*******************************************************************************
#                      ## Importing disability retirement rates ####
#*******************************************************************************

# Indices:
#   - age: 20-80, by 5y, imputation needed  

# Imputation:
#   - across age 20-80 within each grp
#   - all NA values are converted to 0

df_qxd_misc_imputed <- 
  left_join(
  df_qxd_misc_raw %>% 
    select(-qxd.nonocc_male) %>% 
    as.data.frame() %>% 
    splong("age"),
  
  df_qxd_misc_raw %>% 
    select(-qxd.nonocc_female) %>% 
    as.data.frame() %>% 
    splong("age"), # can avoid negative values, but requires monotone inputs
  
  Joining, by = c("age")
  )


df_qxd_sfty_imputed <- 
  left_join(
    df_qxd_sfty_raw %>% 
      select(-qxd.nonocc) %>% 
      as.data.frame() %>% 
      splong("age", method = "hyman"),
    
    df_qxd_sfty_raw %>% 
      select(-qxd.occ) %>% 
      as.data.frame() %>% 
      splong("age", method = "hyman"), # can avoid negative values, but requires monotone inputs
    
    Joining, by = c("age")
  )



## checking negative values
# (df_qxd_imputed$qxd.nonocc<0) %>% sum
# (df_qxd_imputed$qxd.occ<0) %>% sum
# 
# # plot the rates
# df_qxd_misc_imputed %>%
#   gather(var, value, -age) %>%
#   qplot(x = age, y = value, color = var, geom = "line", data = .)
# 
# df_qxd_sfty_imputed %>%
#   gather(var, value, -age) %>%
#   qplot(x = age, y = value, color = var, geom = "line", data = .)


#*******************************************************************************
#                      ## Importing termination rates, refund  ####
#*******************************************************************************

# row indices
#  - groups
#   - misc
#  - yos: 0~50 by 5y,  imputation needed
#  - ea: 20-45, by 5y, imputation needed

# Imputation: 
#  - convert NAs to 0 (yos = 50 in safety and POFF)
#  - first impute across yos from 0-50, using value at yos ==35 for yos > 35
#  - then impute  across ea from 20-69, using value at age= 45 for age > 45


# Misc across YOS
df_qxt.refund_misc_imputed <- 
  df_qxt.refund_misc_raw %>% 
    mutate(qxt.refund = na2zero(qxt.refund)) %>% 
    unite("grp_ea", grp, ea, sep = "+") %>% 
    splong("yos", fitrange = 0:50, method = "hyman") %>% 
    group_by(grp_ea) %>% 
    mutate(qxt.refund = ifelse(qxt.refund<0, 0, qxt.refund),
           qxt.refund = ifelse(yos > 35, qxt.refund[yos == 35], qxt.refund)
           )

# Misc across ea
df_qxt.refund_misc_imputed %<>% 
  ungroup() %>% 
  separate(grp_ea, c("grp", "ea"), sep = "\\+", convert = TRUE) %>% 
  unite("grp_yos", grp, yos, sep = "+") %>% 
  splong("ea", fitrange = 20:69) %>% # use hyman method wherever possible
  # filter(qxt.refund<0, ea <=45)
  group_by(grp_yos) %>% 
  mutate(qxt.refund = ifelse(qxt.refund<0, 0, qxt.refund),
         qxt.refund = ifelse(ea > 45, qxt.refund[ea == 45], qxt.refund)
  ) %>% 
  separate(grp_yos, c("grp", "yos"), sep = "\\+", convert = TRUE) %>% 
  arrange(grp, ea, yos)
  
  
# Safety across yos
df_qxt.refund_sfty_imputed <- 
  df_qxt.refund_sfty_raw %>% 
  mutate(qxt.refund = na2zero(qxt.refund)) %>% 
  splong("yos", fitrange = 0:50, method = "hyman") %>%
  mutate(qxt.refund = ifelse(qxt.refund<0, 0, qxt.refund),
         qxt.refund = ifelse(yos > 35, qxt.refund[yos == 35], qxt.refund)
  )



#*******************************************************************************
#                      ## Importing termination rates, vested  ####
#*******************************************************************************

# Misc
# Row indices
#  - groups
#   - misc
#  - yos: 0~35 by 5y,  imputation needed
#  - ea: 20~40 by 5y, imputation needed

# Imputation: 
#  - convert NAs to 0 
#  - first impute across yos from 5-50, using value at yos ==35 for yos > 35, 0 if yos < 5
#  - then impute  across ea from 20-69, using value at age=40 for age > 40

# df_qxt.vest_raw


# Misc across YOS
df_qxt.vest_misc_imputed <- 
  df_qxt.vest_misc_raw %>% 
  mutate(qxt.vest = na2zero(qxt.vest)) %>% 
  unite("grp_ea", grp, ea, sep = "+") %>% 
  splong("yos", fitrange = 5:50, method = "hyman") %>% 
  group_by(grp_ea) %>% 
  mutate(qxt.vest = ifelse(qxt.vest<0, 0, qxt.vest),
         qxt.vest = ifelse(yos > 35, qxt.vest[yos == 35], qxt.vest)
  )


# Misc across ea
df_qxt.vest_misc_imputed %<>% 
  ungroup() %>% 
  separate(grp_ea, c("grp", "ea"), sep = "\\+", convert = TRUE) %>% 
  unite("grp_yos", grp, yos, sep = "+") %>% 
  splong("ea", fitrange = 20:69) %>% # use hyman method wherever possible
  # filter(qxt.vest<0, ea <=45)
  group_by(grp_yos) %>% 
  mutate(qxt.vest = ifelse(qxt.vest<0, 0, qxt.vest),
         qxt.vest = ifelse(ea > 40, qxt.vest[ea == 40], qxt.vest)
  ) %>% 
  separate(grp_yos, c("grp", "yos"), sep = "\\+", convert = TRUE) %>% 
  arrange(grp, ea, yos)


# Safety across yos
df_qxt.vest_sfty_imputed <- 
  df_qxt.vest_sfty_raw %>% 
  mutate(qxt.vest = na2zero(qxt.vest)) %>% 
  splong("yos", fitrange = 5:50, method = "hyman") %>%
  mutate(qxt.vest = ifelse(qxt.vest<0, 0, qxt.vest),
         qxt.vest = ifelse(yos > 35, qxt.vest[yos == 35], qxt.vest)
  )






#*******************************************************************************
#                      ## Importing pre-retirement mortality  ####
#*******************************************************************************

#' Row indices:
#'  - age: 20-80 by 5, imputation needed
#' Labels in col names:
#'  - pre: pre-retirement
#'  - occ/nonocc: occupational and non-occupational 
#'                (industrial and non-industrial related in CalPERS terms)
#'  - female/male

#' Imputation
#'  - impute across age 20-80

df_qxm.pre_imputed <-  
  df_qxm.pre_raw %>% 
  gather(Var, value, -age) %>% 
  splong("age", method = "hyman") %>% # very little difference between natural and hyman 
  spread(Var, value)


# df_qxm.pre_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)


#*******************************************************************************
#                      ## Importing post-retirement mortality  ####
#*******************************************************************************

# Indices:
#   - age: 20-110, by 5y, imputation needed  


# Imputation:
#   - across yos 20-110 within each variable


df_qxm.post_imputed <- 
  df_qxm.post_raw %>% 
  gather(Var, value, -age) %>% 
  splong("age", method = "hyman") %>% # very little difference between natural and hyman 
  spread(Var, value)



df_qxm.post_proj_imputed <- 
  df_qxm.post_raw_proj %>% 
  gather(Var, value, -age) %>% 
  splong("age", method = "hyman") %>% # very little difference between natural and hyman 
  spread(Var, value)


# df_qxm.post_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)
# 
# df_qxm.post_proj_imputed %>%
#   gather(Var, value, -age) %>%
#   qplot(x = age, y = value, color = Var, geom = "line", data=.)



#*******************************************************************************
#                      ## Importing salary scales ####
#*******************************************************************************

# Row indices
#  - groups
#   - misc
#   - sfty

#  - yos: 0~5, 5~50 by y5,  imputation needed
#  - ea: (20, 30, 40), imputation needed

# Imputation: 
#  - convert NAs to 0 
#  - first impute across yos from 0-50, using value at yos ==30 for yos > 30
#  - then impute  across ea from 20-69, using value at age=40 for age > 40


# df_salScale.merit_raw

df_salScale.merit_imputed <- 
  bind_rows(
    df_salScale.merit_raw %>% 
      filter(grp %in% c("misc")) %>% 
      unite("grp_ea", grp, ea, sep = "+") %>% 
      splong("yos", fitrange = 0:50, method = "hyman") %>% 
      group_by(grp_ea) %>% 
      mutate(salScale.merit = ifelse(salScale.merit<0, 0, salScale.merit),
             salScale.merit = ifelse(yos > 30, salScale.merit[yos == 30], salScale.merit)
      ), 
    
    df_salScale.merit_raw %>% 
      filter(grp %in% c("sfty")) %>% 
      unite("grp_ea", grp, ea, sep = "+") %>% 
      splong("yos", fitrange = 0:50) %>% 
      group_by(grp_ea) %>% 
      mutate(salScale.merit = ifelse(salScale.merit<0, 0, salScale.merit),
             salScale.merit = ifelse(yos > 30, salScale.merit[yos == 30], salScale.merit)
            )
)
  
# df_salScale.merit_imputed

df_salScale.merit_imputed %<>% 
  ungroup() %>% 
  separate(grp_ea, c("grp", "ea"), sep = "\\+", convert = TRUE) %>% 
  unite("grp_yos", grp, yos, sep = "+") %>% 
  splong("ea", fitrange = 20:69, method = "hyman") %>% # use hyman method wherever possible
  # filter(salScale.merit<0, ea <=45)
  group_by(grp_yos) %>% 
  mutate(salScale.merit = ifelse(salScale.merit<0, 0, salScale.merit),
         salScale.merit = ifelse(ea > 40, salScale.merit[ea == 40], salScale.merit)
  ) %>% 
  separate(grp_yos, c("grp", "yos"), sep = "\\+", convert = TRUE) %>% 
  arrange(grp, ea, yos)



#*********************************************************************************************************
#                      ## Review and save the results ####
#*********************************************************************************************************

df_qxr_imputed
df_qxd_misc_imputed
df_qxd_sfty_imputed

df_qxt.refund_misc_imputed
df_qxt.vest_misc_imputed
df_qxt.refund_sfty_imputed
df_qxt.vest_sfty_imputed

df_qxm.pre_imputed
df_qxm.post_imputed
df_qxm.post_proj_imputed

df_salScale.merit_imputed




save(
  df_qxr_imputed,
  df_qxd_misc_imputed,
  df_qxd_sfty_imputed,
  
  df_qxt.refund_misc_imputed,
  df_qxt.vest_misc_imputed,
  df_qxt.refund_sfty_imputed,
  df_qxt.vest_sfty_imputed,
  
  df_qxm.pre_imputed,
  df_qxm.post_imputed,
  df_qxm.post_proj_imputed,
  
  df_salScale.merit_imputed,

	file = paste0(dir_outputs, "Data_BART_decrements_ES2017_imputed.RData")
)




