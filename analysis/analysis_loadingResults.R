
#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

# rm(list = ls())
invisible(gc())


df_simNames <- 
  read_excel(paste0(here::here(), "/model/RunControl.xlsx"), sheet = "SimsAnalysis") %>% 
	filter(!is.na(sim_name))

df_simNames_load <- 
	df_simNames %>% 
	filter(!is.na(sim_name), include)

df_results <- 
	map(df_simNames_load$sim_name, ~readRDS(paste0(here::here(), "/", dir_simResults , "sim_", .x, ".rds"))$results) %>% 
	bind_rows() 

# results_all$sim_name %>% unique



#*******************************************************************************
##           BART plans: combining misc and safety for each policy          ####
#*******************************************************************************

df_results_agg <-
	df_results %>%
	mutate(sim_name = str_remove(sim_name, "misc_|sfty_")) %>%
	group_by(sim_name, sim, year) %>%
	summarise(
		AL = sum(AL),
		MA = sum(MA),
		AA = sum(AA),
		UAAL = sum(UAAL),
		NC = sum(NC),
		SC = sum(SC),
		ERC = sum(ERC),
		EEC = sum(EEC),
		PR  = sum(PR),
		B   = sum(B),
		i.r = i.r[1],
		.groups = "drop"
	) %>%
	mutate(sim_name = paste0("bart_", sim_name),
				 FR_MA  = 100 * MA/AL,
				 ERC_PR = 100 * ERC/PR,
				 EEC_PR = 100 * EEC/PR,
				 NC_PR  = 100 * NC/PR,
				 SC_PR  = 100 * SC/PR,
				 NC.ER_PR = 100 * (NC - EEC)/PR,
				 NC.ER    =  NC - EEC
	)


df_results <-
	bind_rows(df_results,
						df_results_agg) %>%
	arrange(sim_name, sim, year) %>% 
	left_join(df_simNames, by = "sim_name") %>% 
  mutate(
  	sim_name = factor(sim_name, levels = df_simNames$sim_name)
  ) 















